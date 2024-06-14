unit u_audioutils;

{$mode ObjFPC}{$H+}
 {$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, ctypes,
  ALSound, libsndfile;

type

  { TBaseAudioFile }

  TBaseAudioFile = object
    Handle: PSNDFILE;
    Format,
    Channels: integer;
    Frames: int64;
    SampleRate: integer;
    // return a frame index in range [0..Frames-1]
    function TimeToFrameIndex(aTimePos: double): int64;
    // return a duration in seconds from a number of frames
    function FrameToTime(aFrameCount: int64): double;
  private // debug purpose
    FileName: string;
    procedure LogLibSndFileErrorMessage(const aMess: string);
    function GetLibSndFileErrMess: string;
  end;

  { TAudioFileReader }

  TAudioFileReader = object(TBaseAudioFile)
    function OpenRead(const aFileName: string; aPostLogMessage: boolean=True): boolean;
    function ReadShort(p: Pointer; aCount: longword): longword;
    function ReadFloat(p: Pointer; aCount: longword): longword;
    function ReadDouble(p: PDouble; aCount: longword): longword;
    function Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
    function Close: boolean;
    function MoveToFrame(aFrameIndex: int64): boolean;
    function TotalDuration: double;
  end;

  { TAudioFileWriter }

  TAudioFileWriter = object(TBaseAudioFile)
  private
    FCopyBuffer: TALSPlaybackBuffer; // used by copy routines
    procedure InitCopyBuffer(aFramesCapacity: longword);
  public
    function OpenWrite(const aFileName: string;
                       aFormat: TALSFileFormat;
                       aChannels,
                       aSampleRate: integer;
                       aPostLogMessage: boolean=True): boolean;
    function SampleAreFloat: boolean;
    function Close: boolean;
    function WriteShort(p: Pointer; aCount: longword): longword;
    function WriteFloat(p: Pointer; aCount: longword): longword;
    function WriteDouble(p: PDouble; aCount: longword): longword;
    function Write(const aBuf: TALSPlaybackBuffer): longword;
  public
    // copy whole audio
    function CopyAll(const aSrcReader: TAudioFileReader): boolean;
    // copy part of audio by frames
    function CopyPart(const aSrcReader: TAudioFileReader; aFromFrameIndex, aToFrameIndex: int64): boolean;

    procedure CopyMetaDataFrom(const aSrcReader: TAudioFileReader);
    procedure WriteMetaData(const aTitle, aCopyright, aSoftware, aArtist,
                            aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
  end;


  function GetAudioFileDuration(const aFilename: string): single;
  function GetAudioFileFramesCount(const aFilename: string): int64;

const
  PEAK_TIME_SLICE = 0.01;
  ONE_DIV_PEAK_TIME_SLICE = 100;
type


  TSamplePeak = record
    Positive, Negative: Smallint;
  end;

  { TAudioFileLevel }

  TAudioFileLevel = record
  private
    FDuration: single;
    FSampleRate, FFrameCountInSlice: integer;
    FPeaks: array of TSamplePeak;
    function ComputePeakFromAudioBuffer(const aBuf: TALSPlaybackBuffer): TSamplePeak;
    function GetPeakCount: int64;
    function LoadFromPeakFile(const aPKFilename: string): boolean;
    function TimeToIndex(aTime: single): integer;
  public
    procedure InitDefault;
    // try to load the .pk file, if it doesn't exists the function construct and save it.
    function LoadFromFile(const aAudioFilename: string): boolean;

    function GetPeakBetween(aTimeBegin, aTimeEnd: single): TSamplePeak;

    property Count: int64 read GetPeakCount;
    property Duration: single read FDuration;
    property SampleRate: integer read FSampleRate;
  end;
  PAudioFileLevel = ^TAudioFileLevel;

implementation

uses Math, u_logfile, u_common, DateUtils;

//const
//  DECIBEL_MAX_ABSOLUTE_VALUE = -ALS_DECIBEL_MIN_VALUE;

procedure ExceptionChannelsCount;
begin
  Raise Exception.Create('Audio buffer can not have different channel count than the file');
end;

function GetAudioFileDuration(const aFilename: string): single;
var reader: TAudioFileReader;
begin
  Result := 0;
  if reader.OpenRead(aFilename) then begin
    Result := reader.TotalDuration;
    reader.Close;
  end;
end;

function GetAudioFileFramesCount(const aFilename: string): int64;
var reader: TAudioFileReader;
begin
  Result := 0;
  if reader.OpenRead(aFilename) then begin
    Result := reader.Frames;
    reader.Close;
  end;
end;

{ TAudioFileLevel }

function TAudioFileLevel.ComputePeakFromAudioBuffer(const aBuf: TALSPlaybackBuffer): TSamplePeak;
var i, j: integer;
  p: PSmallInt;
begin
  Result.Positive := 0;
  Result.Negative := 0;

  p := PSmallInt(aBuf.Data);
  for i:=0 to aBuf.FrameCount-1 do
    for j:=0 to aBuf.ChannelCount-1 do begin
      if Result.Positive < p^ then Result.Positive := p^;
      if Result.Negative > p^ then Result.Negative := p^;
      inc(p);
    end;

{  aBuf.ComputeChannelsLevel;

  Result := ALS_DECIBEL_MIN_VALUE;
  for i:=0 to aBuf.ChannelCount-1 do
    if Result < aBuf.ChannelsLeveldB[i] then
      Result := aBuf.ChannelsLeveldB[i];

  Result := (Result+DECIBEL_MAX_ABSOLUTE_VALUE)/DECIBEL_MAX_ABSOLUTE_VALUE;  }
end;

function TAudioFileLevel.GetPeakCount: int64;
begin
  Result := Length(FPeaks);
end;

function TAudioFileLevel.LoadFromPeakFile(const aPKFilename: string): boolean;
var stream: TFileStream;
  peakCount, byteCount: integer;
begin
  Result := False;
  FPeaks := NIL;
  stream := TFileStream.Create(aPKFilename, fmOpenRead);
  try
    try
      peakCount := Ceil(stream.Size / SizeOf(TSamplePeak));
      SetLength(FPeaks, peakCount);
      byteCount := peakCount * SizeOf(TSamplePeak);
      stream.Read(FPeaks[0], byteCount);
      Result := True;
    except
      Result := False;
    end;
  finally
    stream.Free;
  end;
end;

function TAudioFileLevel.TimeToIndex(aTime: single): integer;
begin
  Result := Trunc(aTime * ONE_DIV_PEAK_TIME_SLICE);
end;

procedure TAudioFileLevel.InitDefault;
begin
  FillChar(Self, SizeOf(TAudioFileLevel), 0);
end;

function TAudioFileLevel.LoadFromFile(const aAudioFilename: string): boolean;
var reader: TAudioFileReader;
  buf: TALSPlaybackBuffer;
  i, peakCount: integer;
  f: string;
  stream: TFileStream;
  audioDate, peakDate: TDateTime;
begin
  InitDefault;
  Result := False;

  // open file
  if not reader.OpenRead(aAudioFilename) then exit(False);
  FSampleRate := reader.SampleRate;
  FDuration := reader.TotalDuration;
  FFrameCountInSlice := reader.TimeToFrameIndex(PEAK_TIME_SLICE);

  // first try to retrieve the existant .pk file
  f := ChangeFileExt(aAudioFilename, PEAK_AUDIO_FILE_EXTENSION);
  if FileExists(f) then begin
    // now we compare the file peak age to the audio file age.
    // If the former is oldest than the original audio file, we need to reconstruct.
    if FileAge(aAudioFilename, audioDate) and
       FileAge(f, peakDate) and
       (CompareDateTime(audioDate, peakDate) < 0) then begin
      Result := LoadFromPeakFile(f);
      if Result then begin
        reader.Close;
        exit;
      end;
    end;
  end;

  // peak file is not found or fail to retrieve its age, we construct it
  try
{    // open file
    if not reader.OpenRead(aAudioFilename) then exit(False);

    FSampleRate := reader.SampleRate;
    FDuration := reader.TotalDuration;
    FFrameCountInSlice := reader.TimeToFrameIndex(PEAK_TIME_SLICE);   }

    // adjust the size of Levels array
    FPeaks := NIL;
    peakCount := Ceil(reader.Frames / FFrameCountInSlice);
    SetLength(FPeaks, peakCount);

    buf.Init(reader.Channels, ALS_SAMPLE_INT16);
    buf.FrameCapacity := FFrameCountInSlice;
    if buf.OutOfMemory then begin
      reader.Close;
      exit(False);
    end;

    i := 0;
    while reader.Read(buf, buf.FrameCapacity) > 0 do begin
      FPeaks[i] := ComputePeakFromAudioBuffer(buf);
      inc(i);
    end;

    reader.Close;
    buf.FreeMemory;
    Result := True;

    // try to save in the peak levels in a peak file
    if Length(FPeaks) > 0 then begin
      stream := TFileStream.Create(f, fmCreate);
      try
        stream.Write(FPeaks[0], Length(FPeaks)*SizeOf(TSamplePeak));
      finally
        stream.Free;
      end;
    end;
  except
{    on E: Exception do begin
      Log.Error('TFrameViewAudio.ReloadPartFromFrameToEnd('+aFrameIndex.ToString+')'+LineEnding+
                '    raise exception: '+E.Message+LineEnding+
                '    on file "'+Filename);
    end; }
  end;

end;

function TAudioFileLevel.GetPeakBetween(aTimeBegin, aTimeEnd: single): TSamplePeak;
var indexBegin, indexEnd, i: Integer;
  positive, negative: int64;
begin
  if aTimeBegin < 0 then aTimeBegin := 0;
  aTimeBegin := EnsureRange(aTimeBegin, 0, Duration);
  aTimeEnd := EnsureRange(aTimeEnd, aTimeBegin, Duration);

  indexBegin := TimeToIndex(aTimeBegin);
  indexEnd := TimeToIndex(aTimeEnd);
  positive := 0;
  negative := 0;
  for i:=indexBegin to indexEnd do begin
    positive := positive + FPeaks[i].Positive;
    negative := negative + FPeaks[i].negative;
  end;

  i := indexEnd - indexBegin + 1;
  Result.Positive := Round(positive / i);
  Result.Negative := Round(negative / i);
end;

{ TBaseAudioFile }

function TBaseAudioFile.TimeToFrameIndex(aTimePos: double): int64;
begin
  Result := Round(SampleRate*aTimePos);
end;

function TBaseAudioFile.FrameToTime(aFrameCount: int64): double;
begin
  Result := aFrameCount/SampleRate;
end;

procedure TBaseAudioFile.LogLibSndFileErrorMessage(const aMess: string);
begin
  Log.Error(aMess);
  Log.Error('on file "'+FileName+'"', 1);
  Log.Error(GetLibSndFileErrMess);
  Log.AddEmptyLine;
end;

function TBaseAudioFile.GetLibSndFileErrMess: string;
var i: integer;
  errmsg: string;
begin
  if Handle = NIL then begin
    Result := 'Handle = NIL';
    exit;
  end;

  Result := '';

  errmsg := '';
  SetLength(errmsg, 2048);
  sf_command(Handle, SFC_GET_LOG_INFO, @errmsg[1], Length(errmsg));

  i := 1;
  while (errmsg[i] <> #0) and (i < Length(errmsg)) do
    inc(i);
  Result := Copy(errmsg, 1, i);
end;

{ TAudioFileWriter }

procedure TAudioFileWriter.InitCopyBuffer(aFramesCapacity: longword);
begin
  if SampleAreFloat
    then FCopyBuffer.Init(Channels, ALS_SAMPLE_FLOAT32)
    else FCopyBuffer.Init(Channels, ALS_SAMPLE_INT16);
  FCopyBuffer.FrameCapacity := aFramesCapacity;

  if FCopyBuffer.OutOfMemory then begin
    Log.Error('gyv: TAudioFileWriter.InitCopyBuffer out of memory when setting FrameCapacity to '+
              aFramesCapacity.ToString);
    Log.Error('on file "'+FileName+'"', 1);
    Log.AddEmptyLine;
  end;
end;

function TAudioFileWriter.OpenWrite(const aFileName: string;
  aFormat: TALSFileFormat; aChannels, aSampleRate: integer;
  aPostLogMessage: boolean): boolean;
var sfinfo: TSF_INFO;
begin
  Format := aFormat;
  Channels := aChannels;
  SampleRate := aSampleRate;

  sfinfo.Format := cint(aFormat);
  sfinfo.Channels := cint(aChannels);
  sfinfo.SampleRate := cint(aSampleRate);
  Handle := ALSOpenAudioFile(aFileName, SFM_WRITE, sfinfo);
  Result := Handle <> NIL;

  FileName := aFileName;

  if not Result and aPostLogMessage then begin
    Log.Error('gyv: TAudioFileWriter.OpenWrite fail');
    Log.Error('on file "'+FileName+'"', 1);
  end;
end;

function TAudioFileWriter.SampleAreFloat: boolean;
begin
  Result := (Format AND SF_FORMAT_SUBMASK) = SF_FORMAT_FLOAT;
end;

function TAudioFileWriter.Close: boolean;
begin
  Result := False;
  if Handle <> NIL then begin
    sf_write_sync(Handle);
    Result := sf_close(Handle) = 0;
  end;
  Handle := NIL;

  if not Result then
    LogLibSndFileErrorMessage('TAudioFileWriter.Close fail');
end;

function TAudioFileWriter.WriteShort(p: Pointer; aCount: longword): longword;
begin
  if Handle <> NIL then begin
    Result := sf_writef_short(Handle, pcshort(p), aCount);

    if Result <> aCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.WriteShort only write '+
                                Result.ToString+'/'+aCount.ToString+' frames');
  end else Result := 0;
end;

function TAudioFileWriter.WriteFloat(p: Pointer; aCount: longword): longword;
begin
  if Handle <> NIL then begin
    Result := sf_writef_float(Handle, pcfloat(p), aCount);

    if Result <> aCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.WriteFloat only write '+
                                Result.ToString+'/'+aCount.ToString+' frames');
  end else Result := 0;
end;

function TAudioFileWriter.WriteDouble(p: PDouble; aCount: longword): longword;
begin
  if Handle <> NIL then begin
    Result := sf_writef_double(Handle, pcdouble(p), aCount);

    if Result <> aCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.WriteDouble only write '+
                                Result.ToString+'/'+aCount.ToString+' frames');
  end else Result := 0;
end;

function TAudioFileWriter.Write(const aBuf: TALSPlaybackBuffer): longword;
begin
  Result := 0;

  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if (Handle = NIL) or (aBuf.FrameCount = 0) or (aBuf.OutOfMemory) then
    Result := 0
  else begin
    if aBuf.UseFloat then
      Result := sf_writef_float(Handle, pcfloat(aBuf.Data), aBuf.FrameCount)
    else
      Result := sf_writef_short(Handle, pcshort(aBuf.Data), aBuf.FrameCount);

    if Result <> aBuf.FrameCount then
      LogLibSndFileErrorMessage('TAudioFileWriter.Write only write '+
                                Result.ToString+'/'+aBuf.FrameCount.ToString+' frames');
  end;
end;

function TAudioFileWriter.CopyAll(const aSrcReader: TAudioFileReader): boolean;
var frameToRead: longword;
begin
  frameToRead := aSrcReader.Frames;
  if frameToRead = 0 then begin
    Result := True;
    exit;
  end;

  Result := aSrcReader.MoveToFrame(0);
  if not Result then begin
    Log.Error('gyv: TAudioFileWriter.CopyAll failed >> aSrcReader.MoveToFrame(0) failed');
    exit;
  end;

  InitCopyBuffer(Min(32768, frameToRead));
  if FCopyBuffer.OutOfMemory then begin
    Log.Error('gyv: TAudioFileWriter.CopyAll failed >> FCopyBuffer state is out of memory');
    Result := False;
    exit;
  end;

  while (frameToRead > 0) and Result do begin
    aSrcReader.Read(FCopyBuffer, Min(FCopyBuffer.FrameCapacity, frameToRead));
    Result := Result and (Write(FCopyBuffer) = FCopyBuffer.FrameCount);
    frameToRead := frameToRead - FCopyBuffer.FrameCount;
  end;
  Result := Result and (frameToRead = 0);

  FCopyBuffer.FreeMemory;
end;

function TAudioFileWriter.CopyPart(const aSrcReader: TAudioFileReader;
  aFromFrameIndex, aToFrameIndex: int64): boolean;
var frameToRead: integer;
begin
  if aSrcReader.Frames = 0 then begin
    Result := True;
    exit;
  end;

  Result := False;
  if (aFromFrameIndex < 0) or
     (aToFrameIndex < 0) or
     (aToFrameIndex < aFromFrameIndex) then exit;

  if (aFromFrameIndex <= 0) and (aToFrameIndex >= aSrcReader.Frames) then begin
    Result := CopyAll(aSrcReader);
    exit;
  end;

  Result := aSrcReader.MoveToFrame(aFromFrameIndex);
  if not Result then begin
    Log.Error('gyv: TAudioFileWriter.CopyPart failed >> aSrcReader.MoveToFrame'+
              aFromFrameIndex.ToString+') failed');
    exit;
  end;

  frameToRead := aToFrameIndex-aFromFrameIndex+1;

  if frameToRead > 0 then begin
    InitCopyBuffer(Min(32768, frameToRead));
    if FCopyBuffer.OutOfMemory then begin
      Log.Error('gyv: TAudioFileWriter.CopyPart failed >> FCopyBuffer state is out of memory');
      Result := False;
      exit;
    end;

    FCopyBuffer.FrameCount := 1;
    while (frameToRead > 0) and (FCopyBuffer.FrameCount > 0) and Result do begin
      aSrcReader.Read(FCopyBuffer, Min(FCopyBuffer.FrameCapacity, frameToRead));
      Result := Result and (Write(FCopyBuffer) = FCopyBuffer.FrameCount);
      frameToRead := frameToRead - FCopyBuffer.FrameCount;
    end;
    Result := Result and (frameToRead = 0);

    FCopyBuffer.FreeMemory;
  end;
end;

procedure TAudioFileWriter.CopyMetaDataFrom(const aSrcReader: TAudioFileReader);
  procedure DoCopyStr(aStrType: cint);
  var s: PChar;
  begin
    s := sf_get_string(aSrcReader.Handle, aStrType);
    if s <> NIL then sf_set_string(Handle, aStrType, s);
  end;
begin
  DoCopyStr(SF_STR_TITLE);
  DoCopyStr(SF_STR_COPYRIGHT);
  DoCopyStr(SF_STR_SOFTWARE);
  DoCopyStr(SF_STR_ARTIST);
  DoCopyStr(SF_STR_COMMENT);
  DoCopyStr(SF_STR_DATE);
  DoCopyStr(SF_STR_ALBUM);
  DoCopyStr(SF_STR_LICENSE);
  DoCopyStr(SF_STR_TRACKNUMBER);
  DoCopyStr(SF_STR_GENRE);
end;

procedure TAudioFileWriter.WriteMetaData(const aTitle, aCopyright, aSoftware,
  aArtist, aComment, aDate, aAlbum, aLicense, aTrackNumber, aGenre: string);
  procedure DoWriteStr(aStrType: cint; const s: string);
  begin
    if s <> '' then sf_set_string(Handle, aStrType, PChar(s));
  end;
begin
  DoWriteStr(SF_STR_TITLE, aTitle);
  DoWriteStr(SF_STR_COPYRIGHT, aCopyright);
  DoWriteStr(SF_STR_SOFTWARE, aSoftware);
  DoWriteStr(SF_STR_ARTIST, aArtist);
  DoWriteStr(SF_STR_COMMENT, aComment);
  DoWriteStr(SF_STR_DATE, aDate);
  DoWriteStr(SF_STR_ALBUM, aAlbum);
  DoWriteStr(SF_STR_LICENSE, aLicense);
  DoWriteStr(SF_STR_TRACKNUMBER, aTrackNumber);
  DoWriteStr(SF_STR_GENRE, aGenre);
end;

{ TAudioFileReader }

function TAudioFileReader.OpenRead(const aFileName: string; aPostLogMessage: boolean): boolean;
var sfinfo: TSF_INFO;
begin
  sfinfo.Format := 0;
  Handle := ALSOpenAudioFile(aFileName, SFM_READ, sfinfo);

  Result := Handle <> NIL;
  if Result then begin
    Format := sfinfo.Format;
    Channels := sfinfo.Channels;
    Frames := sfinfo.Frames;
    SampleRate := sfinfo.SampleRate;
  end;

  FileName := aFileName;

  if not Result and aPostLogMessage then begin
    Log.Error('gyv: TAudioFileReader.OpenRead fail');
    Log.Error('on file "'+FileName+'"', 1);
  end;
end;

function TAudioFileReader.Close: boolean;
begin
  Result := True;
  if Handle <> NIL then
    Result := sf_close(Handle) = 0;
  Handle := NIL;

  if not Result then
    LogLibSndFileErrorMessage('TAudioFileReader.Close fail');
end;

function TAudioFileReader.MoveToFrame(aFrameIndex: int64): boolean;
begin
  Result := sf_seek(Handle, aFrameIndex, SF_SEEK_SET) <> -1;

  if not Result then
    LogLibSndFileErrorMessage('TAudioFileReader.MoveToFrame fail to move to frame '+
              aFrameIndex.ToString+'/'+Frames.ToString);
end;

function TAudioFileReader.TotalDuration: double;
begin
  if SampleRate <> 0 then
    Result := Frames/SampleRate
  else begin
    Result := 0;
    LogLibSndFileErrorMessage('TAudioFileReader.TotalDuration fail because SampleRate=0');
  end;
end;

function TAudioFileReader.ReadShort(p: Pointer; aCount: longword): longword;
begin
  Result := sf_readf_short(Handle, pcshort(p), sf_count_t(aCount));
end;

function TAudioFileReader.ReadFloat(p: Pointer; aCount: longword): longword;
begin
  Result := sf_readf_float(Handle, pcfloat(p), sf_count_t(aCount));
end;

function TAudioFileReader.ReadDouble(p: PDouble; aCount: longword): longword;
begin
  Result := sf_readf_double(Handle, pcdouble(p), sf_count_t(aCount));
end;

function TAudioFileReader.Read(const aBuf: TALSPlaybackBuffer; aCount: longword): longword;
begin
  if aBuf.ChannelCount <> Channels then
    ExceptionChannelsCount;

  if aBuf.OutOfMemory then begin
    Result := 0;
    exit;
  end;

  if aBuf.UseFloat then
    Result := sf_readf_float(Handle, pcfloat(aBuf.Data), sf_count_t(aCount))
  else
    Result := sf_readf_short(Handle, pcshort(aBuf.Data), sf_count_t(aCount));
  aBuf.FrameCount := Result;
end;


end.


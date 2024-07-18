#!/bin/bash

create_no_install_archive(){
  echo "making ${NO_INSTALL_ARCHIVE_NAME}..."
  # delete the old archive
  if [ -f "${NO_INSTALL_ARCHIVE_NAME}" ]; then
    rm "${NO_INSTALL_ARCHIVE_NAME}"
  fi
 
  # delete and recreate the staging directory
  rm -rf "${STAGING_DIR}"
  mkdir "${STAGING_DIR}"

  # copy the source binary to lower case name in the staging directory
  cp -p ${PROJECT_EXECUTABLE} "${STAGING_EXECUTABLE}"
  # set executable mode
#  chmod 0755 "${STAGING_EXECUTABLE}"
  
  #create the version file
  echo ${VERSION} > "${STAGING_DIR}/version"
  
  # copy the license file
  cp "${PROJECT_DIR}/LICENSE" "${STAGING_DIR}/"
  
  # copy readme file
  cp readme "${STAGING_DIR}/readme"
  
  #copy the icon file
  cp "${PROJECT_DIR}/Design/logo/logo128.png" "${STAGING_DIR}/saynetes.png"
  
  #copy the .desktop file
  cp "saynetes.desktop" "${STAGING_DIR}/saynetes.desktop"

  # copy the right library directory
  cp -r "${PROJECT_BINARY_DIR}/${LIB_DIR}" "${STAGING_DIR}/${LIB_DIR}"

  # copy directory languages
  cp -r "${PROJECT_BINARY_DIR}/languages" "${STAGING_DIR}/languages"
  # rename languages files in lowercase
  mv "${STAGING_DIR}/languages/Saynetes.fr.po" "${STAGING_DIR}/languages/saynetes.fr.po"
  mv "${STAGING_DIR}/languages/Saynetes.po" "${STAGING_DIR}/languages/saynetes.po"
  mv "${STAGING_DIR}/languages/Saynetes.pot" "${STAGING_DIR}/languages/saynetes.pot"
  
  # copy directory Data
  cp -r "${PROJECT_BINARY_DIR}/Data" "${STAGING_DIR}/Data"

  # copy dmx library
  cp -r "${PROJECT_BINARY_DIR}/DMXLibrary" "${STAGING_DIR}/DMXLibrary"

  # copy demo folder
  cp -r "${PROJECT_DIR}/Demo" "${STAGING_DIR}/Demo"

  # compress the temporary directory
  pushd ${STAGING_DIR}
  tar -czf "../../${NO_INSTALL_ARCHIVE_NAME}" *
  popd

  # delete the staging directory
  rm -rf "${STAGING_DIR}"

  echo "ARCHIVE GENERATED"
}

# begin

VERSION=$(cat "../../version.txt")

TARGET_ARCHITECTURE="$(dpkg --print-architecture)"
if [ ${TARGET_ARCHITECTURE} = "amd64" ]; then
  OS_NAME="linux64"
  WIDGETSET="gtk2"
  TARGET_CPU="x86_64"
  LIB_DIR="x86_64-linux"
# elif [ ${TARGET_ARCHITECTURE} = "i386" ]; then
#   OS_NAME="linux32"
#   WIDGETSET="gtk2"
#  TARGET_CPU="i386"
#  LIB_DIR="i386-linux"
else
  echo "${TARGET_ARCHITECTURE} not supported"
  exit 1
fi

echo "${LIB_DIR} detected"

PROJECT_DIR="/media/sf_Pascal/Saynetes"
PROJECT_BINARY_DIR="${PROJECT_DIR}/Binary"
PROJECT_EXECUTABLE="${PROJECT_BINARY_DIR}/saynetes"
LAZARUS_PROJECT="${PROJECT_DIR}/Saynetes.lpi"
STAGING_DIR=./staging
STAGING_EXECUTABLE="${STAGING_DIR}/saynetes"
NO_INSTALL_ARCHIVE_NAME="saynetes_${VERSION}_${OS_NAME}_${WIDGETSET}_no_install.tar.gz"
LAZBUILD_DIR="/home/lulu/fpcupdeluxe/lazarus"

# delete the old project binary file
if [ -f "${PROJECT_EXECUTABLE}" ]; then
  rm "${PROJECT_EXECUTABLE}"
fi

# before compiling the project, we put the fake libdl.so in the project directory
# to force the dependency to glibc 2.2.5
# see https://forum.lazarus.freepascal.org/index.php/topic,58888.msg483544.html?PHPSESSID=f8vtvusgfek6e18kanbaosokk0#msg483544
# cp -p libdl.so ../../libdl.so

# compile project
echo "compiling Lazarus project ${VERSION}..."
# going to the directory where is lazbuild
pushd "${LAZBUILD_DIR}"
# compile and redirect output to /dev/null because we don't want to see the huge amount of message
# only error message are displayed on console
./lazbuild --build-all --quiet --widgetset=${WIDGETSET} --cpu=${TARGET_CPU} --build-mode=Linux64_Release \
           --no-write-project ${LAZARUS_PROJECT} 1> /dev/null
popd

# delete libdl.so from the project directory
# rm ../../libdl.so

# check if binary file was created
if [ ! -f "${PROJECT_EXECUTABLE}" ]; then
  echo "COMPILATION FAILED..."
  exit 1
fi
           
echo "Success"

create_no_install_archive

# delete executable
rm "${PROJECT_EXECUTABLE}"

read -p "Press enter to exit"



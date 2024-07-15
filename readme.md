# Saynètes
![icon](https://github.com/Lulu04/Saynetes/blob/0a5e72501cb0eba0e7e990f5597bd38f26193196/Design/logo/logo96.png)
Sound and lighting control software. Free and open source, written in FreePascal with Lazarus IDE.  
License: GPLv3  
Author: Lulu  

# Supported platforms
Windows 64b (tested on Win10)  
Linux 64b - GTK2 - with oversized font height and some wrong colors (tested on Ubuntu 22.04 LTS)  

# Supported DMX Device
Velleman K8062  
Enttec USB DMX PRO  

# Screenshots
The main window
![The main window](https://github.com/Lulu04/Saynetes/blob/f5aacba1341b4c211a499a8bf532310f63696854/Screenshots/MainScreen.png)
  
Create sequences and build your show
![The sequencer](https://github.com/Lulu04/Saynetes/blob/f5aacba1341b4c211a499a8bf532310f63696854/Screenshots/Sequencer.png)

# How to compile
You must have Lazarus IDE installed on your system with BGRABitmap package.  
- create a directory named saynetes on your disk
- in 'saynetes' directory clone this repository. You can also download the zip.
- again in 'saynetes' directory, clone the repository [UnitsCommon](https://github.com/Lulu04/UnitsCommon). If you choose to download the zip, unzip the file and rename the obtained directory to "UnitsCommon".
- always in the 'saynetes' directory, clone the repository [ALSound library](https://github.com/Lulu04/ALSound). If you choose to download the zip, unzip the file and rename the obtained directory to "ALSound".

The directory structure should be like:  
|- saynetes  
	|- ALSound  
	|- Saynetes (clone version) or Saynetes-main (unziped version)  
	|- UnitsCommon  
  
Now, you are able to open the Lazarus project in the directory Saynetes or Saynetes-main.  

# History
The SAYNÈTES story began around 2011. It was born out of the technical needs felt when setting up amateur live shows.
At first, the software only managed audio, then the possibility of renting DMX lighting equipment at low cost presented itself, and it evolved to support this protocol, at first in a rudimentary way. Needs guided its evolution.
This version of Saynètes has been completely rewritten by the author, who is passionate about programming.

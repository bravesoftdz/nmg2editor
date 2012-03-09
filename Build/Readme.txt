

*************************************************************************************
* Important - Important - Important - Important - Important - Important - Important *
*                                                                                   *
*  When you want to use USB functionality you have to install the generic           *
*  USB driver Libusb-win32 as A >>FILTER<< driver on the original clavia usb driver.* 
*  You should use the Filter wizard for this.                                       *
*                                                                                   *
*                                                                                   *
*  Instructions on how to download and install the driver you can find here:        *
*       http://sourceforge.net/p/nmg2editor/wiki/Installing%20LibUSB/               *
*                                                                                   *
*                                                                                   *
* When you install the libusb-win32 as a device driver (inf-wizard) in stead of a   *
* filter driver (Filter- wizard), the original clavia editor will not work anymore! *
*                                                                                   *
*************************************************************************************


Installation:

Editor:
-------

1) When you want to use USB functionality, you should install the generic libusb-win32 driver, see the above message for this. From version v0.21 USB functionality is optional.

2) The zip file contains a directory structure. You can unpack this zip somewhere on you disk, the editor application should run from there.

3) It is recomended to delete any previous versions of the editor and associated files.

VST:
----

1) You should delete the old versions of the following files (if they exist) in you VST folder:

   G2_vst_D2011.dll
   ModuleDef.xml
   ParamDef.xml

2) then you can copy the new versions of these files to your VST folder.



v0.21 9-3-2012
==============

- The libusb dll is now loaded statically in stead of dynamically. That means that you can run the editor weather or not the libusb-win32 is installed on your system. You might not want to install the libusb, maybe because you want to use midi only functions, or if you want to run the editor as a client wirelessly connected to another editor that maintains the usb connection. When you start the editor on a system without the libusb-win32, you get a warning: "libusb0.dll not found, USB functions are disabled".

- Added the functionality so you can run the editor as a client to another instance of the editor. For example, you could run the editor on a laptop that is has a wireless tcp-ip connection with another editor on a desktop pc that has the usb connection with the G2. The settings for this are on the "Application settings" form.

- Added midi functionality to the editor, for uploading/downloading patches and performances using sysex. I've used the open sources midi library "https://bitbucket.org/h4ndy/midiio-dev" for this.

- Added the buttons associated to knobs on the parameter pages and on the VST. Which parameter is associated with a button when you assign a parameter to a knob on the parameter pages is hard coded in the G2. So I've added this information to the "ModuleDef.xml" (But I've not managed to do all modules yet).

- Added the function so you can assign all parameters of a module to a parameter page in one go. This is also hard coded in the G2, so I've started to add this info to the "ModuleDef.xml" also.

- VST is now a "Synth" VST in stead of a "Effect" VST, so you can use it in an instrument rack

- Tested the VST on cubase, solved some bugs that froze Cubase on occasions

- Added version control to the "ModuleDef.xml" and "ParamDef.xml" files. If these files are of different version to the editor application or VST, you get a warning, for example: "Warning, ModuleDef.xml version differs from application."

- Solved most of the compiler warnings and a number of bugs.


v0.2 10-2-2012
==============

unzip

Editor:

Should run on xp and upwards

leave directory structure intact
install libusb-win32, look for instructions here : http://sourceforge.net/p/nmg2editor/wiki/Installing%20LibUSB/
start the G2
run G2_editor_D2011.exe

VST:

make a copy (not replace!) these 3 files to the VST folder:
G2_vst_D2011.dll
ModuleDef.xml
ParamDef.xml

Only tested it with ableton so far.

Before using the vst, you have to start the editor!

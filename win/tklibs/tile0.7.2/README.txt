
$Id: README.txt,v 1.7 2004/10/18 01:52:32 jenglish Exp $

~ About:

The Tile Widget Set is an experimental reimplementation of
some of the core Tk widgets.  The primary purpose is
to generate ideas for how to build the next generation of Tk,
when the asteroid strikes and we prepare for the 9.0 release.

~ Features:

    + A revised and expanded version of the TIP #48 style engine

    + Native look and feel under Windows XP

    + Native L&F under other Windows versions

    + "Revitalized" look and feel under Unix

    + scrollbar, button, checkbutton, radiobutton, menubutton,
      label, frame, and labelframe widgets, plus a partial 
      implementation of the scale widget

    + new notebook and progressbar widgets


~ Compiling:

You have your choice of not one, not two, but three, count'em three!
separate build systems to try!  One of them is bound to work for you.

Jeff's TEA3-based build system: "./configure ; make ; make install" 
in the top-level directory.

Joe's build system: "./configure ; make ; make install"
in the "generic" subdirectory.  Also TEA-based, just done differently.  
MSVC users can also try 'makefile.vc' in that directory
(will need some hand-editing, 'make install' won't work).

Pat's build system (Windows): "cd win ; nmake -f makefile.vc",
or use the Developer Studio Project File "Tile.dsp".

You can also compile in a separate build directory with 
Joe's or Jeff's system, if that's what you're into.

*** NOTE *** 

The tile package requires access to a few Tk internal routines.
These have been added to the private stubs table for Tk 8.5,
but for 8.4 you will need to link against the Tk 8.4 shared
library directly (import library on Windows).


~ Available themes:

The tile package contains the following built-in themes:

    + "classic", the classic Motif-style appearance

    + "default", a simpler, streamlined look for X11

    + "alt", a "revitalized" look and feel similar
      to GTK+'s default theme and Windows NT appearance;

    + "winnative", which uses the native Win32 API to draw widgets

    + "xpnative", which uses the Windows XP "Visual Styles" API

    + "step", an experimental playground for testing
      out new ideas, such as NeXTStep-style scrollbars.

There are some other themes in the "demos" subdirectory:

    + "blue", another experimental playground used to test out
      the pixmap engine.

    + "kroc", a pixmap theme contributed by David Zolli.

    + "Aquativo", an adaptation of the GTK+ theme of the same name.
      (Images are not included in the tile distribution;
      see the instructions in demos/themes/Aquativo.tcl
      for how to download them from the Gnome site).
      Requires the Img package for PNG support.

    + "WinXPBlue", another imported GTK+ theme providing
       a look and feel similar to Windows XP.  (Again,
       images not include in the tile distribution.
       Also requires the Img package for PNG support.)

~ Availability

The tile widget set is currently hosted under the tktable project
at SourceForge:

    <URL: http://tktable.sourceforge.net/ >
    <URL: http://sourceforge.net/projects/tktable/ >

Sources are available under the 'tile' module in CVS.


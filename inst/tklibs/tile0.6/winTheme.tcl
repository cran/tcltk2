#
# $Id: winTheme.tcl,v 1.24 2004/10/30 19:26:38 jenglish Exp $
#
# Tile widget set: Windows Native theme
#

namespace eval tile {

    style theme settings winnative {

	variable WinGUIFont "{MS Sans Serif} 8"
	if {$tcl_platform(platform) eq "windows"
	    && $tcl_platform(osVersion) >= 5.0} {
	    set WinGUIFont "Tahoma 8"
	}

	style default "." \
	    -background SystemButtonFace \
	    -foreground SystemWindowText \
	    -selectforeground SystemHighlightText \
	    -selectbackground SystemHighlight \
	    -troughcolor SystemScrollbar \
	    -font $WinGUIFont \
	    ;

	style map "." -foreground [list disabled SystemGrayText] ;
        style map "." -embossed [list disabled 1] ;

	style default TButton -width -11 -relief raised -shiftrelief 1
	style default TCheckbutton -padding "0 2"
	style default TRadiobutton -padding "0 2"
	style default TMenubutton -padding "8 4" -arrowsize 3 -relief raised

	style map TButton -relief {{!disabled pressed} sunken}

	style default TEntry \
	    -padding 2 -selectborderwidth 0 -insertwidth 1
	style map TEntry \
	    -fieldbackground \
	    	[list readonly SystemButtonFace disabled SystemButtonFace] \
	    -selectbackground [list !focus "#c3c3c3"] \
	    -selectforeground [list !focus "#000000"] \
	    ;

	style default TCombobox -padding 2

	style default Toolbutton -relief flat -padding {8 4}
	style map Toolbutton \
	    -relief {disabled flat selected sunken  pressed sunken  active raised}

	style default TScale -groovewidth 4 -sliderrelief raised
	style map TScale -sliderrelief {pressed sunken}

	style default TNotebook -expandtab {2 2 2 0}
	style default TNotebook.Tab -padding {3 1} -borderwidth 1

	style default TProgress \
	    -borderwidth 1 \
	    -background SystemHighlight \
	    -sliderrelief sunken
    }
}

#
# $Id: xpTheme.tcl,v 1.26 2005/03/15 16:47:29 jenglish Exp $
#
# Tile widget set: XP Native theme
#
# @@@ todo: spacing and padding needs tweaking

namespace eval tile {

    style theme settings xpnative {

	# NOTE: MS documentation says to use "Tahoma 8" in Windows 2000 / XP,
	# although many MS programs still use "MS Sans Serif 8"
	#
	style default . -font "Tahoma 8"

	style default . \
	    -background SystemButtonFace \
	    -foreground SystemWindowText \
	    -selectforeground SystemHighlightText \
	    -selectbackground SystemHighlight \
	    ;

	style map "." \
	    -foreground [list disabled SystemGrayText] \
	    ;

	style default TButton -padding {1 1} -width -11
	style default TMenubutton -padding {8 4}

	style default TNotebook -expandtab {2 2 2 2}

	style default TLabelframe -foreground "#0046d5"

	# OR: -padding {3 3 3 6}, which some apps seem to use.
	style default TEntry -padding {2 2 2 4}
	style map TEntry \
	    -selectbackground [list !focus "#c3c3c3"] \
	    -selectforeground [list !focus "#000000"] \
	    ;
	style default TCombobox -padding 2

	style default Toolbutton -padding {4 4}
    }
}

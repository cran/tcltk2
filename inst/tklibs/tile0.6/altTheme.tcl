#
# $Id: altTheme.tcl,v 1.26 2005/03/03 20:02:47 jenglish Exp $
#
# Tile widget set: Alternate theme
#

namespace eval tile::theme::alt {

    variable colors
    array set colors {
	-frame 		"#d9d9d9"
	-darker 	"#c3c3c3"
	-activebg 	"#ececec"
	-disabledfg	"#a3a3a3"
	-selectbg	"#4a6984"
	-selectfg	"#ffffff"
    }

    style theme settings alt {

	style default "." \
	    -background 	$colors(-frame) \
	    -foreground 	black \
	    -troughcolor	$colors(-darker) \
	    -selectbackground 	$colors(-selectbg) \
	    -selectforeground 	$colors(-selectfg) \
	    -font 		TkDefaultFont \
	    ;

	style map "." -background \
	    [list disabled $colors(-frame)  active $colors(-activebg)] ;
	style map "." -foreground [list disabled $colors(-disabledfg)] ;
        style map "." -embossed [list disabled 1] ;

	style default TButton \
	    -width -11 -padding "1 1" -relief raised -shiftrelief 1 \
	    -highlightthickness 1 -highlightcolor $colors(-frame)

	style map TButton -relief {
	    {pressed !disabled} 	sunken
	    {active !disabled} 	raised
	} -highlightcolor {alternate black}

	style default TCheckbutton -indicatorcolor "#ffffff"
	style default TRadiobutton -indicatorcolor "#ffffff"
	style map TCheckbutton -indicatorcolor \
	    [list  disabled $colors(-frame)  pressed $colors(-frame)]
	style map TRadiobutton -indicatorcolor \
	    [list  disabled $colors(-frame)  pressed $colors(-frame)]

	style default TMenubutton -width -11 -padding "3 3" -relief raised

	style default TEntry -relief sunken -borderwidth 1 -padding 1
	style map TEntry -fieldbackground \
		[list readonly $colors(-frame) disabled $colors(-frame)]
	style map TCombobox -fieldbackground \
		[list readonly $colors(-frame) disabled $colors(-frame)]

	style default Toolbutton -relief flat -padding 2
	style map Toolbutton -relief \
	    {disabled flat selected sunken pressed sunken active raised}
	style map Toolbutton -background \
	    [list pressed $colors(-darker)  active $colors(-activebg)]

	style default TScrollbar -relief raised
	#style map TScrollbar -relief { pressed sunken }

	style default TNotebook -expandtab {2 2 1 0}
	style default TNotebook.Tab -padding {4 2} -background $colors(-darker)
	style map TNotebook.Tab -background [list selected $colors(-frame)]

	style default TScale -groovewidth 4 -troughrelief sunken
	style map TScale -sliderrelief {pressed sunken  {} raised}
	style default TProgress -background $colors(-selectbg)
    }
}

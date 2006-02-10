#
# $Id: tile.tcl,v 1.82 2005/04/10 02:12:29 jenglish Exp $
#
# Tile widget set initialization script.
#

namespace eval tile {
    if {![info exists library]} {
	variable library [file dirname [info script]]
    }
}

source [file join $tile::library keynav.tcl]
source [file join $tile::library fonts.tcl]
source [file join $tile::library cursors.tcl]

## Widgets:
#	Widgets are all defined in the ::ttk namespace.
#
#	For compatibility with earlier Tile releases, we temporarily 
#	create aliases ::tile::widget, and ::t$widget.
#	Using any of the aliases will issue a warning.
#

namespace eval ttk {
    variable widgets {
	button checkbutton radiobutton menubutton label entry
	frame labelframe scrollbar
	notebook progress combobox separator 
	scale
    }

    # deprecated --
    #	Define $old command as a deprecated alias for $new command
    #	$old and $new must be fully namespace-qualified.
    #
    proc deprecated {old new} {
	interp alias {} $old {} ttk::do'deprecate $old $new
    }
    # do'deprecate --
    #	Implementation procedure for deprecated commands --
    #	issue a warning (once), then re-alias old to new.
    #
    proc do'deprecate {old new args} {
	deprecated'warning $old $new
	interp alias {} $old {} $new
	eval [linsert $args 0 $new]
    }
    # deprecated'warning --
    #	Gripe about use of deprecated commands. 
    #
    proc deprecated'warning {old new} {
	puts stderr "$old deprecated -- use $new instead"
    }

    variable wc
    foreach wc $widgets {
	namespace export $wc

	deprecated ::t$wc ::ttk::$wc
	deprecated ::tile::$wc ::ttk::$wc
	namespace eval ::tile [list namespace export $wc]
    }
}

# tile::takefocus --
#	This is the default value of the "-takefocus" option
#	for widgets that participate in keyboard navigation.
#
proc tile::takefocus {w} {
    return [expr {[winfo viewable $w] && [$w instate !disabled]}]
}

# tile::clickToFocus $w --
#	Utility routine, used in <ButtonPress-1> bindings --
#	Assign keyboard focus to the specified widget if -takefocus is enabled.
#
proc tile::clickToFocus {w} {
    if {[tile::takesFocus $w]} { focus $w }
}

# tile::takesFocus w --
#	Test if the widget should take keyboard focus:
#
#	+ If -takefocus is 0 or 1, returns that value
#	+ Otherwise, append the widget name to -takefocus and evaluate it
#	  as a script.
#
# See also: tk::FocusOK
#
# Note: Only use this with tile widgets, it doesn't implement
#	the default heuristics that tk::FocusOK does.
#
proc tile::takesFocus w {
    if {[winfo viewable $w] && ![catch {$w cget -takefocus} takefocus]} {
	switch -- $takefocus {
	    0 -
	    1 {
		return $takefocus 
	    }
	    default { 
		set value [uplevel #0 $takefocus [list $w]] 
		return [expr {$value eq 1}]
	    }
	}
    }
    return 0
}

# CopyBindings $from $to --
#	Utility routine; copies bindings from one bindtag onto another.
#
proc tile::CopyBindings {from to} {
    foreach event [bind $from] {
	bind $to $event [bind $from $event]
    }
}

## Routines for auto-repeat:
#
# NOTE: repeating widgets do not have -repeatdelay
# or -repeatinterval resources as in standard Tk;
# instead a single set of settings is applied application-wide.
# (TODO: make this user-configurable)
#
# (@@@ Windows seems to use something like 500/50 milliseconds
#  @@@ for -repeatdelay/-repeatinterval)
#

namespace eval tile {
    variable Repeat
    array set Repeat {
	delay		300
	interval	100
	timer		{}
	script		{}
    }
}

proc tile::Repeatedly {args} {
    variable Repeat
    after cancel $Repeat(timer)
    set script [uplevel 1 [list namespace code $args]]
    set Repeat(script) $script
    uplevel #0 $script
    set Repeat(timer) [after $Repeat(delay) tile::Repeat]
}

proc tile::Repeat {} {
    variable Repeat
    uplevel #0 $Repeat(script)
    set Repeat(timer) [after $Repeat(interval) tile::Repeat]
}

proc tile::CancelRepeat {} {
    variable Repeat
    after cancel $Repeat(timer)
}

#
# ThemeChanged --
#	Called from [style theme use].
#	Sends a <<ThemeChanged>> virtual event to all widgets.
#
proc tile::ThemeChanged {} {
    set Q .
    while {[llength $Q]} {
	set QN [list]
	foreach w $Q {
	    event generate $w <<ThemeChanged>>
	    foreach child [winfo children $w] {
		lappend QN $child
	    }
	}
	set Q $QN
    }
}

#
# LoadImages $imgdir ?$patternList? -- utility routine for pixmap themes
#
#	Loads all image files in $imgdir matching $patternList.
#	Returns: a paired list of filename/imagename pairs.
proc tile::LoadImages {imgdir {patterns {*.gif}}} {
    foreach pattern $patterns {
	foreach file [glob -directory $imgdir $pattern] {
	    set img [file tail [file rootname $file]]
	    if {![info exists images($img)]} {
		set images($img) [image create photo -file $file]
	    }
	}
    }
    return [array get images]
}

#
# Public API:
#

# tile::availableThemes --
#	Return list of themes registered in the package database.
#
proc tile::availableThemes {} {
    set themes [list]

    foreach pkg [lsearch -inline -all -glob [package names] tile::theme::*] {
	lappend themes [lindex [split $pkg :] end]
    }

    return $themes
}

# tile::setTheme $theme --
#	Set the current theme to $theme, loading it if necessary.
#
proc tile::setTheme {theme} {
    variable currentTheme	;# @@@ Temp -- [style theme use] doesn't work
    if {[lsearch [style theme names] $theme] < 0} {
	package require tile::theme::$theme
    }
    style theme use $theme
    set currentTheme $theme
}

## Load widget bindings:
#
source [file join $tile::library button.tcl]
source [file join $tile::library menubutton.tcl]
source [file join $tile::library scrollbar.tcl]
source [file join $tile::library scale.tcl]
source [file join $tile::library progress.tcl]
source [file join $tile::library notebook.tcl]
source [file join $tile::library paned.tcl]
source [file join $tile::library entry.tcl]
source [file join $tile::library combobox.tcl]
source [file join $tile::library treeview.tcl]

## Label and Labelframe bindings:
#  (not enough to justify their own file...)
#
bind TLabelframe <<Invoke>>	{ keynav::traverseTo [tk_focusNext %W] }
bind TLabel <<Invoke>>		{ keynav::traverseTo [tk_focusNext %W] }

## Load themes:
#
source [file join $tile::library defaults.tcl]
source [file join $tile::library classicTheme.tcl]
source [file join $tile::library altTheme.tcl]
source [file join $tile::library stepTheme.tcl]
source [file join $tile::library clamTheme.tcl]


## Choose platform-specific default theme:
#
# Notes: 
#	+ xpnative takes precedence over winnative if available.
#	+ On X11, users can use the X resource database to
#	  specify a preferred theme (*TkTheme: themeName)
#

set ::tile::defaultTheme "default"

if {[package provide tile::theme::winnative] != {}} {
    source [file join $tile::library winTheme.tcl]
    set ::tile::defaultTheme "winnative"
}
if {[package provide tile::theme::xpnative] != {}} {
    source [file join $tile::library xpTheme.tcl]
    set ::tile::defaultTheme "xpnative"
}
if {[package provide tile::theme::aqua] != {}} {
    source [file join $tile::library aquaTheme.tcl]
    set ::tile::defaultTheme "aqua"
}

set tile::userTheme [option get . tkTheme TkTheme]
if {$tile::userTheme != {}} {
    if {    [lsearch [style theme names] $tile::userTheme] >= 0
        || ![catch [list package require tile::theme::$tile::userTheme]]
    } {
	set ::tile::defaultTheme $tile::userTheme
    }
}

tile::setTheme $::tile::defaultTheme

#*EOF*

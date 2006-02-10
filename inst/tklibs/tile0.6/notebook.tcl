#
# $Id: notebook.tcl,v 1.14 2005/01/02 10:32:59 muonics Exp $
# 
# Bindings for TNotebook widget
#

bind TNotebook <ButtonPress-1>		{ tile::notebook::Press %W %x %y }
bind TNotebook <Key-Right>		{ tile::notebook::CycleTab %W 1 }
bind TNotebook <Key-Left>		{ tile::notebook::CycleTab %W -1 }
bind TNotebook <Control-Key-Tab>	{ tile::notebook::CycleTab %W 1 }
bind TNotebook <Control-Shift-Key-Tab>	{ tile::notebook::CycleTab %W -1 }
catch {
bind TNotebook <Control-ISO_Left_Tab>	{ tile::notebook::CycleTab %W -1 }
}

namespace eval tile::notebook {
    # Instantiate namespace
}

# ActivateTab $nb $tab --
#	Select the specified tab and set focus.
#
# 	If $tab was already the current tab, set the focus to the
#	notebook widget.  Otherwise, set the focus to the first 
#	traversable widget in the pane.  The behavior is that the 
#	notebook takes focus when the user selects the same tab 
#	a second time.  This mirrors Windows tab behavior.
#
proc tile::notebook::ActivateTab {w tab} {
    if {[$w index $tab] eq [$w index current]} {
	focus $w
    } else {
    	$w select $tab
	update 	;# needed so [tk_focusNext] sees correct mapped/unmapped states
	keynav::traverseTo [tk_focusNext $w]
    }
}

# Press $nb $x $y --
#	ButtonPress-1 binding for notebook widgets.
#	Activate the tab under the mouse cursor, if any.
#
proc tile::notebook::Press {w x y} {
    set index [$w index @$x,$y]
    if {$index ne ""} {
	ActivateTab $w $index
    }
} 

# CycleTab --
#	Select the next/previous tab in the list.
#	Only call this from a binding script;
#	it returns a TCL_BREAK code.
#
proc tile::notebook::CycleTab {w dir} {
    if {[$w index end] != 0} {
	set current [$w index current]
	set select [expr {($current + $dir) % [$w index end]}]
	while {[$w tab $select -state] != "normal" && ($select != $current)} {
	    set select [expr {($select + $dir) % [$w index end]}]
	}
	if {$select != $current} {
	    $w select $select
	}
	return -code break
    }
}

# MnemonicActivation $nb $key --
#	Alt-KeyPress binding procedure for mnemonic activation.
#	Scan all tabs for one with the specified mnemonic.
#	If found, activate it and return TCL_BREAK.
#
proc tile::notebook::MnemonicActivation {nb key} {
    set key [string toupper $key]
    foreach tab [$nb tabs] {
	set label [$nb tab $tab -text]
	set underline [$nb tab $tab -underline]
	set mnemonic [string toupper [string index $label $underline]]
	if {$mnemonic ne "" && $mnemonic eq $key} {
	    ActivateTab $nb [$nb index $tab]
	    return -code break
	}
    }
}

# enableTraversal --
#	Enable keyboard traversal for a notebook widget
#	by adding bindings to the containing toplevel window.
#
proc tile::notebook::enableTraversal {nb} {
    set top [winfo toplevel $nb]
    bind $top <Control-Key-Tab> \
	+[list tile::notebook::CycleTab $nb 1]
    bind $top <Shift-Control-Key-Tab> \
	+[list tile::notebook::CycleTab $nb -1]
    bind $top <Alt-KeyPress> \
	+[list tile::notebook::MnemonicActivation $nb %K]
    catch {
	bind $top <Control-Key-ISO_Left_Tab> \
	    +[list tile::notebook::CycleTab $nb -1]
    }
}


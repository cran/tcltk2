proc handleEnterKey {w} {
        # First, delete the current selection
        if {[$w tag nextrange sel 1.0] != ""} {
            $w delete sel.first sel.last
        }

        # Find the whitespace at the start of the current line
        set startl [$w index "insert linestart"]
        set endl [lindex [split $startl "."] 0].end
        regexp {^([ \t]*)} [$w get $startl $endl] -> ws

        # Create a newline, insert whitespace
        $w insert [$w index insert] "\n$ws"
        # If necessary, scroll the view so cursor is visible
        $w see [$w index insert]
    }

    text .some.text.widget

    bind .some.text.widget <Return> {
        handleEnterKey %W
        # break, since we've already handled the Return
        break
    }


# Another implementation that also considers {}:
proc eindent {w {extra "    "}} {
  set lineno [expr {int([$w index insert])}]
  set line [$w get $lineno.0 $lineno.end]
  regexp {^(\s*)} $line -> prefix
  after 1 [list $w insert insert $prefix]
  if {[string index $line end] eq "\{"} {
    after 2 [list $w insert insert $extra]
    after 3 [list $w insert insert+1c $prefix\}\n]
  }
}


# Test
pack [text .t]
 .t insert end \n ;#-- see below
 bind .t <Return> {eindent %W}
 focus -force .t

 bind . <Escape> {exec wish $argv0 &; exit}
 bind . <F1> {console show}
 

#### or...
proc eindent {w {extra "    "}} {
  set lineno [expr {int([$w index insert])}]
  set line [$w get $lineno.0 $lineno.end]
  regexp {^(\s*)} $line -> prefix
  tk::TextInsert $w "\n$prefix"
  if {[string index $line end] eq "\{"} {
    tk::TextInsert $w "$extra"
  }
}

proc eindent {w {extra "    "}} {
  set lineno [expr {int([$w index insert])}]
  set line [$w get $lineno.0 $lineno.end]
  regexp {^(\s*)} $line -> prefix
  if {[string index $line end] eq "\{"} {
    tk::TextInsert $w "\n$prefix$extra"
  } elseif {[string index $line end] eq "\}"} {
    if {[regexp {^\s+\}} $line]} {
      $w delete insert-[expr [string length $extra]+1]c insert-1c
	  tk::TextInsert $w "\n[string range $prefix 0 end-[string length $extra]]"
    } else {
      tk::TextInsert $w "\n$prefix"
	}
  } else {
	tk::TextInsert $w "\n$prefix"
  }
}

bind .t <Return> {eindent %W;break} 
 
 
## Minimal find&replace
proc searchrep {t {replace 1}} {
    set w .sr
    if ![winfo exists $w] {
	toplevel $w
	wm title $w "Search"
	grid [label $w.1 -text Find:] [entry $w.f -textvar Find] \
		[button $w.bn -text Next \
		-command [list searchrep'next $t]] -sticky ew
	bind $w.f <Return> [list $w.bn invoke]
	if $replace {
	    grid [label $w.2 -text Replace:] [entry $w.r -textvar Replace] \
		    [button $w.br -text Replace \
		    -command [list searchrep'rep1 $t]] -sticky ew
	    bind $w.r <Return> [list $w.br invoke]
	    grid x x [button $w.ba -text "Replace all" \
		    -command [list searchrep'all $t]] -sticky ew
	}
	grid x [checkbutton $w.i -text "Ignore case" -variable IgnoreCase] \
		[button $w.c -text Cancel -command "destroy $w"] -sticky ew
	grid $w.i -sticky w
	grid columnconfigure $w 1 -weight 1
	$t tag config hilite -background yellow
    } else {raise $w}
 }
 
 proc searchrep'next w {
     foreach {from to} [$w tag ranges hilite] {
	 $w tag remove hilite $from $to
     }
     set cmd [list $w search -count n -- $::Find insert+2c]
     if $::IgnoreCase {set cmd [linsert $cmd 2 -nocase]}
     set pos [eval $cmd]
     if {$pos ne ""} {
	 $w mark set insert $pos
	 $w see insert
	 $w tag add hilite $pos $pos+${n}c
     }
 }
 
 proc searchrep'rep1 w {
     if {[$w tag ranges hilite] ne ""} {
	 $w delete insert insert+[string length $::Find]c
	 $w insert insert $::Replace
	 searchrep'next $w
	 return 1
     } else {return 0}
 }
 
 proc searchrep'all w {
     set go 1
     while {$go} {set go [searchrep'rep1 $w]}
 }
 
 package require Tk
 pack [text .t]
 .t insert end "hello world, this is some text to test on"
 searchrep .t
 

# Search and highlight all occurences
 proc find {w what tag _status} {
    upvar #0 $_status status
    foreach {from to} [$w tag ranges $tag] {
	$w tag remove $tag $from $to
    }
    set pos [$w search -count n -- $what insert+2c]
    if {$pos eq ""} {
	set status "not found: $what"
    } else {
	set status "found at $pos: $what"
	$w mark set insert $pos
	$w see $pos
	$w tag add $tag $pos $pos+${n}c
    }
 }

# Demo with an entry for the search term, a text, and a label for the status string:

 package require Tk

 pack [entry .e -textvariable Find] -fill x
 bind .e <Return> {find .t $Find hilite state}

 pack [text .t -wrap word]
 .t tag configure hilite -background orange

 pack [label .l -textvariable state] -anchor w
 


### Determine if text changed since last load
#(If using 8.4 there is also the modified flag in the text widget --Ro)   
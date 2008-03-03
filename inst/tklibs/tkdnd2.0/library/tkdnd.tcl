#
# tkdnd.tcl --
# 
#    This file implements some utility procedures that are used by the TkDND
#    package.
#
# This software is copyrighted by:
# George Petasis, National Centre for Scientific Research "Demokritos",
# Aghia Paraskevi, Athens, Greece.
# e-mail: petasis@iit.demokritos.gr
#
# The following terms apply to all files associated
# with the software unless explicitly disclaimed in individual files.
#
# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
#

package require Tk

namespace eval tkdnd {
  variable _topw ".drag"
  variable _tabops
  variable _state
  variable _x0
  variable _y0

  bind TkDND_Drag1 <ButtonPress-1> {tkdnd::_begin_drag press  %W %s %X %Y}
  bind TkDND_Drag1 <B1-Motion>     {tkdnd::_begin_drag motion %W %s %X %Y}
  bind TkDND_Drag2 <ButtonPress-2> {tkdnd::_begin_drag press  %W %s %X %Y}
  bind TkDND_Drag2 <B2-Motion>     {tkdnd::_begin_drag motion %W %s %X %Y}
  bind TkDND_Drag3 <ButtonPress-3> {tkdnd::_begin_drag press  %W %s %X %Y}
  bind TkDND_Drag3 <B3-Motion>     {tkdnd::_begin_drag motion %W %s %X %Y}
  
  # ----------------------------------------------------------------------------
  #  Command tkdnd::initialise: Initialise the TkDND package.
  # ----------------------------------------------------------------------------
  proc initialise { dir } {
    switch $::tcl_platform(platform) {
      unix {
        source $dir/library/tkdnd_unix.tcl
        load $dir/libtkdnd20.so TkDND
      }
      windows {
        source $dir/library/tkdnd_windows.tcl
        load $dir/libtkdnd20.dll TkDND
      }
    }
    source $dir/library/tkdnd_compat.tcl
  };# initialise
};# namespace tkdnd

# ----------------------------------------------------------------------------
#  Command tkdnd::drag_source
# ----------------------------------------------------------------------------
proc tkdnd::drag_source { mode path { event 1 } } {
  set tags [bindtags $path]
  set idx  [lsearch $tags "TkDND_Drag*"]
  switch -- $mode {
    register {
      if { $idx != -1 } {
        bindtags $path [lreplace $tags $idx $idx TkDND_Drag$event]
      } else {
        bindtags $path [concat $tags TkDND_Drag$event]
      }
    }
    unregister {
      if { $idx != -1 } {
        bindtags $path [lreplace $tags $idx $idx]
      }
    }
  }
};# tkdnd::drag_source

# ----------------------------------------------------------------------------
#  Command tkdnd::drop_target
# ----------------------------------------------------------------------------
proc tkdnd::drop_target { mode path { types {} } } {
  switch -- $mode {
    register {
      switch $::tcl_platform(platform) {
        unix {
          _register_types $path [winfo toplevel $path] $types
        }
        windows {
	  set types [olednd::_platform_specific_types $types]
          _RegisterDragDrop $path
	  bind <Destroy> $path {+ tkdnd::_RevokeDragDrop %W}
        }
      }
      set old_types [bind $path <<DropTargetTypes>>]
      foreach type $types {
        if {[lsearch $old_types $type] < 0} {lappend old_types $type}
      }
      bind $path <<DropTargetTypes>> $old_types
    }
    unregister {
      switch $::tcl_platform(platform) {
        unix {
        }
        windows {
          _RevokeDragDrop $path
        }
      }
      bind $path <<DropTargetTypes>> {}
    }
  }
};# tkdnd::drop_target

# ----------------------------------------------------------------------------
#  Command tkdnd::_begin_drag
# ----------------------------------------------------------------------------
proc tkdnd::_begin_drag { event source state X Y } {
  variable _x0
  variable _y0
  variable _state

  switch -- $event {
    press {
      set _x0    $X
      set _y0    $Y
      set _state "press"
    }
    motion {
      if { ![info exists _state] } {
        # This is just extra protection. There seem to be
        # rare cases where the motion comes before the press.
        return
      }
      if { [string equal $_state "press"] } {
        if { abs($_x0-$X) > 3 || abs($_y0-$Y) > 3 } {
          set _state "done"
          _init_drag $source $state $X $Y
        }
      }
    }
  }
};# tkdnd::_begin_drag

# ----------------------------------------------------------------------------
#  Command tkdnd::_init_drag
# ----------------------------------------------------------------------------
proc tkdnd::_init_drag { source state X Y } {
  # Call the <<DragInitCmd>> command.
  set cmd [string map [list %W $source %s $state %X $X %Y $Y] \
                      [bind $source <<DragInitCmd>>]]
  set info [uplevel \#0 $cmd]
  if { $info != "" } {
    foreach { type operators data } $info { break }
  }
};# tkdnd::_init_drag


# ----------------------------------------------------------------------------
#  Command tkdnd::_end_drag
# ----------------------------------------------------------------------------
proc tkdnd::_end_drag { source target op type data result } {
};# tkdnd::_end_drag

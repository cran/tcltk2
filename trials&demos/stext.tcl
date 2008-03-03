if {[info exists ::stext::version]} { return }

namespace eval stext \
{
# ##########################
#
# package stext
  variable version 1.0
#
#   crée un widget text avec ses ascenseurs
#
# --------------------------
# (C) 2005, ulis
# licence NOL (No Obligation Licence)
# --------------------------
# usage :
#
#   package require stext
#   stext .st -bg beige -width 25 -height 15 -wrap none
#   .st insert end ...
#   pack .st -fill both -expand 1
#   .st yview moveto 1.0
#
# ##########################

  package require Tk
  package provide stext $version

  namespace export stext

  proc stext {w args} \
  {
    frame $w
	text $w.stext_t \
      -xscrollc [list $w.stext_hs set] \
      -yscrollc [list $w.stext_vs set]
    scrollbar $w.stext_hs -orient horizontal -command [list $w.stext_t xview]
    scrollbar $w.stext_vs -orient vertical -command [list $w.stext_t yview]
    grid $w.stext_t $w.stext_vs -sticky nsew
    grid $w.stext_hs -sticky ew
    grid rowconfigure $w 0 -weight 1
    grid columnconfigure $w 0 -weight 1
    rename $w ::stext::_$w
    interp alias {} ::$w {} ::$w.stext_t
    if {$args != ""} { uplevel 1 $w.stext_t config $args }
      return $w
    }
  }

  namespace import ::stext::stext
  
# Exemple d'utilisation:
#package require stext
#  stext .st -bg beige -width 25 -height 15 -wrap none
#  pack .st -fill both -expand 1
#  .st insert end [info body ::stext::stext]

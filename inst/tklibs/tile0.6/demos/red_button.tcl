# red_button.tcl - Copyright (C) 2005 Pat Thoyts <patthoyts@users.sf.net>
#
#
# $Id$

package require Tk
package require tile

style layout Red.Button {
    Red.Button.background
    Red.Button.button -children {
        Red.Button.focus -sticky news -children {
            Red.Button.padding -sticky news -children {
                Red.Button.label
            }
        }
    }
}

proc init {} {
    foreach {up down} [load_images] break;
    style element create Red.Button.button image $up \
        -border {2 2} -padding {2 2} -sticky news \
        -map [list {pressed !disabled} $down]
    
    button .b0 -text "Tk button" -background red
    ttk::button .b1 -text "Tile button" -background ren
    ttk::button .b2 -style Red.Button -text "Red tile button"
    pack .b0 .b1 .b2 -side top -fill x
}

proc load_images {} {
    lappend r [image create photo -data {
        R0lGODlhEAAQAOcAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/
        AP//AAAA//8A/wD//////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAMwAAZgAAmQAAzAAA/wAzAAAzMwAzZgAzmQAzzAAz/wBmAABmMwBmZgBm
        mQBmzABm/wCZAACZMwCZZgCZmQCZzACZ/wDMAADMMwDMZgDMmQDMzADM/wD/
        AAD/MwD/ZgD/mQD/zAD//zMAADMAMzMAZjMAmTMAzDMA/zMzADMzMzMzZjMz
        mTMzzDMz/zNmADNmMzNmZjNmmTNmzDNm/zOZADOZMzOZZjOZmTOZzDOZ/zPM
        ADPMMzPMZjPMmTPMzDPM/zP/ADP/MzP/ZjP/mTP/zDP//2YAAGYAM2YAZmYA
        mWYAzGYA/2YzAGYzM2YzZmYzmWYzzGYz/2ZmAGZmM2ZmZmZmmWZmzGZm/2aZ
        AGaZM2aZZmaZmWaZzGaZ/2bMAGbMM2bMZmbMmWbMzGbM/2b/AGb/M2b/Zmb/
        mWb/zGb//5kAAJkAM5kAZpkAmZkAzJkA/5kzAJkzM5kzZpkzmZkzzJkz/5lm
        AJlmM5lmZplmmZlmzJlm/5mZAJmZM5mZZpmZmZmZzJmZ/5nMAJnMM5nMZpnM
        mZnMzJnM/5n/AJn/M5n/Zpn/mZn/zJn//8wAAMwAM8wAZswAmcwAzMwA/8wz
        AMwzM8wzZswzmcwzzMwz/8xmAMxmM8xmZsxmmcxmzMxm/8yZAMyZM8yZZsyZ
        mcyZzMyZ/8zMAMzMM8zMZszMmczMzMzM/8z/AMz/M8z/Zsz/mcz/zMz///8A
        AP//////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////
        /////////////////////yH5BAEAAP8ALAAAAAAQABAAAAg4ALkJHEiwIKWC
        CAdSOpgQ4cKGDhlCVChxIreHFgVizLjRYseJHyGGbDgyYcmIFxeqXMmypUuV
        AQEAOw==
    }]
    lappend r [image create photo -data {
        R0lGODlhEAAQAOcAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/
        AP//AAAA//8A/wD//////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        AAAAMwAAZgAAmQAAzAAA/wAzAAAzMwAzZgAzmQAzzAAz/wBmAABmMwBmZgBm
        mQBmzABm/wCZAACZMwCZZgCZmQCZzACZ/wDMAADMMwDMZgDMmQDMzADM/wD/
        AAD/MwD/ZgD/mQD/zAD//zMAADMAMzMAZjMAmTMAzDMA/zMzADMzMzMzZjMz
        mTMzzDMz/zNmADNmMzNmZjNmmTNmzDNm/zOZADOZMzOZZjOZmTOZzDOZ/zPM
        ADPMMzPMZjPMmTPMzDPM/zP/ADP/MzP/ZjP/mTP/zDP//2YAAGYAM2YAZmYA
        mWYAzGYA/2YzAGYzM2YzZmYzmWYzzGYz/2ZmAGZmM2ZmZmZmmWZmzGZm/2aZ
        AGaZM2aZZmaZmWaZzGaZ/2bMAGbMM2bMZmbMmWbMzGbM/2b/AGb/M2b/Zmb/
        mWb/zGb//5kAAJkAM5kAZpkAmZkAzJkA/5kzAJkzM5kzZpkzmZkzzJkz/5lm
        AJlmM5lmZplmmZlmzJlm/5mZAJmZM5mZZpmZmZmZzJmZ/5nMAJnMM5nMZpnM
        mZnMzJnM/5n/AJn/M5n/Zpn/mZn/zJn//8wAAMwAM8wAZswAmcwAzMwA/8wz
        AMwzM8wzZswzmcwzzMwz/8xmAMxmM8xmZsxmmcxmzMxm/8yZAMyZM8yZZsyZ
        mcyZzMyZ/8zMAMzMM8zMZszMmczMzMzM/8z/AMz/M8z/Zsz/mcz/zMz///8A
        AP//////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////
        /////////////////////yH5BAEAAP8ALAAAAAAQABAAAAg5ACkJHEiwoMGD
        A7kJ5MawoUOHCx9KZBhx4sOKFhtizKiQEkeIHj9SDClyY0aTFlFOVCmR5UWR
        DQMCADs=
    }]
    return $r
}

if {!$tcl_interactive} {
    init
    bind . <Escape> {destroy .}
    tkwait window .
    exit
}

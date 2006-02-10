# compound.tcl - Copyright (C) 2005 Pat Thoyts <patthoyts@users.sf.net>
#
#

if {0} {

The Tile extension makes it possible to create new widget layouts without
dealing with mega-widget issues in many cases. This is an example where we
show how to create a button with an image that is always on the far left
and the text centered.

With a standard tile or Tk button, compound labels with both an image and
some text are supported by the use of the -compound option. However, with
this option the image is always tight against the text. You can modify this
using ''-padding'' and ''-anchor'' but this shifts both the image and the 
text and still doesn't achieve quite what we want.

Using tile we can create a new layout for the current theme. The example
below is the normal layout for a button, but with a '''Compound''' prefix
for both the layout and all the elements. We can now create a button widget
and cause it to use this layout using the ''-style'' option. The reason for
the naming of the elements is that the layout engine will search for elements
defined in the current theme using the full element name. If it cannot find
such an element, it will remove the first part and try again. In this case,
we have not defined any new elements, so when the engine fails to find a 
'''Compound.Button.background''' element, it will use the 
''Button.background'' element and pick ip the standard theme element.

}

package require Tk
package require tile

style layout Compound.Button {
    Compound.Button.background
    Compound.Button.button -children {
        Compound.Button.focus -sticky news -children {
            Compound.Button.padding -sticky news -children {
                Compound.Button.image -side left -sticky w
                Compound.Button.label
            }
        }
    }
}

set file [image create photo -data {
     R0lGODlhCwANAJEAANnZ2QAAAP///////yH5BAEAAAAALAAAAAALAA0AAAIyhI9G8Q0AguSH
     AMQdxQgxEyEFQfItICQokYgEBMm3gBCKLRIQJN8CQii2SECQfAug+FgAOw==
}]
set folder [image create photo -data {
    R0lGODlhEAANAKIAANnZ2YSEhMbGxv//AP///wAAAP///////yH5BAEAAAAALAAAAAAQAA0A
    AANjCIqhiqDLITgyEgi6GoIjIyMYugCBpMsaWBA0giMjIzgyUYBBMjIoIyODEgVBODIygiMj
    E1gQJIMyMjIoI1GAQSMjODIyghMFQSgjI4MyMhJYEDSCIyMjODJRgKHLXAiApcucADs=
}]


button .b0 -compound left -text "Tk button" -image $file
ttk::button .b1 -style Compound.Button -compound text \
    -text File -image $file
ttk::button .b2 -style Compound.Button -compound text \
    -text Folder -image $folder
pack .b0 .b1 .b2 -side top -fill x

if {!$tcl_interactive} {
    bind . <Escape> {destroy .}
    tkwait window .
    exit
}

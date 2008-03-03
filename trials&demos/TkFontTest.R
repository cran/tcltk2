# Test of fonts under tcltk2
library(tcltk2)
tt <- tktoplevel()
txt <- tktext(tt)
tkpack(txt, fill = "both", expand = 1)

# Add text to it
tkinsert(txt, "end", "This is a title\n", "title")
tkinsert(txt, "end", "This is a text ", "text")
tkinsert(txt, "end", "with bold element.", "text bold")

# Now, format these different elements with various fonts
tcl(txt, "tag", "configure", "title", font = "TkTitleFont", foreground = "darkred",
	background = "gray90", elide = 0)
tcl(txt, "tag", "configure", "text", font = "TkTextFont")
tcl(txt, "tag", "configure", "bold", foreground = "darkblue", underline = 1)

# Now, change TkTitleFont and TkTextFont and see how it is dynamically updated!
tkfont.configure("TkTitleFont", family = "Impact", size = 13, weight = "normal")
tkfont.configure("TkTextFont", family = "Georgia", size = 11, weight = "normal") 
# ... but the element we put in bold is NOT updated!... and there is no option to do so!
# So, should we define a separate font??? 

tclvalue(tkget(txt, "1.0", "2.0 lineend"))
tkdump(txt, "1.0", "end") # From there, it should be possible to reconstitute the content!
tksearch(txt, regexp = "is", "1.0", "end")
tcl(txt, "tag", "remove", "title", "1.0", "1.4")	# Eliminate tags on a part of the text
tcl(txt, "tag", "names")	# Name of tags
tcl(txt, "tag", "names", "2.15") # Idem, for a section

.Tcl("selection get") # Get the current selection

# Copy the current selection to the clipboard
tcl("clipboard", "clear")
.Tcl("clipboard append [selection get]")

tkdelete(txt, "sel.first", "sel.last")# Delete the current selection
tcl(txt, "undo")

.Tcl("selection get -selection CLIPBOARD")

### TODO: this does not work!
# Highlight current line in yellow
tcl(txt, "tag", "configure", "_curline", background = "yellow")
tcl(txt, "tag", "add", "_curline", "2.0", "2.0 lineend")


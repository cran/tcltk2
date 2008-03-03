# Test of tkdnd 2.0

# Text drag&drop
tt <- tktoplevel()
droplab <- tk2label(tt, text="Text Drop Target!")
tcl("tkdnd::drop_target", "register", droplab, "DND_Text")
.Tcl(paste("bind", as.character(droplab), "<<DropEnter>> {list move}"))
.Tcl(paste("bind", as.character(droplab), "<<DropPosition>> {list move}"))
.Tcl(paste("bind", as.character(droplab), "<<DropLeave>> { }"))
.Tcl(paste("bind", as.character(droplab), "<<Drop>> {%W configure -text %D; list move}"))
tkgrid(droplab, padx = 30, pady = 30)

draglab <- tk2label(tt,  text = "Drag Source")
tcl("tkdnd::drag_source", "register", draglab)
#tcl("tkdnd::drag_source", "register", draglab, "[list move]")
#tcl("tkdnd::drag_source", "register", draglab, "", 1)
.Tcl(paste("bind", as.character(draglab), "<<DragInitCmd>> {list {copy move} DND_Text {some text!}}"))
#.Tcl(paste("bind", as.character(draglab), "<<DragInitCmd>> {list move DND_Text", 
#	tclvalue(tkcget(draglab, text = NULL)), "}"))
### TODO: this should be used to delete current selection in case of a move
#.Tcl(paste("bind", as.character(draglab), "<<DragEndCmd>> {puts <<DragEndCmd>>}"))
tkgrid(draglab)


tt <- tktoplevel()
droplab <- tklabel(tt, text="Text Drop Target!")
tcl("tkdnd::drop_target", "register", droplab, "DND_Text")
.Tcl(paste("bind", as.character(droplab), "<<DropEnter>> {%W configure -bg yellow; list copy}"))
.Tcl(paste("bind", as.character(droplab), "<<DropPosition>> {list copy}"))
.Tcl(paste("bind", as.character(droplab), "<<DropLeave>> {%W configure -bg white}"))
.Tcl(paste("bind", as.character(droplab), "<<Drop>> {%W configure -text %D; %W configure -bg white}"))
tkgrid(droplab)


tkbind(droplab, "<<DropEnter>>", function() tkconfigure(droplab, bg="yellow")) #)"{%W configure -bg yellow; list copy}")
#tkbind(droplab, "<<DropPosition>>", "{list copy}")
#tkbind(droplab, "<<DropLeave>>", "{%W configure -bg white}")
tkbind(droplab, "<<Drop>>", function(D) tkconfigure(droplab, text="", bg="white")) #"{%W configure -text %D; %W configure -bg white}")
tkgrid(droplab)


tkdnd::drag_source register .text_drag_source
bind .text_drag_source <<DragInitCmd>> \
     {list {copy move} DND_Text {Hello from Tk!}}


# Files drag&drop
tkdnd::drop_target register .drop_target DND_Files
bind .drop_target <<Drop:DND_Files>> \
   {puts %D; %W configure -bg white}


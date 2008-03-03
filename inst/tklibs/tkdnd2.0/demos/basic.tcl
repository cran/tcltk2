# Load the tkdnd package
package require tkdnd

# Create a widget that will be a drag source.
grid [label .drag_source -text {Drag Source}] -sticky snew -columnspan 2
tkdnd::drag_source register .drag_source
bind .drag_source <<DragInitCmd>> {puts <<DragInitCmd>>}
bind .drag_source <<DragEndCmd>>  {puts <<DragEndCmd>>}

# Create a widget that can be a drop target.
grid [label .drop_target -text {Drop Target:} -bg yellow] \
     [label .drop_target_value -text {                  }] -sticky snew

# Register .drop_target as a drop target of every type!
tkdnd::drop_target register .drop_target *

# Add the various events...
set cmd {handle_event %e %W %X %Y %ST %TT %a %A %CST %CTT %t %T %b %D}
bind .drop_target <<DropEnter>>      $cmd
bind .drop_target <<DropPosition>>   $cmd
bind .drop_target <<DropLeave>>      $cmd

# Add the generic <<Drop>> event. This will be called when more specilised
# drop event is not found for the drop.
bind .drop_target <<Drop>>           $cmd

# Add a specialised <<Drop>> event, when will be called if a file is dropped.
bind .drop_target <<Drop:DND_Files>> $cmd

# Create some widgets for showing event info.
set itemList {Event Widget X Y Source_Types Target_Types Source_Actions Action
              Common_Source_Types Common_Target_Types Types Drop_Type
	      Pressed_Keys Data}
foreach item $itemList {
  grid [label .[string tolower $item] -text [string map {_ \ } $item]:\
          -anchor w] [label .[string tolower $item]_val -width 30 -anchor w \
	  -background white -foreground navy] -sticky snew -padx 1 -pady 1
}
grid columnconfigure . 1 -weight 1
grid rowconfigure . 1 -weight 1

proc handle_event $itemList {
  global itemList
  foreach item $itemList {
    .[string tolower $item]_val configure -text [set $item]
  }
  switch -glob $Event {
    <<DropEnter>> {$Widget configure -bg green}
    <<DropLeave>> {$Widget configure -bg yellow}
    <<Drop>> -
    <<Drop:*>>    {$Widget configure -bg yellow
                   .drop_target_value configure -text $Data}
  }
  return copy
};# handle_event

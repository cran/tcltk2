# Use of ffidl and AutoIt3.dll
tclRequire("Ffidl")

# Example: find the hwnd of a window, giving its title
.Tcl("ffidl::callout dll_FindWindowTitle {int pointer-utf8} int [ffidl::symbol user32.dll FindWindowA]")
Rcons <- tcl("dll_FindWindowTitle", 0, "R Console")
Rcons
# Verification
getWindowsHandle("Console")

## Access AutoIt3 dll version
.Tcl("set DLL AutoItDLL.dll") # save instead of retypeing this name in for each function
# This dll needs to be in the path or system directory (so it is found by ffidl)

### TODO: append \library\tcltk2\bin to the path, so that the programs are accessibles!
### TODO: could we add wish here too?

# below are all the public interfaces to AutoIt
.Tcl("
ffidl::callout AUTOIT_BlockInput {int} int [ffidl::symbol $DLL AUTOIT_BlockInput]
ffidl::callout AUTOIT_ClipGet {pointer-utf8} int [ffidl::symbol $DLL AUTOIT_ClipGet]
ffidl::callout AUTOIT_ClipPut {pointer-utf8} int [ffidl::symbol $DLL AUTOIT_ClipPut]
ffidl::callout AUTOIT_DetectHiddenText {int} int [ffidl::symbol $DLL AUTOIT_DetectHiddenText]
ffidl::callout AUTOIT_IfWinActive {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_IfWinActive]
ffidl::callout AUTOIT_IfWinExist {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_IfWinExist]
ffidl::callout AUTOIT_Init {} int [ffidl::symbol $DLL AUTOIT_Init]
ffidl::callout AUTOIT_IniDelete {pointer-utf8 pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_IniDelete]
ffidl::callout AUTOIT_IniRead {pointer-utf8 pointer-utf8 pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_IniRead]
ffidl::callout AUTOIT_IniWrite {pointer-utf8 pointer-utf8 pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_IniWrite]
ffidl::callout AUTOIT_LeftClick {int int} int [ffidl::symbol $DLL AUTOIT_LeftClick]
ffidl::callout AUTOIT_LeftClickDrag {int int int int} int [ffidl::symbol $DLL AUTOIT_LeftClickDrag]
ffidl::callout AUTOIT_MouseMove {int int} int [ffidl::symbol $DLL AUTOIT_MouseMove]
ffidl::callout AUTOIT_MouseGetPosX {} int [ffidl::symbol $DLL AUTOIT_MouseGetPosX]
ffidl::callout AUTOIT_MouseGetPosY {} int [ffidl::symbol $DLL AUTOIT_MouseGetPosY]
ffidl::callout AUTOIT_RightClick {int int} int [ffidl::symbol $DLL AUTOIT_RightClick]
ffidl::callout AUTOIT_RightClickDrag {int int int int} int [ffidl::symbol $DLL AUTOIT_RightClickDrag]
ffidl::callout AUTOIT_Send {pointer-utf8} int [ffidl::symbol $DLL AUTOIT_Send]
ffidl::callout AUTOIT_SetCapslockState {int} int [ffidl::symbol $DLL AUTOIT_SetCapslockState]
ffidl::callout AUTOIT_SetKeyDelay {int} int [ffidl::symbol $DLL AUTOIT_SetKeyDelay]
ffidl::callout AUTOIT_SetStoreCapslockMode {int} int [ffidl::symbol $DLL AUTOIT_SetStoreCapslockMode]
ffidl::callout AUTOIT_SetTitleMatchMode {int} int [ffidl::symbol $DLL AUTOIT_SetTitleMatchMode]
ffidl::callout AUTOIT_SetWinDelay {int} int [ffidl::symbol $DLL AUTOIT_SetWinDelay]
ffidl::callout AUTOIT_Shutdown {int} int [ffidl::symbol $DLL AUTOIT_Shutdown]
ffidl::callout AUTOIT_Sleep {int} int [ffidl::symbol $DLL AUTOIT_Sleep]
ffidl::callout AUTOIT_WinActivate {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinActivate]
ffidl::callout AUTOIT_WinClose {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinClose]
ffidl::callout AUTOIT_WinMove {pointer-utf8 pointer-utf8 int int int int} int [ffidl::symbol $DLL AUTOIT_WinMove]
ffidl::callout AUTOIT_WinGetActiveTitle {pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinGetActiveTitle]
ffidl::callout AUTOIT_WinHide {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinHide]
ffidl::callout AUTOIT_WinKill {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinKill]
ffidl::callout AUTOIT_WinMaximize {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinMaximize]
ffidl::callout AUTOIT_WinMinimize {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinMinimize]
ffidl::callout AUTOIT_WinMinimizeAll {} int [ffidl::symbol $DLL AUTOIT_WinMinimizeAll]
ffidl::callout AUTOIT_WinMinimizeAllUndo {} int [ffidl::symbol $DLL AUTOIT_WinMinimizeAllUndo]
ffidl::callout AUTOIT_WinRestore {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinRestore]
ffidl::callout AUTOIT_WinSetTitle {pointer-utf8 pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinSetTitle]
ffidl::callout AUTOIT_WinShow {pointer-utf8 pointer-utf8} int [ffidl::symbol $DLL AUTOIT_WinShow]
ffidl::callout AUTOIT_WinWait {pointer-utf8 pointer-utf8 int} int [ffidl::symbol $DLL AUTOIT_WinWait]
ffidl::callout AUTOIT_WinWaitActive {pointer-utf8 pointer-utf8 int} int [ffidl::symbol $DLL AUTOIT_WinWaitActive]
ffidl::callout AUTOIT_WinWaitClose {pointer-utf8 pointer-utf8 int} int [ffidl::symbol $DLL AUTOIT_WinWaitClose]
ffidl::callout AUTOIT_WinWaitNotActive {pointer-utf8 pointer-utf8 int} int [ffidl::symbol $DLL AUTOIT_WinWaitNotActive]
")

 # to use any of these fucntion in Tcl just use the assign Tcl proc name (after callout)
 # example below will minize all the windows on the screen ;-)
tcl("AUTOIT_WinMinimizeAll")
invisible(tcl("AUTOIT_Send", "^L"))	# Clear the console (must be active!)
writeClipboard("ls(all.names = TRUE)\n")
invisible(tcl("AUTOIT_Send", "^V"))	# Paste command to the console


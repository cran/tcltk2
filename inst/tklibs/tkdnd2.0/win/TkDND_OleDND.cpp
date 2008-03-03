/*
 * TkDND_OleDND.h -- Tk OleDND Drag'n'Drop Protocol Implementation
 * 
 *    This file implements the unix portion of the drag&drop mechanism
 *    for the tk toolkit. The protocol in use under windows is the
 *    OleDND protocol.
 *
 * This software is copyrighted by:
 * Georgios Petasis, Athens, Greece.
 * e-mail: petasisg@yahoo.gr, petasis@iit.demokritos.gr
 * Laurent Riesterer, Rennes, France.
 * e-mail: laurent.riesterer@free.fr
 *
 * The following terms apply to all files associated
 * with the software unless explicitly disclaimed in individual files.
 *
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose, provided
 * that existing copyright notices are retained in all copies and that this
 * notice is included verbatim in any distributions. No written agreement,
 * license, or royalty fee is required for any of the authorized uses.
 * Modifications to this software may be copyrighted by their authors
 * and need not follow the licensing terms described here, provided that
 * the new terms are clearly indicated on the first page of each file where
 * they apply.
 * 
 * IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
 * DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
 * IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
 * NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 * MODIFICATIONS.
 */

#include "OleDND.h"

int TkDND_RegisterDragDropObjCmd(ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[]) {
  TkDND_DropTarget *pDropTarget;
  Tk_Window tkwin;
  HRESULT hret;
  
  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "path");
    return TCL_ERROR;
  }
  Tcl_ResetResult(interp);

  tkwin = TkDND_TkWin(objv[1]);
  if (tkwin == NULL) {
    Tcl_AppendResult(interp, "invalid Tk widget path: \"",
                             Tcl_GetString(objv[1]), (char *) NULL);
    return TCL_ERROR;
  }
  Tk_MakeWindowExist(tkwin);

  pDropTarget = new TkDND_DropTarget(interp, tkwin);
  if (pDropTarget == NULL) {
    Tcl_SetResult(interp, "out of memory", TCL_STATIC);
    return TCL_ERROR;
  }
  hret = RegisterDragDrop(Tk_GetHWND(Tk_WindowId(tkwin)), pDropTarget);
  switch (hret) {
    case E_OUTOFMEMORY: {
      delete pDropTarget;
      Tcl_AppendResult(interp, "unable to register \"", Tcl_GetString(objv[1]),
                "\" as a drop target: out of memory", (char *) NULL);
      break;
    }
    case DRAGDROP_E_INVALIDHWND: {
      delete pDropTarget;
      Tcl_AppendResult(interp, "unable to register \"", Tcl_GetString(objv[1]),
                "\" as a drop target: invalid window handle", (char *) NULL);
      break;
    }
    case DRAGDROP_E_ALREADYREGISTERED: {
      /* Silently ignore this. The window has been registered before. */
    }
    case S_OK: return TCL_OK;
  }
  return TCL_ERROR;
}; /* TkDND_RegisterDragDropObjCmd */

int TkDND_RevokeDragDropObjCmd(ClientData clientData, Tcl_Interp *interp,
                                 int objc, Tcl_Obj *CONST objv[]) {
  Tk_Window tkwin;
  HRESULT hret;
  
  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "path");
    return TCL_ERROR;
  }
  Tcl_ResetResult(interp);

  tkwin = TkDND_TkWin(objv[1]);
  if (tkwin == NULL) {
    Tcl_AppendResult(interp, "invalid Tk widget path: \"",
                             Tcl_GetString(objv[1]), (char *) NULL);
    return TCL_ERROR;
  }

  hret = RevokeDragDrop(Tk_GetHWND(Tk_WindowId(tkwin)));
  if (hret != S_OK) {
    Tcl_AppendResult(interp, "Tk widget \"", Tcl_GetString(objv[1]),
              "\" has never been registered as a drop target", (char *) NULL);
    return TCL_ERROR;
  }
  	    
  return TCL_OK;
}; /* TkDND_RevokeDragDropObjCmd */

/*
 * For C++ compilers, use extern "C"
 */
#ifdef __cplusplus
extern "C" {
#endif
DLLEXPORT int Tkdnd_Init(Tcl_Interp *interp);
DLLEXPORT int Tkdnd_SafeInit(Tcl_Interp *interp);
#ifdef __cplusplus
}
#endif

int DLLEXPORT Tkdnd_Init(Tcl_Interp *interp) {
  int major, minor, patchlevel;
  HRESULT hret;

  if (
#ifdef USE_TCL_STUBS 
      Tcl_InitStubs(interp, "8.3", 0)
#else
      Tcl_PkgRequire(interp, "Tcl", "8.3", 0)
#endif /* USE_TCL_STUBS */
            == NULL) {
            return TCL_ERROR;
  }
  if (
#ifdef USE_TK_STUBS
       Tk_InitStubs(interp, "8.3", 0)
#else
       Tcl_PkgRequire(interp, "Tk", "8.3", 0)
#endif /* USE_TK_STUBS */
            == NULL) {
            return TCL_ERROR;
  }

  /*
   * Get the version, because we really need 8.3.3+.
   */
  Tcl_GetVersion(&major, &minor, &patchlevel, NULL);
  if ((major == 8) && (minor == 3) && (patchlevel < 3)) {
    Tcl_SetResult(interp, "tkdnd requires Tk 8.3.3 or greater", TCL_STATIC);
    return TCL_ERROR;
  }

  /*
   * Initialise OLE.
   */
  hret = OleInitialize(NULL);
  
  /*
   * If OleInitialize returns S_FALSE, OLE has already been initialized
   */
  if (hret != S_OK && hret != S_FALSE) {
    Tcl_AppendResult(interp, "unable to initialize OLE2",
      (char *) NULL);
    return TCL_ERROR;
  }

  /* Register the various commands */
  if (Tcl_CreateObjCommand(interp, "_RegisterDragDrop",
           (Tcl_ObjCmdProc*) TkDND_RegisterDragDropObjCmd,
           (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL) == NULL) {
      return TCL_ERROR;
  }
  if (Tcl_CreateObjCommand(interp, "_RevokeDragDrop",
           (Tcl_ObjCmdProc*) TkDND_RevokeDragDropObjCmd,
           (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL) == NULL) {
      return TCL_ERROR;
  }

  Tcl_PkgProvide(interp, TKDND_PACKAGE, TKDND_VERSION);
  return TCL_OK;
} /* Tkdnd_Init */

int DLLEXPORT Tkdnd_SafeInit(Tcl_Interp *interp) {
  return Tkdnd_Init(interp);
} /* Tkdnd_SafeInit */

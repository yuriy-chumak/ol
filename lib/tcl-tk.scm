(define-library (lib tcl-tk)
   (export
      Tcl_CreateInterp
      Tcl_Init
      Tcl_Eval
      Tcl_EvalFile

      Tcl_GetStringResult
      Tcl_FindExecutable
      Tcl_CreateCommand Tcl_SetResult
      Tcl_DeleteCommand

      Tk_Init
      Tk_MainLoop

      TCL_OK
      TCL_ERROR
      TCL_RETURN
      TCL_BREAK
      TCL_CONTINUE
   )
   (import
      (r5rs core)
      (otus ffi))
(begin

   (define TCL (or
      (dlopen "tcl86.dll")
      (dlopen "libtcl8.6.so")))
   (define TK (or
      (dlopen "tk86.dll")
      (dlopen "libtk8.6.so")))

   (define Tcl_Interp* type-vptr)
   (define TclCommand type-vptr)
   (define int type-int+)

   (define Tcl_FreeProc* type-void) ; todo: set to ansi C "free"
   (define Tcl_CmdDeleteProc* type-void) ; todo: same

   (define Tcl_CreateInterp (dlsym TCL Tcl_Interp* "Tcl_CreateInterp"))
   (define Tcl_Init (dlsym TCL int "Tcl_Init" Tcl_Interp*))
   (define Tcl_Eval (dlsym TCL int "Tcl_Eval" Tcl_Interp* type-string))
   (define Tcl_EvalFile (dlsym TCL int "Tcl_EvalFile" Tcl_Interp* type-string))

   (define Tcl_FindExecutable (dlsym TCL type-string "Tcl_FindExecutable" type-string))
   (define Tcl_GetStringResult (dlsym TCL type-string "Tcl_GetStringResult"))

   (define Tcl_CreateCommand (dlsym TCL TclCommand "Tcl_CreateCommand" Tcl_Interp* type-string type-callable type-vptr Tcl_CmdDeleteProc*))
   (define Tcl_DeleteCommand (dlsym TCL TclCommand "Tcl_DeleteCommand" Tcl_Interp* type-string))
   (define Tcl_SetResult (dlsym TCL type-void "Tcl_SetResult" Tcl_Interp* type-string Tcl_FreeProc*))

   (define Tk_Init (dlsym TK int "Tk_Init" Tcl_Interp*))
   (define Tk_MainLoop (dlsym TK int "Tk_MainLoop"))

   (define TCL_OK 0)
   (define TCL_ERROR 1)
   (define TCL_RETURN 2)
   (define TCL_BREAK 3)
   (define TCL_CONTINUE 4)

))
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
      (scheme core)
      (otus ffi))
(begin

   (define TCL (or
      (load-dynamic-library "tcl86.dll")
      (load-dynamic-library "libtcl8.6.so")))
   (define TK (or
      (load-dynamic-library "tk86.dll")
      (load-dynamic-library "libtk8.6.so")))

   (define Tcl_Interp* type-vptr)
   (define TclCommand type-vptr)
   (define int type-int+)

   (define Tcl_FreeProc* fft-void) ; todo: set to ansi C "free"
   (define Tcl_CmdDeleteProc* fft-void) ; todo: same

   (define Tcl_CreateInterp (TCL Tcl_Interp* "Tcl_CreateInterp"))
   (define Tcl_Init (TCL int "Tcl_Init" Tcl_Interp*))
   (define Tcl_Eval (TCL int "Tcl_Eval" Tcl_Interp* type-string))
   (define Tcl_EvalFile (TCL int "Tcl_EvalFile" Tcl_Interp* type-string))

   (define Tcl_FindExecutable (TCL type-string "Tcl_FindExecutable" type-string))
   (define Tcl_GetStringResult (TCL type-string "Tcl_GetStringResult"))

   (define Tcl_CreateCommand (TCL TclCommand "Tcl_CreateCommand" Tcl_Interp* type-string type-callable type-vptr Tcl_CmdDeleteProc*))
   (define Tcl_DeleteCommand (TCL TclCommand "Tcl_DeleteCommand" Tcl_Interp* type-string))
   (define Tcl_SetResult (TCL fft-void "Tcl_SetResult" Tcl_Interp* type-string Tcl_FreeProc*))

   (define Tk_Init (TK int "Tk_Init" Tcl_Interp*))
   (define Tk_MainLoop (TK int "Tk_MainLoop"))

   (define TCL_OK 0)
   (define TCL_ERROR 1)
   (define TCL_RETURN 2)
   (define TCL_BREAK 3)
   (define TCL_CONTINUE 4)

))
#!/usr/bin/ol

(import (lib tcl-tk))

(define tcl (Tcl_CreateInterp))
(Tcl_Init tcl)



;typedef int (Tcl_CmdProc) (ClientData clientData, Tcl_Interp *interp,
;	int argc, CONST84 char *argv[]);

;(define add (syscall 85 (cons
;   ; arguments ; no return type yet
;
;   (list type-vptr type-vptr type-int+ (list type-string type-string type-string))
;   (lambda (userdata tcl argc argv)
;      (print (reverse argv))
;      ;(fold + '(1 2 3))
;      #t
;      ;(print "a: " (first argv))
;      ;(print "b: " (second argv))
;      ;
;)) #f #f))

(define calculate (syscall 85 (cons
   (list type-vptr type-vptr type-int+ (list type-string type-string))
   (lambda (userdata tcl argc argv)
      (let ((argv (reverse argv)))
         (Tcl_SetResult tcl (fold string-append "" (list "(" (cadr argv) ")")) #f))
      TCL_OK
)) #f #f))

(define cmd (Tcl_CreateCommand tcl "calculate" calculate #f #f))
;
;(Tcl_Eval tcl "add \"1\" \"2\"")
;
;
; run
;(Tk_Init tcl)
;(Tcl_Eval tcl "
;   grid [ttk::button .b -text \"---\"]
;")
(Tcl_EvalFile tcl "p.tcl")

(Tk_MainLoop)
,quit

#!/usr/bin/ol

(import (lib winapi))

(MessageBox #f "aпbривет" "hello" 0)

(print "привет")
(print (string->runes "привет"))
(print (string-length "a-привет"))

,quit


(import (lib tcl-tk))

(define tcl (Tcl_CreateInterp))
(Tcl_Init tcl)


;typedef int (Tcl_CmdProc) (ClientData clientData, Tcl_Interp *interp,
;	int argc, CONST84 char *argv[]);

;(define add (syscall 175 (cons
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

(define calculate (syscall 175 (cons
   (list type-vptr type-vptr type-int+ (list type-string type-string))
   (lambda (userdata tcl argc argv)
      (let ((argv (reverse argv)))
         (Tcl_SetResult tcl (c-string (fold string-append "" (list "(" (cadr argv) ")"))) #f))
      TCL_OK
)) #f #f))

(define cmd (Tcl_CreateCommand tcl "calculate" calculate #f #f))
;
;(Tcl_Eval tcl (c-string "add \"1\" \"2\""))
;
;
; run
;(Tk_Init tcl)
;(Tcl_Eval tcl (c-string "
;   grid [ttk::button .b -text \"---\"]
;"))
(Tcl_EvalFile tcl (c-string "p.tcl"))

(Tk_MainLoop)
,quit

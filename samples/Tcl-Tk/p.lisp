#!/usr/bin/ol

(import (lib tcl-tk))

(define tcl (Tcl_CreateInterp))
(Tcl_Init tcl)

; let's declare function
(define calculate (syscall 85 (cons
   (cons type-vptr (list type-vptr type-int+ (list type-string type-string)))
   (lambda (userdata tcl argc argv)
      (let ((argv (reverse argv)))
         (print "argc: " argc)
         (print "argv: " argv)

         (let ((result
                  (fold string-append "" (list "{" (cadr argv) "}"))))

            (Tcl_SetResult tcl (c-string result) #f)))
      TCL_OK
)) #f #f))

; and send it to the tcl
(define cmd (Tcl_CreateCommand tcl (c-string "calculate") calculate #f #f))



;(Tcl_Eval tcl (c-string "calculate 123"))
;
;
; run
;(Tk_Init tcl)
;(Tcl_Eval tcl (c-string "
;   grid [ttk::button .b -text \"---\"]
;"))
(Tcl_EvalFile tcl (c-string "p.tcl"))

(Tk_MainLoop)

(Tcl_DeleteCommand tcl (c-string "calculate"))


,quit

#!/usr/bin/env ol

(import (lib tcl-tk)
        (otus ffi)
        (scheme misc))

(define tcl (Tcl_CreateInterp))
(Tcl_Init tcl)

; let's declare function
(define calculate (vm:pin (cons
   (cons fft-int
   ;     userdata  tcl       argc      arguments in reverse order
   (list type-vptr type-vptr fft-int  (list type-string type-string)))
   (lambda (userdata tcl argc argv)
      (let ((argv (reverse argv)))
         (let*((feets (string->number (cadr argv) 10))
               (meters (* feets 0.3048))
               (result
                  (number->string (inexact meters))))
            (print "feets: " feets)
            (print "meters: " meters)
            (Tcl_SetResult tcl result #f)))
      TCL_OK
))))
(define calculateCallback (make-callback calculate))

; and send it to the tcl
(define cmd (Tcl_CreateCommand tcl "calculate" calculateCallback #f #f))

(Tcl_EvalFile tcl "p.tcl")
(Tk_MainLoop)
(Tcl_DeleteCommand tcl "calculate")

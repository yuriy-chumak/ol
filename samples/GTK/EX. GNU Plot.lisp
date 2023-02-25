#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3)
   (lib gtk-3 socket))
(import (otus ffi))
(import (scheme inexact))

; main:
(gtk_init '(0) #f)

; gnuplot:
; gnuplot:
(import (otus ffi))
(define FILE* type-vptr)
(define this (load-dynamic-library #f))
(define popen (this FILE* "popen" type-string type-string))
(define fprintf (this fft-int "fprintf" FILE* type-string))
(define fflush (this fft-void "fflush" FILE*))
; start gnuplot
(define gnuplot (popen "gnuplot" "w"))
(define (plot . args)
   (define buffer (open-output-string))
   (for-each (lambda (a)
         (display-to buffer a))
      args)
   (fprintf gnuplot "%s\n" (get-output-string buffer))
   (fflush gnuplot))

; load and decode a file
; -------------------------------------------------------------------------
(define builder (gtk_builder_new_from_file "templates/EX. GNU Plot.glade"))
(gtk_builder_connect_signals builder #f)

; get window from template
(define window (gtk_builder_get_object builder "window"))

; get a button from template
(define plot-area (gtk_builder_get_object builder "plot-area"))
(define socket (gtk_socket_new))
(gtk_widget_show socket)
(gtk_container_add plot-area socket)
(gtk_widget_realize socket) ; if one of ancestors is not yet visible.
(define plug-added ; connect notification
   (GTK_CALLBACK (self userdata)
      (print "A widget (most likley gnuplot) has just been jacked in!")
      TRUE))
(g_signal_connect socket "plug-added" (G_CALLBACK plug-added) NULL)

(plot "set terminal x11 window '"
      (number->string (gtk_socket_get_id socket) 16) "'")
(plot "set title 'Trigonometry'")
(plot "set xzeroaxis\nset xtics axis")
(plot "set yzeroaxis\nset ytics axis")
(plot "plot '-' with lines")
(plot "0 0") (plot "e")

(define draw-gnuplot
   (GTK_CALLBACK (self userdata)
      (plot "plot '-' with lines")
      (for-each (lambda (i)
            (plot i " " (cos (/ i #i100))))
         (iota 628 -314))
      (plot "e")
      TRUE))
(define regenerate (gtk_builder_get_object builder "regenerate"))
(g_signal_connect regenerate "clicked" (G_CALLBACK draw-gnuplot) NULL)

; builder is no more required, let's free a system resource
(g_object_unref builder)
; socket+gnuplot workaround:
;  we should force window size change
   (define-values (w h) (values '(0) '(0)))
   (gtk_window_get_default_size window w h)
   (gtk_window_resize window (car w) (+ (car h) 1))
(gtk_widget_show_all window)

; close button processor
(define quit
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit)))

(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; show window and run
(gtk_main)

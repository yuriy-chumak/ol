#!/usr/bin/env ol
(import
   (lib glib-2)
   (lib gtk-3))

; application init
(define (activate app)
   ; main application window
   (define window (GtkWindow app {
      'title "Window With Content"
      'width 320 'height 180
   }))

   ; add a label to the window
   (define label (GtkLabel
      "Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit."))
   ((window 'add) label)

   ; display the window (and it's content)
   ((window 'show-all)))

;; create an application
(define app (GtkApplication {
   'id "org.gtk.example"
   'on-activate activate
}))

;; run
((app 'run) (command-line))

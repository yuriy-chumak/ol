#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "5. Terminal")
(import (lib gl console))

(import (OpenGL version-1-0))

(define fps (create-window 70 24 10 1))
(define started (time-ms)) (define time '(0))
(define frames '(0 . 0))

(set-window-writer fps (lambda (print)
   (set-car! frames (+ (car frames) 1))
   (let ((now (time-ms)))
      (if (> now (+ started (car time) 1000))
         (begin
            (set-cdr! frames (car frames))
            (set-car! frames 0)
            (set-car! time (- now started)))))
   (print 'fps GREEN (cdr frames) " fps")
))


(define main (create-window 1 1 78 23))
(set-window-border main WHITE)
(set-window-writer main (lambda (print)
   (define F LIGHTGRAY)
   (define G GRAY)
   (define W WHITE)
   (apply print (list F
      "DEC's first successful video terminal was the " W "VT50" F ", introduced in 1974 and "    "\n"
      "quickly replaced by the VT52 in 1975. The " W "VT52" F " featured a text display with 80" "\n"
      "columns and 24 rows, bidirectional scrolling, and a custom control language"     "\n"
      "that allowed the cursor to be moved about the screen. These 'smart terminals'"   "\n"
      "were a hit both due to their capabilities and their ability to be run over"      "\n"
      "inexpensive serial links, rather than custom connection as in the case of"       "\n"
      "systems like the " W "IBM 3270" F " that generally required expensive controllers for"    "\n"
      "distributed applications."
      "\n\n"

      "The VT100 was introduced in August 1978, replacing the VT50/VT52 family. Like"   "\n"
      "the earlier models, it communicated with its " G "host" F " system over serial lines at"  "\n"
      "a minimum speed of 50 bit/s, but increased the maximum speed to double that of"  "\n"
      "the VT52 at 19,200 bit/s. Basic improvements on the VT52 included a 132 column"  "\n"
      "mode, and a variety of 'graphic renditions' including blinking, bolding,"        "\n"
      G "reverse video" F ", and underlining. The VT100 also introduced an additional " G "box-" "\n"
      "drawing character" F " set containing various pseudographics that allowed the"   "\n"
      "drawing of on-screen forms. All setup of the VT100 was accomplished using"       "\n"
      "interactive displays presented on the screen; the setup data was stored in " G "non-"     "\n"
      "volatile memory" F " within the terminal. Maintainability was also significantly""\n"
      "improved since a VT100 could be disassembled quickly without use of tools."

      "\n\n"
      "In 1983, the " W "VT100" F " was replaced by the more-powerful " W "VT200" F " series terminals"     "\n"
      "such as the " W "VT220" F "."))
))


; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.1 0.1 0.1 1)

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (render-windows)))

(gl:set-mouse-handler (lambda (button x y)
   (let ((selection (windows-make-selection x y)))
      (cond
         ((symbol? selection)
            (case selection
               ('fps
                  (print "You've clicked 'fps' window"))))
         ((function? selection)
            (selection))))))

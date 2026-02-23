#!/usr/bin/env ol

(define-library (file xpm)
   (export
      xpm3-parser
      (exports (file parser)))
   (import
      (otus lisp)
      (file parser)
      (data s-exp))
(begin
   (define get-rest-of-line ; internal
      (let-parse* (
            (/ (greedy* (byte-if (lambda (x) (not (eq? x #\newline))))))
            (/ (get-imm #\newline))) ;; <- note that this won't match if line ends to eof
         #true))

   (define xpm3-parser
      (let-parses (
            (* get-rest-of-line) ; /* XPM */
            (* get-rest-of-line) ; static char *sample[] = {
            (* get-rest-of-line) ; /* columns rows colors chars-per-pixel */

            (c-code (get-imm #\"))
            (columns get-number)
            (* (get-greedy+ (get-imm #\space)))
            (rows get-number)
            (* (get-greedy+ (get-imm #\space)))
            (colors get-number)
            (* (get-greedy+ (get-imm #\space)))
            (chars-per-pixel (get-imm #\1))
            (* (get-greedy+ (get-imm #\space)))
            (* get-rest-of-line)

            (* (times colors get-rest-of-line))
            (* get-rest-of-line) ; /* pixels */

            (bitmap (times rows (let-parses (
                  (* (get-imm #\"))
                  (row (times columns get-byte))
                  (* get-rest-of-line))
               row)))
            (/ get-rest-of-line))
         {
            'columns columns
            'rows    rows
            'colors  colors
            'bitmap  bitmap
         }))
         ;; (cons [columns rows colors 1] bitmap)))
))

(import (file xpm))

; --------------------------
; skeletal-animation library
(define filename "sample.xpm")
(define xpm3 (parse xpm3-parser (file->bytestream "sample.xpm") #f #f #f))

(define MAX 65536)  ; should be power of two
; size of game board (should be less than MAX)
(define WIDTH (xpm3 'columns))
(define HEIGHT (xpm3 'rows))

; helper function
(define (hash x y)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT)))
   (+ (* y MAX) x)))

; ---------------
(import (lib gl-2))
(gl:set-window-title "Wireworld")

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho 0 WIDTH 0 HEIGHT 0 1)

(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height)
   (glPointSize (/ height HEIGHT))))


; generate random field
(import (scheme dynamic-bindings))
(define userdata (make-parameter
   (fold (lambda (ff v y)
            (fold (lambda (ff v x)
                     (if (eq? v #\space) ff (put ff (hash x y) v)))
               ff v (iota (length v))))
      #empty (reverse (xpm3 'bitmap)) (iota (length (xpm3 'bitmap))))))

; main game loop
(gl:set-renderer (lambda (mouse)
(let ((generation (userdata)))
   (glClear GL_COLOR_BUFFER_BIT)

   ; draw the cells
   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_POINTS)
      (ff-fold (lambda (st key value)
         (case value
            (#\o (glColor3f 1 1 0))
            (#\H (glColor3f 0 1 1))
            (#\T (glColor3f 1 0 0)))
         (glVertex2f (mod key MAX)
                     (div key MAX))
      ) #f generation)
   (glEnd)

   (userdata
   (ff-fold (lambda (ff k v)
         (case v
            (#\H (put ff k #\T)) ; head -> tail
            (#\T (put ff k #\o)) ; tail -> wire
            (#\o (let ((heads (fold (let ((x (mod k MAX))
                                          (y (div k MAX)))
                                       (lambda (s xy)
                                          (+ s (if (eq? (getf generation (hash (+ x (car xy)) (+ y (cdr xy)))) #\H) 1 0))))
                                 0
                                  '((-1 . -1) ( 0 . -1) ( 1 . -1)
                                    (-1 .  0)           ( 1 .  0)
                                    (-1 .  1) ( 0 .  1) ( 1 .  1)))))
                     (case heads
                        ((1 2)
                           (put ff k #\H))
                        (else
                           (put ff k #\o)))))
            (else
               ff)))
      #empty generation))

   (sleep 5)
)))

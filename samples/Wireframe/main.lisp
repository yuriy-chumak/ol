#!/usr/bin/ol

(define-library (file xpm)
   (export
      xpm3-parser)
   (import
      (otus lisp)
      (file parser)
      (lang sexp))
(begin
   (define get-rest-of-line ; internal
      (let-parses
         ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x 10))))))
            (skip (get-imm 10))) ;; <- note that this won't match if line ends to eof
         chars))

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

            (* (get-n-times colors get-rest-of-line))
            (* get-rest-of-line) ; /* pixels */

            ;(bitmap (get-n-times columns get-byte))

            (bitmap (get-n-times rows (let-parses (
                  (* (get-imm #\"))
                  (row (get-n-times columns get-byte))
                  (* get-rest-of-line))
               row)))
      )
         (cons [columns rows colors 1] bitmap)))
))

(import (file xpm))

(define (syntax-fail pos info lst)
   (print-to stderr "parser fail: " info)
   (print-to stderr ">>> " pos); "-" (runes->string lst) " <<<")
   '(() (())))

; --------------------------
; skeletal-animation library
(define filename "sample.xpm")
(define xpm3 (xpm3-parser (file->list "sample.xpm")
   (lambda (data fail value pos) value)
   (lambda (pos reason) reason)
   0))

(define MAX 65536)  ; should be power of two
; size of game board (should be less than MAX)
(define WIDTH 170)
(define HEIGHT 96)

; helper function
(define (hash x y)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT)))
   (+ (* y MAX) x)))

; ---------------
(import (lib gl2))
(gl:set-window-title "Wireworld")

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho 0 WIDTH 0 HEIGHT 0 1)

(glPointSize (/ 854 WIDTH))

; generate random field
(gl:set-userdata
(fold (lambda (ff v y)
         (fold (lambda (ff v x)
                  (if (eq? v #\space) ff (put ff (hash x y) v)))
            ff v (iota (length v))))
   #empty (reverse (cdr xpm3)) (iota (length (cdr xpm3)))))

; main game loop
(gl:set-renderer (lambda (mouse)
(let ((generation (gl:get-userdata)))
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

   (gl:set-userdata
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

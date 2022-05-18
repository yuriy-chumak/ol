#!/usr/bin/env ol
; 7 colors for "wire"
; 7 light colors for "bright wire"

; first color item is an "not a wire"
; second color item is an "initially powered wire"

(define TTL 4) ; time to make wire powered

; simulation board is a matrix: [[]]
; we should have TWO empty lines before borders
(import (file xpm3))
(import (scheme dynamic-bindings))

(define file (xpm3-parse-file
   (or (and (pair? (command-line)) (car (command-line))) "sample.xpm")))
(define (print-board board)
   (for-each (lambda (row)
         (for-each (lambda (cell)
               (display (if cell (string cell) " ")))
            row)
         (print))
      board))
(define (rref board i j) (ref (ref board j) i))

(define board (file 'bitmap))
(define WIDTH (file 'width))
(define HEIGHT (file 'height))

; let's update board (remove first color from the board)
(define not-a-wire (ref (car (file 'color-table)) 1))
(define board (vector-map (lambda (row)
      (vector-map (lambda (cell)
            (if (not (eq? cell not-a-wire)) cell))
         row))
   board))

; step 1: find all closed circuits
; step 2: find all NOT gates and put into separate dictionary
; step 3: connect circuits to the games as "in" and "out" sockets

; 1. let's find all wires
(define wires (vector-map (lambda (row)
      (make-vector (size row))) board))

(define wires-count
   (fold (lambda (n j)
            (fold (lambda (n i)    ; if already assigned or not a wire
                     (if (or (rref wires i j) (not (rref board i j)))
                        n
                     else
                        ; well, we have found a new unassigned wire
                        (define N (++ n))
                        (print "found new wire at point " i " " j ", assinged number " N)

                        (let loop ((i i) (j j))
                           (if (or (rref wires i j) (not (rref board i j)))
                              ; check possible cross-wire connection (first LogicWire pattern)
                              (and (not (rref board i j)) ; empty center
                                 ; empty corners
                                   (not (rref board (- i 1) (- j 1)))
                                   (not (rref board (+ i 1) (- j 1)))
                                   (not (rref board (+ i 1) (+ j 1)))
                                   (not (rref board (- i 1) (+ j 1)))
                                 ; wired sides
                                   (rref board (- i 1) j)
                                   (rref board (+ i 1) j)
                                   (rref board i (- j 1))
                                   (rref board i (+ j 1)))
                           else
                              ; just directly connected wires
                              (set-ref! (ref wires j) i N)
                              (for-each (lambda (di dj)
                                    (if (loop (+ i di) (+ j dj))
                                       ; if there are cross-wire connection found
                                       (loop (+ i (* di 2)) (+ j (* dj 2)))))
                                 '(-1  0 +1  0)
                                 '( 0 -1  0 +1)))) ; #false
                        ; 2. check the cross-wire connection
                        N))
               n (iota WIDTH 1)))
      0 (iota HEIGHT 1)))
(print "wires count: " wires-count)

;; ; print wires:
;; (vector-for-each (lambda (row)
;;       (vector-for-each (lambda (cell)
;;             (cond
;;                ((not cell) (display "  "))
;;                ((< cell 10)(display " ")(display cell))
;;                (else
;;                   (display cell)))
;;             (display " "))
;;          row)
;;       (print))
;;    wires)

; step 2: find all NOT gates
; list of [from to]
; we don't need to store gate coordinates, just a way
(define gates
   (define (wired-cells i j)
      (fold (lambda (s di dj)
               (+ s (if (rref board (+ i di) (+ j dj)) 1 0)))
         0
         '(-1 -1 -1  0   0 +1 +1 +1)
         '(-1  0 +1 -1  +1 -1  0 +1)))
(fold (lambda (ff j)
      (fold (lambda (ff i)
            ; totally 4 gates with different rotation
            (if (rref board i j) ; if central not empty, just speedup
               ff
            else
               (cond
                  ; down->up
                  ((and (not (rref board (- i 1) (- j 1)))
                        (not (rref board (+ i 1) (- j 1)))
                        (eq? (wired-cells i j) 6))
                                       ;[from to]
                     (cons [(rref wires i (+ j 1))
                            (rref wires i (- j 1))] ff))
                  ; up->down
                  ((and (not (rref board (- i 1) (+ j 1)))
                        (not (rref board (+ i 1) (+ j 1)))
                        (eq? (wired-cells i j) 6))
                                       ;[from to]
                     (cons [(rref wires i (- j 1))
                            (rref wires i (+ j 1))] ff))
                  ; left->right
                  ((and (not (rref board (+ i 1) (- j 1)))
                        (not (rref board (+ i 1) (+ j 1)))
                        (eq? (wired-cells i j) 6))
                                       ;[from to]
                     (cons [(rref wires (- i 1) j)
                            (rref wires (+ i 1) j)] ff))
                  ; right->left
                  ((and (not (rref board (- i 1) (- j 1)))
                        (not (rref board (- i 1) (+ j 1)))
                        (eq? (wired-cells i j) 6))
                                       ;[from to]
                     (cons [(rref wires (+ i 1) j)
                            (rref wires (- i 1) j)] ff))
                  (else
                     ff))))
         ff
         (iota WIDTH 1)))
      '()
   (iota HEIGHT 1)))

(print "gates: " gates)


; current wires state: powered (true) or not (false)
(define wire-states (fold (lambda (f i)
      (put f i #f)) ; 
   {}
   (iota wires-count 1)))
; power all "second color" wires (for TTL cycles)
(define power-wire (ref (lref (file 'color-table) 1) 1))
(for-each (lambda (j)
      (for-each (lambda (i)
            (if (eq? (rref board i j) power-wire)
               (put! wire-states (rref wires i j) TTL)))
         (iota WIDTH 1)))
   (iota HEIGHT 1))
(print wire-states)

; let's draw wires
(import (lib gl-2))
(gl:set-window-title "LogicWire")
(gl:set-window-size (* WIDTH 3) (* HEIGHT 3))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho 0 WIDTH HEIGHT 0 0 10) ; inverse Y axis

(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height)
   (glPointSize (/ width WIDTH))))

(define color-table (fold (lambda (ff array)
      (define index (ref array 1))
      (define color (list->string (ref array 3)))
      (put ff index (cond
         ((string-eq? color "black")  '(  0   0   0))
         ((string-eq? color "red")    '(205  49  49))
         ((string-eq? color "green")  '( 13 188 121))
         ((string-eq? color "yellow") '(229 229  16))
         ((string-eq? color "blue")   '( 36 114 200))
         ((string-eq? color "magenta")'(188  63 188))
         ((string-eq? color "cyan")   '( 17 168 205))
         ((string-eq? color "white")  '(229 229 229))
         ((string-eq? color "None")   '( 17  17  17)))))
   {}
   (file 'color-table)))

(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   ; draw the cells
   (glColor3f 1 1 0.2)
   (glBegin GL_POINTS)
      (for-each (lambda (j)
            (for-each (lambda (i)
                  (define cell (rref board i j))
                  (define wire (rref wires i j))
                  (when wire
                     (define color (color-table cell '(0 0 0)))
                     (if (wire-states wire #f)
                        (glColor3bv (map (lambda (c) (/ c 2)) color))
                        (glColor3bv (map (lambda (c) (/ c 4)) color)))
                     (glVertex2f i j)))
               (iota WIDTH 1)))
         (iota HEIGHT 1))
   (glEnd)
   (mail 'simulation #false)
))

(gl:set-mouse-handler (lambda args
   (mail 'simulation args)))

; helper function
(define (natural? x)
   (and (integer? x) (> x 0)))

; simulation loop
; 1. process all gates, invert input signals, collect to the output wires
; 2. process all wires, makes it "powered" if at least one "1" signal provided
; 3. goto to loop
;(async (lambda ()
(actor 'simulation (lambda ()
   (let loop ()
      (let*((envelope (wait-mail))
            (sender msg envelope))
      (if msg
         ; process manual change of wire state
         (apply (lambda (button x y)
               (define X (+ (div (* (- x 3) WIDTH) (gl:get-window-width)) 1))
               (define Y (+ (div (* (- y 3) HEIGHT) (gl:get-window-height)) 1))
               (define wire (rref wires X Y))
               (when wire
                  (define state (wire-states wire #f))
                  (print "select wire " wire " < " state)
                  (if (natural? state)
                     (put! wire-states wire #false)
                  else
                     (put! wire-states wire TTL)))) ; TODO?: change (one click - +TTL timer state)
            msg)
      ; calculate new signals
      else
         (define signals
            (fold (lambda (ff gate)
                     (define from (ref gate 1))
                     (define to (ref gate 2))
                     (if (not (wire-states from #f))
                        (put ff to #true)
                     else
                        ff))
               {}
               gates))
         ; update wires
         (for-each (lambda (wire)
               (define state (wire-states wire #f))
               (if (natural? state) ; wire manually powered
                  (put! wire-states wire (- state 1))
               else
                  (put! wire-states wire (signals wire #false))))
            (keys wire-states))))
      (loop))))

(print "ok")

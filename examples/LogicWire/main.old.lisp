#!/usr/bin/env ol
; 7 colors for "wire"
; 7 light colors for "bright wire"

; first color item is an "not a wire"
; second color item is an "initially powered wire"

(define TTL 5) ; time to make wire powered
(define INITIAL-SCALE 8) ; circuit scaling
;(define RELAX-TIME 15) ; time to start sim

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

(display "Loading wires... ")
(define wires-count
   (fold (lambda (n j)
            (fold (lambda (n i)    ; if already assigned or not a wire
                     (if (or (rref wires i j) (not (rref board i j)))
                        n
                     else
                        ; well, we have found a new unassigned wire
                        (define N (++ n))
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
(print "loaded " wires-count)

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
(display "Loading gates... ")
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
(print "loaded " (length gates))

; current wires state: powered (true) or not (false)
(define wire-states (fold (lambda (f i)
      (put f i #f)) ; 
   {}
   (iota wires-count 1)))
; power all red wires
(define power-wire (ref (lref (file 'color-table) 1) 1))
(for-each (lambda (j)
      (for-each (lambda (i)
            (if (eq? (rref board i j) power-wire)
               (put! wire-states (rref wires i j) TTL)))
         (iota WIDTH 1)))
   (iota HEIGHT 1))

; ---------------------------------------------------------------------------------------
; simulation loop
; 1. process all gates, invert input signals, collect to the output wires
; 2. process all wires, makes it "powered" if at least one "1" signal provided
; 3. goto to loop
;(async (lambda ()
(actor 'simulation (lambda ()
   ; helper function
   (define (natural? x)
      (and (integer? x) (> x 0)))
   ; main simulation loop
   (let loop ()
      (let*((envelope (wait-mail))
            (sender msg envelope))
      (if msg
         ; process manual change of wire state
         (apply (lambda (x y)
               (define wire (rref wires x y))
               (when wire
                  (define state (wire-states wire #f))
                  (print "select wire " wire " < " state)
                  (if state
                     (put! wire-states wire #false)
                  else
                     (put! wire-states wire TTL))))
            msg)
      ; calculate new signals
      else
         ;(sleep 10000)
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
            (keys wire-states))
         (mail sender 'ok)))
      (loop))))

; 

; power all "second color" wires (for TTL cycles)
;; (define power-wire (ref (lref (file 'color-table) 1) 1))
;; (for-each (lambda (j)
;;       (for-each (lambda (i)
;;             (if (eq? (rref board i j) power-wire)
;;                (put! wire-states (rref wires i j) TTL)))
;;          (iota WIDTH 1)))
;;    (iota HEIGHT 1))


; ---------------------------------------------------------------------------------------
; let's draw wires

; preconfigure window size
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (pairs->ff `(
      (width . 840)
      (height . 720))))))

(import (lib gl-2))
(gl:set-window-title "LogicWire")
(import (lib soil))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(define (/2 x) (/ x 2))
(define color-table (fold (lambda (ff array)
      (define index (ref array 1))
      (define color (list->string (ref array 3)))
      (put ff index (cond                ; /2 is to signed-int
         ((string-eq? color "black")  (map /2 '(  0   0   0)))
         ((string-eq? color "red")    (map /2 '(205  49  49)))
         ((string-eq? color "green")  (map /2 '( 13 188 121)))
         ((string-eq? color "yellow") (map /2 '(229 229  16)))
         ((string-eq? color "blue")   (map /2 '( 36 114 200)))
         ((string-eq? color "magenta")(map /2 '(188  63 188)))
         ((string-eq? color "cyan")   (map /2 '( 17 168 205)))
         ((string-eq? color "white")  (map /2 '(229 229 229)))
         ((string-eq? color "orange") (map /2 '(255 160   0)))
         ((string-eq? color "None")   (map /2 '( 17  17  17)))
         (else '(255 255 255)))))
   {}
   (file 'color-table)))

(define X0 (make-parameter 0)); 440))
(define Y0 (make-parameter 0)); 120))

(define GO '(#true))
(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height) ))

(define SCALE (make-parameter INITIAL-SCALE))

(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (define-values (x0 y0) (values (X0) (Y0)))
   (define width (gl:get-window-width))
   (define height (gl:get-window-height))
   (glLoadIdentity)
   (define scale (SCALE))              ; inverse Y axis
   (glOrtho x0 (+ (/ width scale) x0)  (+ (/ height scale) y0) y0  0 1)
   (glPointSize scale)

   ; draw the circuit borders
   (glColor3f 1 1 0.2)
   (glLineWidth 2.0)
   (glBegin GL_LINE_LOOP)
      (glVertex2f 0 0)
      (glVertex2f WIDTH 0)
      (glVertex2f WIDTH HEIGHT)
      (glVertex2f 0 HEIGHT)
   (glEnd)

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
                        (glColor3bv color)
                        (glColor3bv (map /2 color)))
                     (glVertex2f i j)))
               (iota (div width scale) x0))) ; opt: draw only visible cells
         (iota (div height scale) y0)) ; opt: draw only visible cells
   (glEnd)
   ;; (print "SOIL_save_screenshot: " SOIL_save_screenshot)
   ;; (SOIL_save_screenshot "out1.png" SOIL_SAVE_TYPE_TGA 0 0 width height)
   ;; (exit 1)
   (if (car GO)
      (await (mail 'simulation #false)))
))

(gl:set-mouse-handler (lambda args
   (case (ref args 1)
      (1
         (define scale (SCALE))
         (define X (+ (div (+ (lref args 1) (div scale 2) -1) scale) (X0)))
         (define Y (+ (div (+ (lref args 2) (div scale 2) -1) scale) (Y0)))
         (mail 'simulation (list X Y)))
      (2
         (fasl-save wire-states "wire-states.fasl"))
      (3
         (await (mail 'simulation #false)))
      (4 ; scroll up)
         (Y0 (- (Y0) 10)))
      (5 ; scroll down)
         (Y0 (+ (Y0) 10))) )))

(import (lib keyboard))
(gl:set-keyboard-handler (lambda (key)
   (print key)
   (case key
      (KEY_ESC ; esc
         (exit #t))
      (113 ; q
         (exit #t))

      (#xffad ; minus key
         (SCALE (max (- (SCALE) 2) 1)))
      (#xffab ; plus key
         (SCALE (min (+ (SCALE) 2) 9)))

      (97 ; home
         (X0 0) (Y0 0))

      ((33 42) ; Pause/Play, Go
         (set-car! GO (not (car GO))))
      (39 ; Step
         (await (mail 'simulation #false)))

      (102 ; right arrow
         (X0 (+ (X0) 10)))
      (100 ; left arrow
         (X0 (- (X0) 10)))
      (98 ; up arrow
         (Y0 (- (Y0) 10)))
      (104 ; down arrow
         (Y0 (+ (Y0) 10))) )))

; show Help
(print "
Usage:
  LMB: power wire for TTL ticks,
  RMB: step
  Scroll Up: scroll up
  Scroll Down: scroll down

  Home: reset position
  Right Arrow: scroll right
  Left Arrow: scroll left
  Up Arrow: scroll up
  Down Arrow: scroll down

  P: pause/play
  G: pause/play
  -: scale down
  +: scale up

  Esc: exit
")

(print "ok")

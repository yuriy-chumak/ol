#!/usr/bin/env ol
; first color item is a "not a wire" (typically 'black')
; second color item is an "initially powered wire" (typically 'red')

(define TTL 5) ; time to make wire powered
(define INITIAL-SCALE 1) ; circuit scaling
(define RELAX-TIME 15) ; time to start sim (not used)

; simulation board is a matrix: [[]]
; we should have TWO empty lines before borders
(import (file xpm3))
(import (scheme dynamic-bindings))

(define file (xpm3-parse-file
   (or (and (pair? (command-line)) (car (command-line))) "8bit_cpu.xpm")))
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

; 1. let's find all wires,
;    check the cross-wire connections
(define wires (vector-map (lambda (row)
      (make-vector (size row))) board))

(display "Loading wires... ")
(define wires-count
   (fold (lambda (n j)
            (fold (lambda (n i)   ; if already assigned or not a wire
                     (if (or (rref wires i j) (not (rref board i j)))
                        n
                     else
                        ; well, we have found a new unassigned wire
                        (define N (++ n))
                        (let loop ((i i) (j j))
                           (if (or (rref wires i j) (not (rref board i j)))
                              ; check possible cross-wire connection (first LogicWire pattern)
                              (and (not (rref board i j))  ; empty center
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
                        N))
               n (iota WIDTH)))
      0 (iota HEIGHT)))
(print "loaded " wires-count)

; step 2: find all NOT gates
;  -> list of [from to]
;  we don't need to store gate coordinates, just a way
(display "Loading gates... ")
(define gates
   (define (wired-cells i j)
      (fold (lambda (s di dj)
               (+ s (if (rref board (+ i di) (+ j dj)) 1 0)))
         0
         '(-1 -1 -1  0   0 +1 +1 +1)
         '(-1  0 +1 -1  +1 -1  0 +1)))

   (fold (lambda (out j)
         (fold (lambda (out i)
               (or
                  ; totally 4 gates with different rotation
                  (if (not (rref board i j)) ; speedup, (if central not empty)
                     (if (eq? (wired-cells i j) 6)
                        (cond
                           ; down->up -> [from to]
                           ((and (not (rref board (- i 1) (- j 1)))
                                 (not (rref board (+ i 1) (- j 1))))
                              (cons [(rref wires i (+ j 1))
                                    (rref wires i (- j 1))] out))
                           ; up->down
                           ((and (not (rref board (- i 1) (+ j 1)))
                                 (not (rref board (+ i 1) (+ j 1))))
                              (cons [(rref wires i (- j 1))
                                    (rref wires i (+ j 1))] out))
                           ; left->right
                           ((and (not (rref board (+ i 1) (- j 1)))
                                 (not (rref board (+ i 1) (+ j 1))))
                              (cons [(rref wires (- i 1) j)
                                    (rref wires (+ i 1) j)] out))
                           ; right->left
                           ((and (not (rref board (- i 1) (- j 1)))
                                 (not (rref board (- i 1) (+ j 1))))
                              (cons [(rref wires (+ i 1) j)
                                    (rref wires (- i 1) j)] out)))))
                        out))
            out
            (iota WIDTH)))
         '()
      (iota HEIGHT)))
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
      (if (pair? msg)
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

         (if (function? msg)
            (msg)))

         (mail sender 'ok))
      (loop))))

; ---------------------------------------------------------------------------------------
; let's draw wires

(import (lib gl-2))
(gl:set-window-title "LogicWire")
(gl:set-window-size WIDTH HEIGHT)
(import (lib soil))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

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
         ((string-eq? color "orange") '(255 160   0))
         ((string-eq? color "None")   '( 17  17  17))
         (else '(255 255 255)))))
   {}
   (file 'color-table)))

(define X0 (make-parameter 0)); 440))
(define Y0 (make-parameter 0)); 120))

(define GO '(#false))
(gl:set-resize-handler (lambda (width height)
   (glViewport 0 0 width height) ))

(define SCALE (make-parameter INITIAL-SCALE))

; let's speedup rendering
; --------------------------------------------------------------
; we do not change the topology, so let's use texture for colors

; 2-dimensional color texture of the board
(define board-id (box 0))
(glGenTextures 1 board-id)
(glBindTexture GL_TEXTURE_2D (unbox board-id))

(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)

(define data (make-bytevector (* (+ WIDTH 1) (+ HEIGHT 1) 4)))
(for-each (lambda (j)
      (for-each (lambda (i p)
            (define cell (rref board i j))
            (define wire (rref wires i j))

            (define color (color-table cell '(0 0 0)))
            (set-ref! data    p    (car color))
            (set-ref! data (+ p 1) (cadr color))
            (set-ref! data (+ p 2) (caddr color))
            (set-ref! data (+ p 3) 1))
         (iota WIDTH 0)
         (iota WIDTH (* j WIDTH 4) 4)))
   (iota HEIGHT 0))

(glTexImage2D GL_TEXTURE_2D 0 GL_RGB WIDTH HEIGHT 0 GL_RGBA GL_UNSIGNED_BYTE data)
(glBindTexture GL_TEXTURE_2D 0)
(define data #false) ; cleanup

; 1-dimentional per-pixel texture
; wires power texture:
(define power (make-bytevector (+ wires-count 1))) ; "0" is reserved

(define atlas-id (box 0))
(glGenTextures 1 atlas-id)
(glBindTexture GL_TEXTURE_1D (unbox atlas-id))

(glTexParameteri GL_TEXTURE_1D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_1D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
(glTexParameteri GL_TEXTURE_1D GL_TEXTURE_MAG_FILTER GL_NEAREST)
(glTexParameteri GL_TEXTURE_1D GL_TEXTURE_MIN_FILTER GL_NEAREST)
(glTexImage1D GL_TEXTURE_1D 0 GL_RGB (+ wires-count 1) 0 GL_RGB GL_UNSIGNED_BYTE NULL)
(glBindTexture GL_TEXTURE_1D 0)

(define (update-wires-texture)
   ; update wires texture
   (glBindTexture GL_TEXTURE_1D (unbox atlas-id))
   (for-each (lambda (wire)
         (if (wire-states wire #f)
            (set-ref! power wire 227)
         else
            (set-ref! power wire 100)))
      (iota wires-count 1))
   (glTexImage1D GL_TEXTURE_1D 0 GL_LUMINANCE (+ wires-count 1) 0 GL_LUMINANCE GL_UNSIGNED_BYTE power))

; -----------------------------
(define topology (glGenLists 1))

; generate wires topology:
(glNewList topology GL_COMPILE)
(glBegin GL_POINTS)
(begin
   (for-each (lambda (j)
         (for-each (lambda (i)
               (define cell (rref board i j))
               (define wire (rref wires i j))
               (when wire
                  (glTexCoord1f (/ wire wires-count))
                  (glVertex2f i j)))
            (iota WIDTH 0)))
      (iota HEIGHT 0)))
(glEnd)
(glEndList)

(define sequence '(100000))

(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (define-values (x0 y0) (values (X0) (Y0)))
   (define width (gl:get-window-width))
   (define height (gl:get-window-height))
   (glLoadIdentity)
   (define scale (SCALE))              ; inverse Y axis
   (glOrtho x0 (+ (/ width scale) x0)  (+ (/ height scale) y0) y0  0 1)
   (glPointSize scale)

   ; draw the board
   (glColor3f 1 1 1)
   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D (unbox board-id))
   (glBegin GL_QUADS)
   (begin
      (define W (- WIDTH 0.48))
      (define H (- HEIGHT 0.51))
      (define O -0.51)
      (define X -0.48)
      (for-each (lambda (x y s t)
            (glTexCoord2f s t)
            (glVertex2f x y))
         (list X W W X)
         (list O O H H)
         (list 0 1 1 0)
         (list 0 0 1 1)) )
   (glEnd)
   (glDisable GL_TEXTURE_2D)

   ; draw the cells
   (glColor3f 1 1 1)
   (glEnable GL_TEXTURE_1D)
   (glEnable GL_BLEND)
   (glBlendFunc GL_ZERO GL_SRC_COLOR) ; GL_SRC_ALPHA

   (glCallList topology)
   (glDisable GL_TEXTURE_1D)
   (glDisable GL_BLEND)

   ; draw the circuit borders
   (glColor3f 1 1 0.2)
   (glLineWidth 2.0)
   (glBegin GL_LINE_LOOP)
      (glVertex2f 0 0)
      (glVertex2f WIDTH 0)
      (glVertex2f WIDTH HEIGHT)
      (glVertex2f 0 HEIGHT)
   (glEnd)

   ;; (syscall 1017 (string-append "/usr/bin/convert"
   ;;    " "
   ;;    "screenshots/" (number->string (car sequence)) ".tga"
   ;;    " "
   ;;    "screenshots/" (number->string (car sequence)) ".png"
   ;;    "; "
   ;;    "rm -f "
   ;;    "screenshots/" (number->string (car sequence)) ".tga"
   ;; ))

   (when (car GO)
      (set-car! sequence (+ (car sequence) 1))
      ;; (SOIL_save_screenshot (string-append "screenshots/" (number->string (car sequence)) ".tga") SOIL_SAVE_TYPE_TGA 0 0 width height)

      (await (mail 'simulation update-wires-texture)))
))

(gl:set-mouse-handler (lambda args
   (case (ref args 1)
      (1
         (define scale (SCALE))
         (define X (+ (div (+ (lref args 1) (div scale 2) -1) scale) (X0)))
         (define Y (+ (div (+ (lref args 2) (div scale 2) -1) scale) (Y0)))
         (await (mail 'simulation (list X Y)))
         (update-wires-texture))
      (2
         (fasl-save wire-states "wire-states.fasl"))
      (3
         (await (mail 'simulation update-wires-texture)))
      (4 ; scroll up
         (Y0 (- (Y0) 10)))
      (5 ; scroll down
         (Y0 (+ (Y0) 10))) )))

(import (lib keyboard))
(gl:set-keyboard-handler (lambda (key)
   ;; (print (string-append "0x" (number->string key 16)))
   (case key
      (KEY_ESC
         (exit #t))

      (#xffad ; minus key
         (SCALE (max (- (SCALE) 1) 1)))
      (#xffab ; plus key
         (SCALE (min (+ (SCALE) 1) 9)))

      (KEY_HOME ; home
         (X0 0) (Y0 0))

      (KEY_P ; Pause/Play, Go
         (set-car! GO (not (car GO))))
      (KEY_G ; Pause/Play, Go
         (set-car! GO (not (car GO))))
      (KEY_S ; Step
         (await (mail 'simulation update-wires-texture)))

      (KEY_RIGHT ; right arrow
         (X0 (+ (X0) 10)))
      (KEY_LEFT ; left arrow
         (X0 (- (X0) 10)))
      (KEY_UP ; up arrow
         (Y0 (- (Y0) 10)))
      (KEY_DOWN ; down arrow
         (Y0 (+ (Y0) 10))) )))

; show Help
;; (print "
;; Usage:
;;   LMB: power wire for TTL ticks,
;;   RMB: step
;;   Scroll Up: scroll up
;;   Scroll Down: scroll down

;;   Home: reset position
;;   Right Arrow: scroll right
;;   Left Arrow: scroll left
;;   Up Arrow: scroll up
;;   Down Arrow: scroll down

;;   P: pause/play
;;   G: pause/play
;;   -: scale down
;;   +: scale up

;;   Esc: exit
;; ")

(print "ok")

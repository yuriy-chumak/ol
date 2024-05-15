#!/usr/bin/env ol
(import (lib gl) (otus random!))
(import (OpenGL 1.1))

(define WIDTH  (/ 85 1)) ;(floor (/ 854 GLYPH_WIDTH))) ; /14 = 45.7
(define HEIGHT (/ 56 1)) ;(floor (/ 480 GLYPH_HEIGHT))) ;/16 = 30

(define NGLYPHS 28)           ; constant
(define SWITCH_FADE  0)        ; Затемненный иероглиф
(define SWITCH_PLAIN 1)        ; Обычный иероглиф
(define SWITCH_GLOW  2)        ; Подсвеченный иероглиф
(define SLIDING-MODE 1)        ; 0 - без слайда, 1 - частичный слад, 2 - только слайд

(define PHOSPHOR-ENABLED #true)
(define RANDGLOW-ENABLED #true)

;(define DEFAULT-DENCITY 20)
;(define DEFAULT-GLOWRATE 10)

; defaults
(define config {
   'density  20
   'glowrate 10
})


(define (ne? x y)
   (not (eq? x y)))

(define ith vector-ref)

(define (create-scalar value)
   (cons value null))
(define (create-vector size)
   (make-vector size 0))
(define (create-matrix size-x size-y)
   (vector-map (lambda (_)
                  (create-vector size-x))
      (make-vector size-y)))

(define (get-value scalar)
   (car scalar))
(define (get-vector-value vector i)
   (ith vector i))
(define (get-matrix-value matrix i j)
   (ith (ith matrix j) i))

(define (set-matrix-value matrix i j value)
   (set-ref! (ith matrix j) (+ i 1) value))


(define setx! (case-lambda
   ((matrix i j value)
      (set-ref! (ith matrix j) (+ i 1) value))
   ((vector i value)
      (set-ref! vector (+ i 1) value))
   ((scalar value)
      (set-car! scalar value))))


; ===========================================================
;matrix():

(define glyphs (create-matrix WIDTH HEIGHT))
(define glows  (create-matrix WIDTH HEIGHT))

(define cells {
;   (cons 'glyph     (create-matrix WIDTH HEIGHT))
;   (cons 'glow      (create-matrix WIDTH HEIGHT))
   'spinner   (create-matrix WIDTH HEIGHT) ; 1/0
})

(define feeders {
   'y (create-vector WIDTH)
})
(define remainings (create-vector WIDTH))
(define throttles (create-vector WIDTH))

(define spinners {
   'x (create-vector 101)
   'y (create-vector 101)
})

(define density (create-scalar (getf config 'density)))


; spinners:
(define (create_spinner i)
(let ((x (rand! WIDTH))
      (y (rand! HEIGHT)))
   (setx! (spinners 'x) i x)
   (setx! (spinners 'y) i y)
   (setx! (cells 'spinner) x y 1)))

(define (clear_spinner i)
(let ((x (get-vector-value (spinners 'x) i))
      (y (get-vector-value (spinners 'y) i)))
   (setx! (cells 'spinner) x y  0)))


(define spinners_length (create-scalar 0))
(define spinners_new_length (create-scalar 20)) ; config.spinners, may change

(define (densitizer density)
   density)
;   (cond
;      ((< density 10) 85)
;      ((< density 15) 60)
;      ((< density 20) 45)
;      ((< density 25) 25)
;      ((< density 30) 20)
;      ((< density 35) 15)
;      ((< density 45) 10)
;      ((< density 50)  8)
;      ((< density 55)  7)
;      ((< density 65)  5)
;      ((< density 80)  3)
;      ((< density 90)  2)
;      (else 1)))

;(runtime-error "debug-exit")

;
(define (insert_glyph2 glyph x y slide)
;(if (< y HEIGHT)
(let ((bottom_feeder_p (>= y 0)))
(let ((y (if bottom_feeder_p
            y
         else
            (let loop ((y (- HEIGHT 1)))
               (if (eq? y 0)
                  0
               else
                  (if (and
                        PHOSPHOR-ENABLED
                        (ne? (get-matrix-value (cells 'glyph) x y) 0)
                        (eq? (get-matrix-value (cells 'glyph) x (- y 1)) 0))
                     (setx! (cells 'glow) x y -1)
                  else
                     (setx! (cells 'glow ) x y (get-matrix-value (cells 'glow ) x (- y 1)))
                     (setx! (cells 'glyph) x y (get-matrix-value (cells 'glyph) x (- y 1))))
                  (loop (- y 1)))))))

   (setx! (cells 'glyph) x y glyph)

   (if (eq? glyph 0)
      (if bottom_feeder_p
         (setx! (cells 'glow ) x y (+ 1 (rand! 2)))
         (setx! (cells 'glow ) x y 0))))))

(define (insert_glyph glyph x y)
(if (< y HEIGHT)
(let ((bottom_feeder_p (>= y 0)))
(let ((y (if bottom_feeder_p
            y
         else
            (let loop ((y (- HEIGHT 1)))
               (if (eq? y 0)
                  0
               else
                  (if (and
                        PHOSPHOR-ENABLED
                        (ne? (get-matrix-value glyphs x y) 0)
                        (eq? (get-matrix-value glyphs x (- y 1)) 0))
                     (setx! glows x y -1)
                  else
                     (setx! glows x y (get-matrix-value glows x (- y 1)))
                     (setx! glyphs x y (get-matrix-value glyphs x (- y 1))))
                  (loop (- y 1)))))))

   (setx! glyphs x y glyph)

   (if (eq? glyph 0)
      (if bottom_feeder_p
         (setx! glows x y (+ 1 (rand! 2)))
         (setx! glows x y 0)))))))


(gl:set-window-title "Digital Rain")

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0 0 0 1.0)

(glEnable GL_TEXTURE_2D)
(glBindTexture GL_TEXTURE_2D 0)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGB8
   42 448
   0 GL_RGB GL_UNSIGNED_BYTE (file->bytevector "matrix.rgb"))

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(glScalef -1 -1 1)
(glOrtho 0 WIDTH 0 HEIGHT 0 1)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)

; draw
(gl:set-renderer (lambda ()

   ; update:
   (when #true  ;(not (= oldtime time))

   ; feed matrix
   (when #true
   (let loop ((x 0))
      (if (< x WIDTH)
         (let ((throttle  (get-vector-value throttles x))
               (remaining (get-vector-value remainings x))
               (y         (get-vector-value (feeders 'y) x)))

            (cond
               ((> throttle 0)
                  (setx! throttles x (- throttle 1)))
               ((> remaining 0)
                  (insert_glyph (+ (rand! NGLYPHS) 1) x y)
                  (setx! remainings x (- remaining 1))
                  (if (>= y 0)
                     (setx! (feeders 'y) x (+ y 1))))
               (else
                  (insert_glyph 0 x y)
                  (if (>= y 0)
                     (setx! (feeders 'y) x (+ y 1)))))
            (if (eq? (rand! 10) 0)
               (setx! throttles x (+ (rand! 5) (rand! 5))))

            (loop (+ x 1)))))
   )

   ; hack matrix:

   ;; implemented glow rate here -- just an arbitary value to multiply by
   (if RANDGLOW-ENABLED
      (let loop ((i (rand! (floor (/ (* (getf config 'glowrate) (/ WIDTH 2)) 10)))))
         (if (> i 0)
            (let ((y (rand! HEIGHT))
                  (x (rand! WIDTH)))
               (if (and
                     (ne? (get-matrix-value glyphs x y) 0)
                     (eq? (get-matrix-value glows x y) 0))
                  (set-matrix-value glows x y (rand! 20)))
               (loop (- i 1))))))

   ;; Change some of the feeders
   (when #true
   (let loop ((x 0))
      (if (< x WIDTH) (begin
         (if (and
               (eq? (get-vector-value remainings x) 0)
               (eq? (rand! (densitizer (get-value density)))       0))
            (begin
               (setx! remainings x (+ 3 (rand! HEIGHT)))
               (setx! throttles x (+ (rand! 5) (rand! 5)))
               (if (> (rand! 4) 0)
                  (setx! remainings x 0))

               (case SLIDING-MODE
                  (0 (setx! (feeders 'y) x  (rand! HEIGHT)))
                  (1 (setx! (feeders 'y) x  (if (eq? (rand! 2) 0) -1 (rand! HEIGHT))))
                  (2 (setx! (feeders 'y) x -1)))))
         (loop (+ x 1)))))
   )

   ; скорость обновления спиннеров - в 5 раз ниже матрицы
   ; спиннеры можно вынести в отдельный массив и рендерить поверх основной матрицы

   (when #true
   (when (eq? (rand! 50) 0)
      ; update spinners
      (cond
         ((> (get-value spinners_new_length) (get-value spinners_length))
            (setx! spinners_length (+ (get-value spinners_length) 1))
            (create_spinner (get-value spinners_length)))
         ((< (get-value spinners_new_length) (get-value spinners_length))
            (setx! spinners_length (- (get-value spinners_length) 1))
            (clear_spinner  (get-value spinners_length))))

      (if (ne? (get-value spinners_length) 0)
         (let ((i (rand! (get-value spinners_length))))
            (clear_spinner i)
            (create_spinner i))))
   )
   )

   ; renderer:
   (glClear GL_COLOR_BUFFER_BIT)
   (glColor3f 1 1 1)
   (glBindTexture GL_TEXTURE_2D 0)

   (glBegin GL_QUADS)
   ; Let's draw the matrix!
   (for-each (lambda (y)
      (for-each (lambda (x)
         (let ((glow    (get-matrix-value glows  x y))
               (glyph   (get-matrix-value glyphs x y))
               (spinner (get-matrix-value (cells 'spinner) x y)))

         (let ((u (/ (cond
                        ((> spinner 0) SWITCH_GLOW)
                        ((less? 0 glow) SWITCH_GLOW)
                        ((less? glow 0) SWITCH_FADE)
                        (else SWITCH_PLAIN)) 3))
               (v (/ glyph NGLYPHS)))

            (glTexCoord2f    u         v)
            (glVertex2f x y)
            (glTexCoord2f    u      (+ v 1/28))
            (glVertex2f x (+ y 1))
            (glTexCoord2f (+ u 1/3) (+ v 1/28))
            (glVertex2f (+ x 1) (+ y 1))
            (glTexCoord2f (+ u 1/3)    v)
            (glVertex2f (+ x 1) y))

            (when #true ;(not (= oldtime time)) (begin ...)
               (if (> glow 0)
                  (setx! glows x y (- glow 1)) ; cell->changed = 1;
               (if (< glow 0) (begin
                  (setx! glows x y (+ glow 1)) ; cell->changed = 1;
                  (if (eq? glow -1)
                     (setx! glyphs x y  0))))) ; cell->changed = 1;

               (if (> spinner 0)
                  (setx! glyphs x y (rand! NGLYPHS))))))
      (iota WIDTH)))
   (iota HEIGHT))
 ; cell->changed = 1;

   (glEnd)))

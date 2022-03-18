#!/usr/bin/env ol

(import (lib gl-2))
(import (scheme inexact))

(gl:set-window-title "Convey's The game of Life on the Trefoil Knot")

(define (sqr x) (* x x))
(define (mul v n)
   (cond
      ((vector? v)
         (vector-map (lambda (q) (* q n)) v))
      ((list? v)
         (map (lambda (q) (* q n)) v))))

(define (div_ v n)
   (map (lambda (q) (/ q n)) v))

(define (normalize v)
   (define mag (sqrt (fold + 0 (map sqr v))))
   (div_ v mag))

(import (otus algebra vector))

(import (otus random!))
(import (scheme dynamic-bindings))

; read initial population
(import (owl parse))
(import (file xpm))

(define population (parse xpm-parser (file->bytestream
   (or
      (and (pair? (command-line)) (car (command-line)))
      "generator.xpm"))
   #f #f #f))

; the size of world
(define WIDTH  (population 'width))
(define HEIGHT (population 'height))

; the color palette in rgb ([0..1][0..1][0..1])
(define colors (vector-map (lambda (color) (map (lambda (v) (inexact (/ v 255))) color))
   '[(255 0 0) (255 90 0) (255 154 0) (255 206 0) (255 232 8)
   (255 232  18) (255 232  38) (255 232  58) (255 232  78) (255 232  98) (255 232 118) (255 232 138) (255 232 158)
   (255 232 178) (255 232 198) (255 232 218) (255 232 238) (255 232 258)
   ]))

(define (color age)
   (ref colors (min age (size colors))))



(define (hash x y)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT)))
   (+ (* y 65536) x)))

(define (alive gen key)
   (let ((x (mod key 65536))
         (y (div key 65536)))
   (let ((n (fold
               (lambda (st dx dy)
                  (+ st (if (gen (hash (+ x dx) (+ y dy)) #f) 1 0)))
               0
               '(-1  0 +1   -1  +1   -1  0 +1)
               '(-1 -1 -1    0   0   +1 +1 +1))))
      (if (eq? n 2)
         (gen (hash x y) #f)
      (if (eq? n 3)
         #true)))))


(define userdata (make-parameter
   (let ((initial population))
      (fold (lambda (ff row y)
               (fold (lambda (ff col x)
                        (if (eq? col #\space)
                           ff
                           (put ff (hash x y) 1)))
                  ff row (iota (initial 'width) (/ (- WIDTH (initial 'width)) 2))))
         #empty
         (initial 'bitmap)
         (iota (initial 'height) (/ (- HEIGHT (initial 'height)) 2))))))



(glShadeModel GL_SMOOTH)
(glClearColor 0.2 0.2 0.2 1)

(glEnable GL_DEPTH_TEST)
(glEnable GL_NORMALIZE)

(glEnable GL_LIGHTING)
(glLightModelf GL_LIGHT_MODEL_TWO_SIDE GL_TRUE)
(glEnable GL_LIGHT0)
(glLightfv GL_LIGHT0 GL_POSITION (list 10 0 0 1))

(glEnable GL_CULL_FACE)
(glCullFace GL_BACK)

(glEnable GL_COLOR_MATERIAL)
(glColorMaterial GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE)

(gl:set-renderer (lambda (mouse)
   (define generation (userdata))

   (define-values (ss ms) (clock))
   (define tick (/ (+ ss (/ ms 1000)) 10))

   (glClear (vm:ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 1/1000 1000)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt
      (* 10 (sin tick)) (* 10 (cos tick)) 10
      0 0 0
      0 0 1)

   (define delta (/ 6.28 WIDTH))

   ; rotation v over k by θ
   (define (point k v θ)
      (vector-map +
         (mul v (cos θ))
         (mul (cross-product k v) (sin θ))
         (mul k (* (dot-product k v) (- 1 (cos θ))))))

   (glBegin GL_QUADS)
   (for-each (lambda (t X)
         ; текущая точка на узле
         (define x (+ (sin t) (* +2 (sin (* 2 t)))))
         (define y (+ (cos t) (* -2 (cos (* 2 t)))))
         (define z (- (sin (* 3 t))))

         (define t2 (+ t delta))
         
         ; следующая точка на узле
         (define x2 (+ (sin t2) (* +2 (sin (* 2 t2)))))
         (define y2 (+ (cos t2) (* -2 (cos (* 2 t2)))))
         (define z2 (- (sin (* 3 t2))))

         (define t3 (+ t2 delta))

         ; следующая следующая точка на узле
         (define x3 (+ (sin t3) (* +2 (sin (* 2 t3)))))
         (define y3 (+ (cos t3) (* -2 (cos (* 2 t3)))))
         (define z3 (- (sin (* 3 t3))))

         (define scale 0.5)

         ; первый тангент вектор
         (define tangent (list->vector (normalize (list
            (- x2 x)
            (- y2 y)
            (- z2 z)))))

         ; первый битангент-вектор
         (define bitangent (list->vector (mul (normalize (vector->list
            (cross-product tangent [x y z]))) scale)))

         ; аналогично второй и третий
         (define tangent2 (list->vector (normalize (list
            (- x3 x2)
            (- y3 y2)
            (- z3 z2)))))

         ; первый битангент-вектор
         (define bitangent2 (list->vector (mul (normalize (vector->list
            (cross-product tangent2 [x2 y2 z2]))) scale)))

         (define d (/ 6.28 HEIGHT))
         (for-each (lambda (θ Y)
               (define key (hash X Y))
               (define value (generation key #false))
               (if value
                  (apply glColor3f (color value))
                  (glColor3f 0.5 0.5 0.5))

               (define v1 (point tangent bitangent θ))
               (define v2 (point tangent bitangent (+ θ d)))
               (define v3 (point tangent2 bitangent2 θ))
               (define v4 (point tangent2 bitangent2 (+ θ d)))

               (glVertex3f (+ x (ref v1 1))
                           (+ y (ref v1 2))
                           (+ z (ref v1 3)))
               (glVertex3f (+ x (ref v2 1))
                           (+ y (ref v2 2))
                           (+ z (ref v2 3)))
               (glVertex3f (+ x2 (ref v4 1))
                           (+ y2 (ref v4 2))
                           (+ z2 (ref v4 3)))
               (glVertex3f (+ x2 (ref v3 1))
                           (+ y2 (ref v3 2))
                           (+ z2 (ref v3 3))))
            (iota HEIGHT 0 d)
            (iota HEIGHT)))
      (iota WIDTH 0 delta)
      (iota WIDTH))
   (glEnd)

   (define new-generation
      (ff-union (lambda (a b) b)
         (ff-fold
            (lambda (st key value)
               (let ((x (mod key 65536))
                     (y (div key 65536)))
                  (fold
                     (lambda (st dx dy) (put st (hash (+ x dx) (+ y dy)) 0))
                     st
                     '(-1  0 +1  -1 +1  -1  0 +1)
                     '(-1 -1 -1   0  0  +1 +1 +1))))
            {} generation)
         generation)) ; save the current point age
   (userdata
      (ff-fold (lambda (st key value)
            (if (alive generation key)
               (put st key (+ value 1))
               st))
         #empty new-generation))
   #true
))
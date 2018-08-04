#!/usr/bin/ol
(import (lib opengl))
(import (otus random!))

(define WIDTH 128)  ;128
(define HEIGHT 96) ;96)

(define (hash x y)
   (let ((x (mod (+ x WIDTH) WIDTH))
         (y (mod (+ y HEIGHT) HEIGHT)))
   (+ (* y 65536) x)))
;(define (hash x y)
;   (+ (* y 64) x))

(define (alive gen x y)
   (let ((n
      (+ (get gen (hash (- x 1) (- y 1)) 0)
         (get gen (hash    x    (- y 1)) 0)
         (get gen (hash (+ x 1) (- y 1)) 0)
         (get gen (hash (- x 1)    y   ) 0)
         ;get gen (hash    x       y   ) 0)
         (get gen (hash (+ x 1)    y   ) 0)
         (get gen (hash (- x 1) (+ y 1)) 0)
         (get gen (hash    x    (+ y 1)) 0)
         (get gen (hash (+ x 1) (+ y 1)) 0))))
      (if (eq? n 2)
         (get gen (hash x y) #f)
      (if (eq? n 3)
         #true))))

(gl:set-window-title 
   "Convey's The game of Life")

   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)
   (glOrtho 0 WIDTH 0 HEIGHT 0 1)

;   (list
;   (list->ff (map (lambda (i) (let ((x (rand2! WIDTH)) (y (rand2! HEIGHT)))
;                                 (cons (hash x y) 1))) (iota 1200)))))

(gl:set-userdata
   (let ((initial (file->vector "initial.bmp")))
   (list->ff (map (lambda (p) (cons (hash (car p) (cdr p)) 1))
      (fold (lambda (st p)
         (let ((n (+ p #x436)))
         (if (eq? (vector-ref initial n) 0)
            (cons (cons (mod p 64) (div p 64)) st) st)))
      null
      (iota (- (size initial) #x436)))))))


(gl:set-renderer (lambda (generation)
   (glClear GL_COLOR_BUFFER_BIT)

;   (let ((min-x (ff-fold (lambda (st key value)
;                            (let ((x (mod key 1024)))
;                               (if (< x st) x st)))
;                         -1 generation))
;         (max-x (ff-fold (lambda (st key value)
;                            (let ((x (mod key 1024)))
;                               (if (> x st) x st)))
;                         +1 generation))
;         (min-y (ff-fold (lambda (st key value)
;                            (let ((y (div key 1024)))
;                               (if (< y st) y st)))
;                         -1 generation))
;         (max-y (ff-fold (lambda (st key value)
;                            (let ((y (div key 1024)))
;                               (if (> y st) y st)))
;                         +1 generation)))
;   (print min-x "-" max-x ":" min-y "-" max-y)
;   (glLoadIdentity)
;   (glOrtho min-x max-x min-y max-y 0 1))

   (glPointSize (/ 1280 WIDTH))
   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_POINTS)
      (ff-fold (lambda (st key value)
         (glVertex2f (mod key 65536)
                     (div key 65536))
      ) #f generation)
   (glEnd)

   (print "cells count: "
   (ff-fold (lambda (st key value)
               (+ st 1))
      0
      generation))


   (list ; generation)))
      (ff-fold (lambda (st key value)
         (let ((x (mod key 65536))
               (y (div key 65536)))
            (fold (lambda (st key)
                     (let ((x (car key))
                          (y (cdr key)))
                        (if (alive generation x y) (put st (hash x y) 1) st)))
               (if (alive generation x y) (put st (hash x y) 1) st) ; the cell
               (list (cons (- x 1) (- y 1)) ; possible cell neighbors
                     (cons    x    (- y 1))
                     (cons (+ x 1) (- y 1))
                     (cons (- x 1)    y   )
                     ;cons    x       y   )
                     (cons (+ x 1)    y   )
                     (cons (- x 1) (+ y 1))
                     (cons    x    (+ y 1))
                     (cons (+ x 1) (+ y 1))))))
         #empty generation))))

(gl:finish)
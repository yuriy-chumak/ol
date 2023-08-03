#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3))

; --= mnist data =----------
(define mnist-root "./")
(print "<run `make` to download mnist handwritten digits dataset>");

; neural topology setup:
; --------------------------------------------------------
(define INPUT-LEN 784) ; every input image has 28 * 28 pixels,
(define OUTPUT-LEN 10) ; and only 10 possible numbers (0 .. 9)

(define LAYER1-LEN 64) ; the size of an intermediate layer

; neural math functions:
(import (srfi 27)) ; randomizer
(define (new-matrix rows columns)
   (list->vector (map (lambda (_)
         (list->vector (map (lambda (_)
               (- (random-real) #i0.5))
            (iota columns))))
      (iota rows))))

(import (scheme inexact))
(define (sigmoid x)
   (/ #i1 (+ #i1 (exp (negate x)))))
(define (sigmoid/ sx) ; sx == sigmoid(x)
   (* sx (- 1 sx)))

; neurons
(import (scheme dynamic-bindings))
(define neuron1 (make-parameter (or
   (fasl-load "neuron1.fasl" #f)
   (new-matrix INPUT-LEN  LAYER1-LEN))))
(define neuron2 (make-parameter (or
   (fasl-load "neuron2.fasl" #f)
   (new-matrix LAYER1-LEN OUTPUT-LEN))))

; matrix * matrix
(define (matrix-product A B)
   (define m (size A))
   (define n (size (ref A 1)))
   (assert (eq? (size B) n) ===> #true)
   (define q (size (ref B 1)))
   (define (at m x y)
      (ref (ref m x) y))

   (let mloop ((i m) (rows #null))
      (if (eq? i 0)
         (list->vector rows)
      else
         (mloop (-- i)
            (cons
               (let rloop ((j q) (row #null))
                  (if (eq? j 0)
                     (list->vector row)
                  else
                     (rloop (-- j)
                        (cons
                           (let loop ((k 1) (c 0))
                              (if (less? n k)
                                 c
                              else
                                 (loop (++ k) (+ c (* (at A i k) (at B k j))))))
                           row))))
               rows)))))

(define (matrix-transpose A)
   (list->vector
      (map (lambda (i)
            (vector-map (lambda (row)
                  (ref row i))
               A))
         (iota (size (ref A 1)) 1))))

(define (vector*matrix A B)
   (define n (size A))
   (assert (eq? (size B) n) ===> #true)
   (define q (size (ref B 1)))
   (define (at m x y)
      (ref (ref m x) y))

   (let rloop ((j q) (row #null))
      (if (eq? j 0)
         (list->vector row)
      else
         (rloop (-- j)
            (cons
               (let loop ((k 1) (c 0))
                  (if (less? n k)
                     c
                  else
                     (loop (++ k) (+ c (* (ref A k) (at B k j))))))
               row)))))

(define (mmap f array1 array2)
   (let loop ((array1 array1) (array2 array2))
      (if (vector? (ref array1 1))
         (vector-map loop array1 array2)
      else
         (vector-map f array1 array2))))

(define (matrix->list matrix)
   (define m (size matrix))
   (define n (size (ref matrix 1)))

   (let loopn ((j m) (out #n))
      (if (eq? j 0)
         out
      else
         (define row (ref matrix j))
         (loopn (- j 1) (let loopm ((i n) (out out))
                           (if (eq? i 0)
                              out
                              (loopm (- i 1) (cons (ref row i) out))))))))

; -=( main )=-------------------------------------------------------------
(print "Please wait while we are loading a train database, if not exist.")
(import
   (owl parse) (otus ffi)
   (file mnist)
   (file gzip))
; train labels
(display "loading train labels file...")
(define labels-file (try-parse gzip-parser (file->bytestream (string-append mnist-root
                        "train-labels-idx1-ubyte.gz")) #f))
(define labels (try-parse train-labels-parser ((car labels-file) 'stream) #f))
(define labels (list->vector ((car labels) 'labels)))
(print "ok.")

; train images
(display "loading train images file...")
(define images-file (try-parse gzip-parser (file->bytestream (string-append mnist-root
                        "train-images-idx3-ubyte.gz")) #f))
(define images (try-parse train-images-parser ((car images-file) 'stream) #f))
; let's convert raw bytearray into floats
(define images (map (lambda (image)
      (map (lambda (v)
            (/ v #i255))
         image))
   ((car images) 'images)))
; .. and a vector of images (to simplify random access)
(define images (list->vector images))
(print "ok.")

; testing dataset...
(display "loading test labels file...")
(define test-labels-file (try-parse gzip-parser (file->bytestream (string-append mnist-root
                        "t10k-labels-idx1-ubyte.gz")) #f))
(define test-labels (try-parse train-labels-parser ((car test-labels-file) 'stream) #f))
(define test-labels (list->vector ((car test-labels) 'labels)))
(print "ok.")

(display "loading train images file...")
(define test-images-file (try-parse gzip-parser (file->bytestream (string-append mnist-root
                        "t10k-images-idx3-ubyte.gz")) #f))
(define test-images (try-parse train-images-parser ((car test-images-file) 'stream) #f))
(define test-images (map (lambda (image)
      (map (lambda (v)
            (/ v #i255))
         image))
   ((car test-images) 'images)))
(define test-images (list->vector test-images))
(print "ok.")


; -=( show the image using gtk )=------------------------------
(import (lib gdk-3))
(import (lib gtk-3 image))
(import (lib gtk-3 tool-button))

(define bw-palette
   (let loop ((i #xFF) (out #n))
      (if (eq? i 0)
         (cons* 0 0 0 out)
         (loop (-- i) (cons* i i i out)))))
(define (set-image widget image width height)
   ; create B/W TGA image
   (define TGA (list->bytevector (append
      ; Image header
      (list
         0 1 1
         0 0  0 1  24 ; Color Map
         0 0  0 0 ; Origin of Image (x y)
         (band  width #xFF) (band (>>  width 8) #xFF)
         (band height #xFF) (band (>> height 8) #xFF)
         8 0) ; Pixel Size, Descriptor Byte
      ; Palette
      bw-palette
      ; Image
      (map (lambda (i) (exact (floor (* i #xFF)))) image))))

   (define loader (gdk_pixbuf_loader_new))
   (gdk_pixbuf_loader_write loader TGA (size TGA) #f)
   (gdk_pixbuf_loader_close loader #f)

   (gtk_image_set_from_pixbuf widget
      (gdk_pixbuf_scale_simple
         (gdk_pixbuf_flip (gdk_pixbuf_loader_get_pixbuf loader) 0)
         (gtk_widget_get_allocated_width widget)
         (gtk_widget_get_allocated_height widget) 0)))


; --------------------------------------------------------
; UI:
(gtk_init '(0) #f)
(define builder (gtk_builder_new_from_file "mnist.glade"))
(define window (gtk_builder_get_object builder "window"))

(define INPUT  (gtk_builder_get_object builder "INPUT" ))
(define LAYER1 (gtk_builder_get_object builder "LAYER1"))

(define NEURON1 (gtk_builder_get_object builder "NEURON1"))
(define NEURON2 (gtk_builder_get_object builder "NEURON2"))

(define P (gtk_builder_get_object builder "NUMBER"))

(define (test)
   (define p (random-integer (size test-images)))
   (gtk_label_set_text P (string-append "test" (number->string p)))
   ; draw the source image:
   (define texture (ref test-images p))
   (set-image INPUT texture 28 28)

   (define label (ref test-labels p))
   (for-each (lambda (l)
         (define s (number->string l))
         (gtk_label_set_markup (gtk_builder_get_object builder s)
            (if (eq? l label) (string-append "<b>" s "</b>") s)))
      '(0 1 2 3 4 5 6 7 8 9))

   ; calculations:
   (define input (list->vector texture))
   (define layer0 input)
   (define matrix1 (neuron1))   ; выход нейрона 1 (входного слоя)
   (define layer1 (vector-map sigmoid (vector*matrix layer0 matrix1)))
   (define layer1/ (vector-map sigmoid/ layer1)) ; speedup
   ; note: we can calculate vector*matrix, and then f and f/ from it

   (define matrix2 (neuron2))   ; выход нейрона 2 (выходной слой)
   (define layer2 (vector-map sigmoid (vector*matrix layer1 matrix2)))
   (define layer2/ (vector-map sigmoid/ layer2))

   (define output layer2)

   (set-image LAYER1 (vector->list layer1) 8 8) ; (size layer1) 1

   ; show the output:
   (define select (vector-fold max 0 output))
   (for-each (lambda (l)
         (define s (string-append "o" (number->string l)))
         (define v (substring (number->string (vector-ref output l)) 0 3))
         (gtk_label_set_markup (gtk_builder_get_object builder s)
            (if (= (vector-ref output l) select)
               (if (= l label)
                  (string-append "<span color='green'>" v "</span>")
                  (string-append "<span color='red'>" v "</span>"))
               v)))
      '(0 1 2 3 4 5 6 7 8 9))

   ; нарисуем новые нейроны
   (set-image NEURON1 (matrix->list matrix1) (size matrix1) (size (ref matrix1 1)))
   (set-image NEURON2 (matrix->list matrix2) (size matrix2) (size (ref matrix2 1)))
)


(define (step)
   (define p (random-integer (size images)))
   (gtk_label_set_text P (number->string p))
   ; draw the source image:
   (define texture (ref images p))
   (set-image INPUT texture 28 28)

   ; todo: output correct label
   (define label (ref labels p))
   (for-each (lambda (l)
         (define s (number->string l))
         (gtk_label_set_markup (gtk_builder_get_object builder s)
            (if (eq? l label) (string-append "<b>" s "</b>") s)))
      '(0 1 2 3 4 5 6 7 8 9))

   ; calculations:
   (define input (list->vector texture))
   (define layer0 input)

   (define matrix1 (neuron1))   ; выход нейрона 1 (входного слоя)
   (define layer1 (vector-map sigmoid (vector*matrix layer0 matrix1)))
   (define layer1/ (vector-map sigmoid/ layer1)) ; speedup
   ; note: we can calculate vector*matrix, and then f and f/ from it

   (define matrix2 (neuron2))   ; выход нейрона 2 (выходной слой)
   (define layer2 (vector-map sigmoid (vector*matrix layer1 matrix2)))
   (define layer2/ (vector-map sigmoid/ layer2))

   (define output layer2)

   (set-image LAYER1 (vector->list layer1) 8 8) ; (size layer1) 1

   ; show the output:
   (define select (vector-fold max 0 output))
   (for-each (lambda (l)
         (define s (string-append "o" (number->string l)))
         (define v (substring (number->string (vector-ref output l)) 0 3))
         (gtk_label_set_markup (gtk_builder_get_object builder s)
            (if (= (vector-ref output l) select)
               (if (= l label)
                  (string-append "<span color='green'>" v "</span>")
                  (string-append "<span color='red'>" v "</span>"))
               v)))
      '(0 1 2 3 4 5 6 7 8 9))

   ; learning
   ; 'ok' is a right answer
   (define ok (list->vector (map (lambda (i)
         (if (eq? i label) #i1 #i0))
      (iota 10))))

   (define e (vector-map - output ok))
   ;(define err (/ (vector-fold + 0 (vector-map * e e)) 2))

   (define delta2 (vector-map * e layer2/))

   (define delta1 (vector-map *
      (vector*matrix delta2 (matrix-transpose matrix2))
      layer1/))

   (define alpha 1.0) ; learning rate

   ; новая матрица весов
   ;; todo: optimize this code
   (define D2 (matrix-product (matrix-transpose [layer1]) [delta2]))
   (define new-matrix2 (mmap (lambda (x y) (- x (* y alpha))) matrix2 D2))
   
   (define D1 (matrix-product (matrix-transpose [layer0]) [delta1]))
   (define new-matrix1 (mmap (lambda (x y) (- x (* y alpha))) matrix1 D1))

   (neuron1 new-matrix1)
   (neuron2 new-matrix2)

   ; нарисуем новые нейроны
   (set-image NEURON1 (matrix->list matrix1) (size matrix1) (size (ref matrix1 1)))
   (set-image NEURON2 (matrix->list matrix2) (size matrix2) (size (ref matrix2 1)))

   (define n (string->number (gtk_label_get_text (gtk_builder_get_object builder "COUNTER"))))
   (gtk_label_set_text (gtk_builder_get_object builder "COUNTER") (number->string (+ n 1) 10))
)

(actor 'walker (lambda ()
   (let loop ((envelope (wait-mail)))
      (if envelope
      then
         (let*((sender msg envelope))
            (case msg
               ('play
                  (loop #f))
               ('pause
                  (loop (wait-mail)))
               ('step
                  (step)
                  (mail sender 'ok)
                  (loop (wait-mail)))
               ('test
                  (test)
                  (mail sender 'ok)
                  (loop (wait-mail)))
               ('quit
                  (mail sender 'ok))
               (else
                  (runtime-error "unknown command" msg))))
      else
         (step)
         (loop (check-mail))))))

; do one neural network step:
(define do-step
   (GTK_CALLBACK (widget userdata)
      (await (mail 'walker 'step))
      TRUE))
(gtk_builder_add_callback_symbol builder "step" (G_CALLBACK do-step))

(define do-play
   (GTK_CALLBACK (widget userdata)
      ; gtk_image_new_from_stock
      (mail 'walker 'play)
      TRUE))
(gtk_builder_add_callback_symbol builder "play" (G_CALLBACK do-play))

(define do-test
   (GTK_CALLBACK (widget userdata)
      (await (mail 'walker 'test))
      TRUE))
(gtk_builder_add_callback_symbol builder "test" (G_CALLBACK do-test))

(define do-pause ; merge with "step"
   (GTK_CALLBACK (widget userdata)
      (mail 'walker 'pause)
      TRUE))
(gtk_builder_add_callback_symbol builder "pause" (G_CALLBACK do-pause))

(define do-save
   (GTK_CALLBACK (widget userdata)
      (fasl-save (neuron1) "neuron1.fasl")
      (fasl-save (neuron2) "neuron2.fasl")
      (print "saved.") TRUE))
(gtk_builder_add_callback_symbol builder "save" (G_CALLBACK do-save))


; show window and run
(define-values (w h) (values '(0) '(0)))
(gtk_window_get_default_size window w h)
(gtk_window_resize window (car w) (+ (car h) 1))

(gtk_builder_connect_signals builder #f)
(gtk_widget_show_all window)

; idle function to be actors alive
(define idle (GTK_CALLBACK (userdata)
   (sleep 1) TRUE))
(gdk_threads_add_idle (G_CALLBACK idle) nullptr)

(define quit
   (GTK_CALLBACK (widget userdata)
      (gtk_main_quit)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

(gtk_main)
(shutdown 1)
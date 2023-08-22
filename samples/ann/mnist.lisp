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

(define LAYER1-LEN 64) ; size of intermediate (hidden) layer I

; neural math functions:
(import (otus algebra))
(import (otus algebra infix-notation))
(import (otus algebra print))

(define T matrix-transpose)

(import (srfi 27)) ; randomizer
(define (new-matrix rows columns)
   (Fill (Matrix~ rows columns)
      (lambda ()
         (- (random-real) #i0.5))))

; neurons
(import (scheme dynamic-bindings))
(define neuron1 (make-parameter (or ; neuron 1 weights (sinapses)
   ;(Matrix~ (fasl-load "neuron1.fasl" #f))
   (fasl-load "neuron1.fasl" #f)
   (new-matrix INPUT-LEN  LAYER1-LEN))))
(define neuron2 (make-parameter (or ; neuron 2 weights (sinapses)
   ;(Matrix~ (fasl-load "neuron2.fasl" #f))
   (fasl-load "neuron2.fasl" #f)
   (new-matrix LAYER1-LEN OUTPUT-LEN))))

(define (matrix->list matrix)
   (rfoldr cons #n matrix))


; -=( plot )=----------------------------------------------
; gnuplot:
(import (otus ffi))
(define FILE* type-vptr)
(define this (load-dynamic-library #f))
(define popen (this FILE* "popen" type-string type-string))
(define fprintf (this fft-int "fprintf" FILE* type-string))
(define fflush (this fft-void "fflush" FILE*))
; start gnuplot
(define gnuplot (popen "gnuplot" "w"))
(define (plot . args)
   (define buffer (open-output-string))
   (for-each (lambda (a)
         (display-to buffer a))
      args)
   (fprintf gnuplot "%s\n" (get-output-string buffer))
   (fflush gnuplot))

; -=( main )=-------------------------------------------------------------
(print "Please wait while we are loading a train database, if not exist.")
(import
   (owl parse) (otus ffi)
   (file mnist)
   (file gzip))
; train labels
(display "loading train labels file...")
(define labels-file (try-parse gzip-parser (file->bytestream "train-labels-idx1-ubyte.gz") #f))
(define labels (try-parse train-labels-parser ((car labels-file) 'stream) #f))
(define labels (list->vector ((car labels) 'labels)))
(print "ok.")

; train images
(display "loading train images file...")
(define images-file (try-parse gzip-parser (file->bytestream "train-images-idx3-ubyte.gz") #f))
(define images (try-parse train-images-parser ((car images-file) 'stream) #f))
; let's convert raw bytearray into floats
(define images (map (lambda (image)
      ; make an 28*28 image from bytevector
      (Reshape (Vector~ (list->vector
         (map (lambda (v)
               (/ v #i255))
            image)))
         '(28 28)))
   ((car images) 'images)))
; .. and a vector of images (to simplify random access)
(define images (list->vector images))
(print " " (size images) " images loaded.")

; testing dataset...
(display "loading test labels file...")
(define test-labels-file (try-parse gzip-parser (file->bytestream "t10k-labels-idx1-ubyte.gz") #f))
(define test-labels (try-parse train-labels-parser ((car test-labels-file) 'stream) #f))
(define test-labels (list->vector ((car test-labels) 'labels)))
(print "ok.")

(display "loading test images file...")
(define test-images-file (try-parse gzip-parser (file->bytestream "t10k-images-idx3-ubyte.gz") #f))
(define test-images (try-parse train-images-parser ((car test-images-file) 'stream) #f))
(define test-images (map (lambda (image)
      (Reshape (Vector~ (list->vector
         (map (lambda (v)
               (/ v #i255))
            image)))
         '(28 28)))
   ((car test-images) 'images)))
(define test-images (list->vector test-images))
(print " " (size test-images) " test images loaded.")

(define zero #i0.0)

; -=( show the image using gtk )=------------------------------
(import (lib gdk-3))
(import (lib gtk-3 image))
(import (lib gtk-3 tool-button))

(define bw-palette
   (let loop ((i #xFF) (out #n))
      (if (eq? i 0)
         (cons* 0 0 0 out)
         (loop (-- i) (cons* i i i out)))))
(define (set-image widget image width height normalize)
   (define byte (if normalize
      (lambda (float) (exact (floor (* (+ float #i1.0) #x7F))))
      (lambda (float) (exact (floor (* float #xFF))))))
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
      (map byte image))))

   (define loader (gdk_pixbuf_loader_new))
   (gdk_pixbuf_loader_write loader TGA (size TGA) #f)
   (gdk_pixbuf_loader_close loader #f)

   (define flip (gdk_pixbuf_flip (gdk_pixbuf_loader_get_pixbuf loader) 0))
   (define scaled (gdk_pixbuf_scale_simple
         flip
         (gtk_widget_get_allocated_width widget)
         (gtk_widget_get_allocated_height widget) 0))
   (gtk_image_set_from_pixbuf widget scaled)

   (g_object_unref scaled)
   (g_object_unref flip)
   (g_object_unref loader))


; --------------------------------------------------------
; UI:
(gtk_init '(0) #f)
(define builder (gtk_builder_new_from_file "mnist.glade"))
(define window (gtk_builder_get_object builder "window"))

(define INPUT (gtk_builder_get_object builder "INPUT"))
;; (define LAYER1 (gtk_builder_get_object builder "LAYER1"))

(define NEURON1 (gtk_builder_get_object builder "NEURON1"))
(define NEURON2 (gtk_builder_get_object builder "NEURON2"))

(define LOSS (gtk_builder_get_object builder "LOSS"))

(import (lib gtk-3 socket))
(define socket (gtk_socket_new))
(gtk_widget_show socket)
(gtk_container_add LOSS socket)
(gtk_widget_realize socket) ; if one of ancestors is not yet visible.
(define plug-added ; connect notification
   (GTK_CALLBACK (self userdata)
      (print "A widget (most likley gnuplot) has just been jacked in!")
      TRUE))
(g_signal_connect socket "plug-added" (G_CALLBACK plug-added) NULL)

(define P (gtk_builder_get_object builder "NUMBER"))

(define (test)
   ; let's select random test image
   (define p (+ (random-integer (size test-images)) 1))
   (gtk_label_set_text P (string-append " (test) " (number->string p)))

   ; draw the source image:
   (define test-image (ref test-images p)); слева-направо и сверху-вниз
   (define shifted (Shift test-image (map (lambda (lr)
                                             (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                                        (Paddings test-image))))

   (set-image INPUT (matrix->list shifted) 28 28 #false)

   (define label (ref test-labels p))
   (for-each (lambda (l)
         (define s (number->string l))
         (gtk_label_set_markup (gtk_builder_get_object builder s)
            (if (eq? l label) (string-append "<b>" s "</b>") s)))
      '(0 1 2 3 4 5 6 7 8 9))

   ; calculations:
   (define matrix1 (neuron1))   ; матрица нейрона 1 (hidden layer)
   (define matrix2 (neuron2))   ; матрица нейрона 2 (output layer)

   ; let's make a vector from the input texture
   (define layer0 (Reshape shifted (list 1 (Size shifted))))
   (define layer1 (Logistic (matrix·matrix layer0 matrix1)))
   (define layer2 (Logistic (matrix·matrix layer1 matrix2)))

   (define output (Reshape layer2 (list (Size layer2))))

   ;; (gtk_widget_show LAYER1)
   ;; (gtk_widget_hide LOSS)
   ;; (set-image LAYER1 (matrix->list layer1) 8 8 #f) ; (size layer1) 1

   ; show the output:
   (define select (fold max 0 (matrix->list output)))
   (for-each (lambda (l i)
         (define s (string-append "o" (number->string l)))
         (define v (if (< (Ref output i) #i0.00001)
                     "0"
                     (substring (number->string (Ref output i)) 0 3)))
         (gtk_label_set_markup (gtk_builder_get_object builder s)
            (if (= (Ref output i) select)
               (if (= l label)
                  (string-append "<span color='green'>" v "</span>")
                  (string-append "<span color='red'>" v "</span>"))
               v)))
      (iota 10 0)
      (iota 10 1))

   ; нарисуем новые нейроны
   (set-image NEURON1 (matrix->list matrix1) (first (Shape matrix1)) (second (Shape matrix1)) #true)
   (set-image NEURON2 (matrix->list matrix2) (first (Shape matrix2)) (second (Shape matrix2)) #true)
)


(define acc (make-parameter 0))
(define loss (make-parameter (fasl-load "loss.fasl" '())))

(define (step N)
   ;(define old (time-ms))

   (let loop ((input #false)
              (matrix1 (neuron1)) ; матрица нейрона 1 (hidden layer)
              (matrix2 (neuron2)) ; матрица нейрона 2 (output layer)
              (n N))
      (if (zero? n)
      then
         ; последняя картинка батча
         (set-image INPUT (matrix->list input) 28 28 #f)
         ; нарисуем новые нейроны
         (set-image NEURON1 (matrix->list matrix1) (first (Shape matrix1)) (second (Shape matrix1)) #true)
         (set-image NEURON2 (matrix->list matrix2) (first (Shape matrix2)) (second (Shape matrix2)) #true)

         ; и обновимся
         (neuron1 matrix1)
         (neuron2 matrix2)
      else
         (define p (+ (random-integer (size images)) 1))
         (gtk_label_set_text P (number->string p))
         ; draw the source image:
         (define image (ref images p))

         (define shifted (Shift image
               (map (lambda (lr)
                       (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                  (Paddings image))))

         ; todo: output correct label
         (define label (ref labels p))
         (for-each (lambda (l)
               (define s (number->string l))
               (gtk_label_set_markup (gtk_builder_get_object builder s)
                  (if (eq? l label) (string-append "<b>" s "</b>") s)))
            '(0 1 2 3 4 5 6 7 8 9))

         ; calculations:

         ; let's make a vector (one-row matrix) from input texture
         (define layer0 (Reshape shifted (list 1 (Size shifted))))  ; input layer
         (define layer1 (Logistic (matrix·matrix layer0 matrix1)))
         (define layer2 (Logistic (matrix·matrix layer1 matrix2)))  ; todo: change to matrix*vectorT

         (define output (Reshape layer2 (list (Size layer2)))) ; output vector

         ; show the output:
         (define select (fold max 0 (matrix->list output)))
         (for-each (lambda (l i)
               (define s (string-append "o" (number->string l)))
               (define v (if (< (Ref output i) #i0.00001)
                           "0"
                           (substring (number->string (Ref output i)) 0 3)))
               (gtk_label_set_markup (gtk_builder_get_object builder s)
                  (if (= (Ref output i) select)
                     (if (= l label)
                        (string-append "<span color='green'>" v "</span>")
                        (string-append "<span color='red'>" v "</span>"))
                     v)))
            (iota 10 0)
            (iota 10 1))

         ; --------------------------------------------------
         ; learning (back propagation)
         ; 'ok' is a right answer
         (define ok (Matrix~ 1 (list->vector (map (lambda (i) ; cirrect answer
               (if (eq? i label) #i1 #i0))
            (iota 10)))))

         (define alpha 1.0) ; learning rate

         ; layer2 calculations
         (define layer2/ (DLogisticDx_Logistic layer2)) ; dOut/dNet, because layer2 already Logisticised
         (define delta2 (- layer2 ok)) ; dEtotal/dOut (error)
         (define error2 (* delta2 layer2/)) ; dEtotal/dOut * dOut/dNet, покомпонентное умножение векторов
         ; dEtotal/dWi * dEtotal/dOut * dOut/dNet
         (define new-matrix2 (infix-notation
            matrix2 - alpha * T(layer1) • error2
         ))

         ; layer1 calculations
         (define layer1/ (DLogisticDx_Logistic layer1)) ; dOut/dNet
         (define delta1
            ;(matrix·matrix error2 (T matrix2)))    ; slower - need to T matrix (expensive)
            (T (matrix·matrix matrix2 (T error2)))) ; faster - need to T two vectors (a very fast operation, zero-copy)
         (define error1 (* delta1 layer1/))

         (define new-matrix1 (infix-notation
            matrix1 - alpha * T(layer0) • error1
         ))

         ; увеличим счетчик
         (define c (string->number (gtk_label_get_text (gtk_builder_get_object builder "COUNTER"))))
         (gtk_label_set_text (gtk_builder_get_object builder "COUNTER") (number->string (+ c 1) 10))

         ; посчитаем loss (и выведем на график, если пришло время)
         (define err (let ((delta (matrix->list delta2)))
            (/ (fold + 0 (map * delta delta)) (length delta))))
         (acc (+ (acc) err))

         (define epoch 1000)
         (define c1 (+ c 1))
         (when (and (> c1 1) (eq? (mod c1 epoch) 1))
            (define v (/ (acc 0) epoch))
            (loss (append (loss) (list v))))

         (when (eq? (mod c1 epoch) 1)
            (define l (loss))
            (unless (null? l)
               (plot "plot '-' with lines title 'Loss', '-' smooth bezier lt rgb 'black' title 'approx', 0.03 title '3%' lt rgb 'green'") ; linesp
               (for-each plot l) (plot "e")
               (for-each plot l) (plot "e")))

         (loop shifted new-matrix1 new-matrix2 (- n 1))))

   ;(define new (time-ms)) (print new "-" old " = " (- new old))
)

(actor 'walker (lambda ()
   (define batch-size '(1))
   (let loop ((envelope (wait-mail)))
      (if envelope
      then
         (let*((sender msg envelope))
            (case msg
               ('play
                  (set-car! batch-size 1)
                  (loop #f))
               ('batch
                  (set-car! batch-size 100)
                  (loop #f))
               ('pause
                  (loop (wait-mail)))

               ('step
                  (step 1)
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
         (step (car batch-size))
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

(define do-batch
   (GTK_CALLBACK (widget userdata)
      (mail 'walker 'batch)
      TRUE))
(gtk_builder_add_callback_symbol builder "batch" (G_CALLBACK do-batch))

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
      (fasl-save (loss) "loss.fasl")
      (print "saved.") TRUE))
(gtk_builder_add_callback_symbol builder "save" (G_CALLBACK do-save))


; show window and run
(define-values (w h) (values '(0) '(0)))
(gtk_window_get_default_size window w h)
(gtk_window_resize window (car w) (+ (car h) 1))

(gtk_builder_connect_signals builder #f)
(gtk_widget_show_all window)

; init plot
(plot "set terminal x11 window '"
      (number->string (gtk_socket_get_id socket) 16) "'")
(plot "set notitle")
(plot "unset mouse")
(plot "set yrange [0:]")
(plot "clear")

; idle function to be actors alive
(define idle (GTK_CALLBACK (userdata)
   (sleep 0) TRUE))
(gdk_threads_add_idle (G_CALLBACK idle) nullptr)

(define quit
   (GTK_CALLBACK (widget userdata)
      (await (mail 'walker 'quit))
      (gtk_main_quit)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

(gtk_main)
(shutdown 1)

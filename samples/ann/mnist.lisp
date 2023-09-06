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

(define LAYER1-LEN 99) ; size of intermediate (hidden) layer I
;; (define LAYER2-LEN 32) ; size of intermediate (hidden) layer 2

; neural math functions:
(import (otus algebra))
(import (otus algebra infix-notation))
(import (otus algebra print))
; let's shorten infix-notation
(define-macro @ (lambda args
   `(infix-notation ,args)))

(import (otus algebra unicode))
(define T matrix-transpose)

(import (srfi 27)) ; randomizer
(define (new-matrix rows columns)
   (Fill (Matrix~ rows columns)
      (lambda ()
         (- (random-real) #i0.5))))

; neurons
(import (scheme dynamic-bindings))
(define neuron1 (make-parameter (or
   (fasl-load "neuron1.fasl" #f)
   (new-matrix INPUT-LEN  LAYER1-LEN))))
(define neuron2 (make-parameter (or
   (fasl-load "neuron2.fasl" #f)
   (new-matrix LAYER1-LEN OUTPUT-LEN))))

;; (define neuron3 (make-parameter (or ; output neuron weights (sinapses)
;;    ;(Matrix~ (fasl-load "neuronH.fasl" #f))
;;    (fasl-load "neuron3.fasl" #f)
;;    (new-matrix LAYER2-LEN OUTPUT-LEN))))

; --------------------------
(define (tensor? t)    ; c vector
   (and (eq? (type t) type-pair)
        (eq? (type (cdr t)) type-bytevector)))

(define (matrix->list matrix)
   (cond
      ((vector? matrix) ; builtin array
         (vector->list (Reshape matrix (list (Size matrix)))))
      ((tensor? matrix) ; external data
         (rfoldr cons #n matrix))))

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
(import (lib gtk-3 bin))

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
   ;; (define p 2)
   (gtk_label_set_text P (string-append " (test) " (number->string p)))

   ; draw the source image:
   (define image (ref test-images p)); слева-направо и сверху-вниз
   (define shifted (Shift image (map (lambda (lr)
                                        (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                                   (Paddings image))))
   (set-image INPUT (matrix->list shifted) 28 28 #false)

   (define label (ref test-labels p))
   (for-each (lambda (l)
         (define s (number->string l))
         (gtk_label_set_markup (gtk_bin_get_child (gtk_builder_get_object builder s))
            (if (eq? l label) (string-append "<b>" s "</b>") s)))
      '(0 1 2 3 4 5 6 7 8 9))

   ;; ; calculations:
   (define matrix1 (neuron1))   ; матрица нейрона 1 (hidden layer)
   (define matrix2 (neuron2))   ; матрица нейрона 2 (hidden layer)
   ;; (define matrix3 (neuron3))   ; матрица нейрона 3 (output layer)

   ; let's make a one-row matrix [vector] from input texture
   (define layer0 (Reshape shifted (list 1 (Size shifted)))) ; input layer

   (define layer1 (Logistic (· layer0 matrix1)))
   (define layer2 (Logistic (· layer1 matrix2)))

   (define output (Reshape layer2 (list (Size layer2))))  ; back to vector

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


;; (define acc (make-parameter 0))
(define Loss (make-parameter (fasl-load "Loss.fasl" '())))

(define (step N)
   ;(define old (time-ms))

   (let loop ((input #false)
              (matrix1 (neuron1)) ; матрица нейрона 1 (hidden layer)
              (matrix2 (neuron2)) ; матрица нейрона 2 (hidden layer)
            ;;   (matrix3 (neuron3)) ; матрица нейрона 3 (output layer)
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
         ;; (neuron3 matrix3)
      else
         (define p (+ (random-integer (size images)) 1))
         (gtk_label_set_text P (number->string p))
         ; draw the source image:
         (define image (ref images p))

         (define shifted (Shift image
               (map (lambda (lr)
                       (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                  (Paddings image))))
         ;; (define shifted image)

         ; output correct label:
         (define label (ref labels p))
         (for-each (lambda (l)
               (define s (number->string l))
               (gtk_label_set_markup (gtk_bin_get_child (gtk_builder_get_object builder s))
                  (if (eq? l label) (string-append "<b>" s "</b>") s)))
            '(0 1 2 3 4 5 6 7 8 9))

         ; calculations:

         ; let's make a one-row matrix [vector] from input texture
         (define layer0 (Reshape shifted (list 1 (Size shifted)))) ; input layer

         (define layer1 (infix-notation
            Logistic (layer0 · matrix1)
         ))
         (define layer2 (infix-notation
            Logistic (layer1 · matrix2)
         ))

         (define output (Reshape layer2 (list (Size layer2))))  ; back to vector

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

         ; learning rate
         (define alpha (+ 0.1 (* (random-real) 0.2)))

         ; layer3 calculations
         (define deltaO (- layer2 ok)) ; dEtotal/dOut (output error)

         ;; (define layer3/ (DLogisticDx_Logistic layer3)) ; dOut/dNet, because layer2 already Logisticised
         ;; (define error3 (* deltaO layer3/)) ; dEtotal/dOut * dOut/dNet, покомпонентное умножение векторов
         ;; ; dEtotal/dWi * dEtotal/dOut * dOut/dNet
         ;; (define new-matrix3 (infix-notation
         ;;    matrix3 - alpha * T(layer2) • error3
         ;; ))

         ;; ; layer2 calculations
         ;; (define delta2 (T (matrix·matrix matrix3 (T error3)))) ; dEtotal/dOut (error) / (matrix·matrix error3 (T matrix3)))

         (define layer2/ (DLogisticDx_Logistic layer2)) ; dOut/dNet
         (define error2 (* deltaO layer2/))
         (define new-matrix2 (infix-notation
            matrix2 - alpha * T(layer1) • error2
         ))

         ; layer1 calculations
         (define delta1 (T (matrix·matrix matrix2 (T error2)))) ; faster - need to T two vectors (a very fast operation, zero-copy)
         (define layer1/ (DLogisticDx_Logistic layer1)) ; dOut/dNet
         (define error1 (* delta1 layer1/))
         (define new-matrix1 (infix-notation
            matrix1 - alpha * T(layer0) • error1
         ))

         ; увеличим счетчик
         (define c (string->number (gtk_label_get_text (gtk_builder_get_object builder "COUNTER"))))
         (gtk_label_set_text (gtk_builder_get_object builder "COUNTER") (number->string (+ c 1) 10))

         ;; ; посчитаем loss (и выведем на график, если пришло время)
         ;; (define err (let ((delta (matrix->list deltaO)))
         ;;    (/ (fold + 0 (map * delta delta)) (length delta))))
         ;; (acc (+ (acc) err))

         ;; (define epoch 1000)
         ;; (define c1 (+ c 1))
         ;; (when (and (> c1 1) (eq? (mod c1 epoch) 1))
         ;;    (define v (/ (acc 0) epoch))
         ;;    (loss (append (loss) (list v))))

         ; посчитаем loss на тестовых данных и выведем на график (если пришло время)
         ;; (define err (let ((delta (matrix->list deltaO)))
         ;;    (/ (fold + 0 (map * delta delta)) (length delta))))
         ;; (acc (+ (acc) err))

         (define epoch 1000)
         (define c1 (+ c 1))

         ; считаем только если минимум одна эпоха прошла
         (when (and (> c1 1) (eq? (mod c1 epoch) 1))
            (define N 100)
            (define loss (/
               (fold (lambda (S _)
                        (define p (+ (random-integer (size test-images)) 1))
                        (define test-label (ref test-labels p))
                        (define test-image (ref test-images p)); слева-направо и сверху-вниз
                        (define shifted (Shift test-image (map (lambda (lr)
                                                                  (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                                                            (Paddings test-image))))

                        (define layer0 (Reshape shifted (list 1 (Size shifted)))) ; input layer

                        (define layer1 (Logistic (matrix·matrix layer0 matrix1)))
                        (define layer2 (Logistic (matrix·matrix layer1 matrix2)))
                        ;; (define layer3 (Logistic (matrix·matrix layer2 matrix3)))

                        (define output (Reshape layer2 (list (Size layer2))))  ; back to vector
                        (define ok (Matrix~ 1 (list->vector (map (lambda (i) ; cirrect answer
                              (if (eq? i test-label) #i1 #i0))
                           (iota 10)))))
                        (+ S 
                           (sqrt (fold (lambda (dx ok out)
                                          (+ dx (* (- ok out) (- ok out))))
                                    0
                                    (matrix->list ok)
                                    (matrix->list output))) ))
                  0
                  (iota N))
               N))
            (Loss (append (Loss) (list loss))))

         (when (eq? (mod c1 epoch) 1)
            (define l (Loss))
            (unless (null? l)
               (plot "plot '-' with lines title '" (last l) "' lt rgb 'yellow', "
                          "'-' smooth bezier title 'approx' lt rgb 'black', "
                          "0.2 title '0.2' lt rgb 'green'")
               (for-each plot l) (plot "e")
               (for-each plot l) (plot "e")))

         (loop shifted new-matrix1 new-matrix2 #|new-matrix3|# (- n 1))))

   ;(define new (time-ms)) (print new "-" old " = " (- new old))
)

(define (regen n)
   ;; (define vec (list->vector (map
   ;;    (lambda (i)
   ;;       (if (= i n) 1 0))
   ;;    (iota 10))))

   ;; (define out (Matrix~ 1 vec))

   ;; (define matrix1 (neuron1)) ; матрица нейрона 1 (hidden layer)
   ;; (define matrix2 (neuron2)) ; матрица нейрона 2 (output layer)

   ;; (define layer2 (T out))
   ;; (define layer1 (matrix·matrix matrix2 layer2))
   ;; (define layer0 (matrix·matrix matrix1 layer1))
   ;; (define inp layer0)

   ;; (set-image INPUT (matrix->list inp) 28 28 #f)
   #false)

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

               (['regen n]
                  (regen n)
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

(define do-regen ; merge with "step"
   (GTK_CALLBACK (widget userdata)
      (define n (string->number (gtk_label_get_text (gtk_bin_get_child widget))))
      (mail 'walker ['regen n])
      TRUE))
(gtk_builder_add_callback_symbol builder "regen" (G_CALLBACK do-regen))

(define do-save
   (GTK_CALLBACK (widget userdata)
      (fasl-save (neuron1) "neuron1.fasl")
      (fasl-save (neuron2) "neuron2.fasl")
      ;; (fasl-save (neuron3) "neuron3.fasl")
      (fasl-save (Loss) "Loss.fasl")
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

#!/usr/bin/env ol
(import
   (lib glib-2)
   (lib gtk-3))

; --= mnist data =----------
(define mnist-root "./")
(print "<run `make` to download mnist handwritten digits dataset>");

; neural topology setup:
; ------------------------------------------------------------
(define INPUT-LEN (* 28 28)) ; input image = (28 * 28) pixels,
(define OUTPUT-LEN 10) ; and only 10 possible numbers (0 .. 9)

(define LAYER1-LEN 99) ; size of intermediate (hidden) layer 1

; ------------------------------------------------------------

; neural math functions:
(import (otus algebra))
(import (math infix-notation))

(import (srfi 27)) ; randomizer
(define (new-matrix rows columns)
   (Fill (Matrix~ rows columns)
      (lambda ()
         (- (random-real) #i0.5))))

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
   (file mnist) (file gzip))
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
               (/ v #i255.0))
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

; ==============================================
; GL Draw Area
(import (lib gtk-3 glarea))
(import (OpenGL 2.1))
(import (only (OpenGL ARB texture_rg) GL_R32F))

(define GlDrawArea
   (define (make ptr options)
      ; private
      (define id (make-list 1 0)) ; texture id
      (define po (make-list 1 0)) ; program id

      ; oop
      (define base (GtkGLArea ptr {
         ; opengl init
         'on-realize (lambda (this)
            (gtk_gl_area_make_current ptr)
            (glClearColor 0.2 0.2 0.2 1.0)

            (glEnable GL_TEXTURE_2D)
            ; create texture
            (glGenTextures 1 id)
            (glBindTexture GL_TEXTURE_2D (car id))

            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
            (glTexImage2D GL_TEXTURE_2D 0 GL_R 1 1 0 GL_RED GL_FLOAT #false)
            (glBindTexture GL_TEXTURE_2D 0)
            ; create program
            (set-car! po (gl:create-program
               "#version 120 // OpenGL 2.1
                  void main() {
                     gl_Position = gl_Vertex;
                     gl_TexCoord[0] = gl_MultiTexCoord0;
                  }"
               "#version 120 // OpenGL 2.1
                  uniform sampler2D matrix;
                  uniform bool normalize;
                  uniform float shift;
                  void main(void) {
                     float weight = texture2D(matrix, gl_TexCoord[0].st).r;
                     if (normalize) {
                        weight += 1.0;
                        weight /= 2.0;
                     }
                     gl_FragColor = vec4(weight, weight, weight, 1.0);
                  }")) )

         ; render texture
         'on-render (lambda (this)
            (glClear GL_COLOR_BUFFER_BIT)

            (glUseProgram (car po))
            (glUniform1i (glGetUniformLocation (car po) "matrix") 0)
            (glUniform1f (glGetUniformLocation (car po) "shift") (options 'shift 0))
            (glUniform1i (glGetUniformLocation (car po) "normalize") (if (options 'normalize #f) 1 0))
            (glColor3f 1 1 1)
            (glBindTexture GL_TEXTURE_2D (car id))
            (glBegin GL_QUADS)
               (glTexCoord2f 0 0)
               (glVertex2f -1  1)
               (glTexCoord2f 0 1)
               (glVertex2f -1 -1)
               (glTexCoord2f 1 1)
               (glVertex2f  1 -1)
               (glTexCoord2f 1 0)
               (glVertex2f  1  1)
            (glEnd))
      }))
      ; public interface
      (define this (ff-replace base {
         ; Load matrix to texture
         'update (lambda (matrix)
            (gtk_gl_area_make_current ptr)

            (define I (car id))
            (define shape (Shape matrix))

            ; set texture content
            (glBindTexture GL_TEXTURE_2D I)
            (if matrix
               (glTexImage2D GL_TEXTURE_2D 0 GL_R32F
                  (first shape) (second shape)
                  0 GL_RED GL_FLOAT (cdr matrix))
            else
               (glTexImage2D GL_TEXTURE_2D 0 GL_R32F
                  1 1
                  0 GL_RED GL_FLOAT #false) )
            (glBindTexture GL_TEXTURE_2D 0)
            ; signal rerender
            ((base 'queue-render)))

         ; internals
         'super base
      }))

      ; smart object
      (GObject this))
   (case-lambda
      ((ptr) (make ptr #e))
      ((ptr op) (make ptr op))))

; 


; ==============================================
; ==============================================
; UI
(gtk_init {
   'multithreaded #true
})
(define builder (GtkBuilder "mnist.glade"))
(define window ((builder 'get-object) "window"))
(define P ((builder 'get-object) "NUMBER"))

; OpenGL
(define INPUT (GlDrawArea
   ((builder 'get-object) "INPUT")))
(define NEURON1 (GlDrawArea
   ((builder 'get-object) "NEURON1") {'normalize #t}))
(define NEURON2 (GlDrawArea
   ((builder 'get-object) "NEURON2") {'normalize #t}))

(define NEURON3 (GlDrawArea
   ((builder 'get-object) "NEURON3") {'normalize #f}))

; custom output
(define (show-calculated-answer answer label)
   (print "show-calculated-answer")
   ; calculated answer
   (define select (fold max 0 (matrix->list answer)))
   ; show correct answer
   (for-each (lambda (l)
         (define s (number->string l))
         (define B (GtkButton ((builder 'get-object) s)))
         ((B 'set-markup)
            (if (eq? l label) (string-append "<big><b>" s "</b></big>") s)))
      '(0 1 2 3 4 5 6 7 8 9))

   ; and calculated answer
   (for-each (lambda (l i)
         (define s (string-append "o" (number->string l)))
         (define v (if (< (Ref answer i) #i0.00001)
                     "0"
                     (substring (number->string (Ref answer i)) 0 3)))
         (gtk_label_set_markup ((builder 'get-object) s)
            (if (= (Ref answer i) select)
               (if (= l label)
                  (string-append "<span color='green'>" v "</span>")
                  (string-append "<span color='red'>" v "</span>"))
               v)))
      (iota 10 0)
      (iota 10 1)))

; -=( business logic )=-------------------------
; AI topology
; ----------------------------------------------
(import (scheme dynamic-bindings))
(define neuron1 (make-parameter (or
   (fasl-load "model/neuron1.fasl" #f)
   (new-matrix INPUT-LEN  LAYER1-LEN))))
(define neuron2 (make-parameter (or
   (fasl-load "model/neuron2.fasl" #f)
   (new-matrix LAYER1-LEN OUTPUT-LEN))))
(define Loss (make-parameter
   (fasl-load "model/Loss.fasl" '())))
(gtk_label_set_text ((builder 'get-object) "COUNTER")
   (fasl-load "model/Count.fasl" "0"))

; common functions
(define (calculate-loss)
   0)
;; (define (calculate-loss)
;;    (print "calculate-loss")
;;    ; прогоним проверку по 100 рандомным картинкам
;;    (define N 100)
;;    (/ (fold (lambda (S _)
;;                (define p (+ (random-integer (size test-images)) 1))
;;                (define test-label (ref test-labels p))
;;                (define test-image (ref test-images p))
;;                (define shifted (Shift test-image (map (lambda (lr)
;;                                                          (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
;;                                                    (Padding test-image))))

;;                (define layer0 (Reshape shifted (list 1 (Size shifted)))) ; input layer

;;                (define layer1 (Logistic (matrix·matrix layer0 (neuron1))))
;;                (define layer2 (Logistic (matrix·matrix layer1 (neuron2))))
;;                ;; (define layer3 (Logistic (matrix·matrix layer2 matrix3)))

;;                (define output (Reshape layer2 (list (Size layer2)))) ; back to vector
;;                ;; (define ok (Matrix~ 1 (list->vector (map (lambda (i)  ; correct answer
;;                ;;       (if (eq? i test-label) #i1 #i0))
;;                ;;    (iota 10)))))
;;                ;; (+ S (/
;;                ;;    (fold (lambda (dx ok out)
;;                ;;             (+ dx (** (- ok out) 2)))
;;                ;;       0
;;                ;;       (matrix->list ok)
;;                ;;       (matrix->list output)) (Size output)) ))
;;                (define ok (- (fold (lambda (ok i)
;;                      (if (> (Ref output i) (Ref output ok))
;;                         i ok))
;;                   1
;;                   (iota (Size output) 1)) 1))
;;                (if (= ok test-label)
;;                   (+ S 1)
;;                   S))
;;          0
;;          (iota N))
;;       (inexact N)))

; calculate raw loss on random data
(when (null? (Loss))
   (define loss (calculate-loss))
   (Loss (list loss)))

; test functions
(define (test id)
   (print "test " id)

   ; let's select random test image
   (define p (if id id (+ (random-integer (size test-images)) 1)))
   ;; (define p 35)
   (gtk_label_set_text P (string-append " (test) " (number->string p)))

   ; source image
   (define image (ref test-images p)); слева-направо и сверху-вниз
   (define shifted (Shift image (map (lambda (lr)
                                        (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                                   (Padding image))))
   ;; (define shifted image)
   ; show the input
   ((INPUT 'update) shifted)
   (print "x")
   ;((NEURON3 'update) (Reshape shifted (list (Size shifted) 1))) ; TEMP
   ; correct answer
   (define label (ref test-labels p))
   (print "label: " label)

   ;; calculations:
   (define matrix1 (neuron1))   ; матрица нейрона 1 (hidden layer)
   (define matrix2 (neuron2))   ; матрица нейрона 2 (hidden layer)
;; (define matrix3 (neuron3))   ; матрица нейрона 3 (output layer)

   ; let's make a one-row matrix [vector] from input texture
   (define layer0 (Reshape shifted (list 1 (Size shifted)))) ; input layer
   (define layer1 (Logistic (· layer0 matrix1)))
   (define layer2 (Logistic (· layer1 matrix2)))
   (define output (Reshape layer2 (list (Size layer2))))  ; back to vector

   ; show the output:
   (show-calculated-answer output label)
   ; нарисуем нейроны
   ((NEURON1 'update) matrix1)
   ((NEURON2 'update) matrix2)
   (print "test ok")
   #false)

; work functions
(define (step N)
   (print "step " N)
   (let loop ((input #false)
              (matrix1 (neuron1)) ; матрица нейрона 1 (hidden layer)
              (matrix2 (neuron2)) ; матрица нейрона 2 (hidden layer)
              (n N))
      (if (zero? n)
      then
         ; последняя картинка батча
         ;; ((INPUT 'update) input)
         ; нарисуем новые нейроны
         ((NEURON1 'update) matrix1)
         ((NEURON2 'update) matrix2)
         ;; (neuron3 matrix3)
      else
         (define p (+ (random-integer (size images)) 1))
         (gtk_label_set_text P (number->string p))
         ; draw the source image:
         (define image (ref images p))

         (define shifted (Shift image
               (map (lambda (lr)
                       (+ (random-integer (- (cdr lr) (car lr) -1)) (car lr)))
                  (Padding image))))

         ((INPUT 'update) shifted)

         ; ---------------------------------------------------------------------
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
         ; ---------------------------------------------------------------------

         ; show the output:
         (define label (ref labels p))
         (show-calculated-answer output label)

         ; --------------------------------------------------
         ; learning (back propagation)
         (define T matrix-transpose)

         ; 'ok' is a right answer
         (define ok (Matrix~ 1 (list->vector (map (lambda (i) ; cirrect answer
               (if (eq? i label) #i1 #i0))
            (iota 10)))))

         ; learning rate
         (define alpha (+ 0.1 (* (random-real) 0.2)))

         ; layer3 calculations
         (define deltaO (- layer2 ok)) ; dEtotal/dOut (output error)

         ;; ; layer2 calculations
         ;; (define delta2 (T (matrix·matrix matrix3 (T error3)))) ; dEtotal/dOut (error) / (matrix·matrix error3 (T matrix3)))
         (define delta2 deltaO)

         (define layer2/ (DLogisticDx_Logistic layer2)) ; dOut/dNet
         (define error2 (* delta2 layer2/))
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

         ; и обновимся
         (neuron1 new-matrix1)
         (neuron2 new-matrix2)

         ; ---------------------------------
         ; увеличим счетчик
         ;; (let ((counter (GtkLabel ...))))
         (define c (string->number (gtk_label_get_text ((builder 'get-object) "COUNTER"))))
         (gtk_label_set_text ((builder 'get-object) "COUNTER") (number->string (+ c 1) 10))

         ; loss calculations
         (define epoch 1000)
         (define c1 (+ c 1))

         ; считаем только если минимум одна эпоха прошла
         (when (and (> c1 1) (eq? (mod c1 epoch) 1))
            (define loss (calculate-loss))
            (Loss (append (Loss) (list loss))))

         (when (eq? (mod c1 epoch) 1)
            (define l (Loss))
            (unless (null? l)
               (plot "plot '-' with linespoints title '" (last l) "' lt rgb 'red', "
                          "'-' smooth bezier title 'approx' lt rgb 'black', "
                          "1.0 title '100%' lt rgb 'green'")
               (for-each plot l) (plot "e")
               (for-each plot l) (plot "e")))

         ; все, можно уходить в цикл
         (loop shifted new-matrix1 new-matrix2 #|new-matrix3|# (- n 1)))
      ))

; -=( walker )=-----------------
(actor 'walker (lambda ()
   (define batch-size '(1))
   (let loop ((envelope (wait-mail)))
      (if envelope
         (let*((sender msg envelope))
            (case msg
               ; trainig step
               ('step
                  (step 1)
                  (mail sender 'ok)
                  (loop (wait-mail)))

               ; trainig cycle
               ('play
                  (set-car! batch-size 1)
                  (loop #f))
               ;; ('batch
               ;;    (set-car! batch-size 100)
               ;;    (loop #f))
               ('pause
                  (loop (wait-mail)))

               ; manual test(s)
               ('test
                  (test #f)
                  (mail sender 'ok)
                  (loop (wait-mail)))

               ; ..............
               ('quit
                  (mail sender 'ok))
               (else
                  (runtime-error "unknown command" msg))))
      else
         (step (car batch-size))
         (loop (check-mail))))))

; -=( loss )=----------------------
(import
   (lib gtk-3 socket))

(define socket (gtk_socket_new))
(gtk_widget_show socket)
(gtk_container_add ((builder 'get-object) "LOSS") socket)
(gtk_widget_realize socket) ; if one of ancestors is not yet visible.
(define plug-added ; connect notification
   (GTK_CALLBACK (self userdata)
      (print "A widget (probably gnuplot) has just been connected")
      ; show the loss immediately (if any)
      (define l (Loss))
      (unless (null? l)
         (plot "plot '-' with linespoints title '" (last l) "' lt rgb 'red', "
                    "'-' smooth bezier title 'approx' lt rgb 'black', "
                    "1.0 title '100%' lt rgb 'green'")
         (for-each plot l) (plot "e")
         (for-each plot l) (plot "e"))

      TRUE))
(g_signal_connect socket "plug-added" (G_CALLBACK plug-added) NULL)

; -- button handlers --------
((builder 'add-callback-symbol) "test"
   (GTK_CALLBACK (widget userdata)
      (await (mail 'walker 'test))
      TRUE))

((builder 'add-callback-symbol) "step"
   (GTK_CALLBACK (widget userdata)
      (await (mail 'walker 'step))
      TRUE))

((builder 'add-callback-symbol) "play"
   (GTK_CALLBACK (widget userdata)
      ((NEURON3 'update) #f)
      (mail 'walker 'play)
      TRUE))

((builder 'add-callback-symbol) "pause"
   (GTK_CALLBACK (widget userdata)
      (mail 'walker 'pause)
      TRUE))

((builder 'add-callback-symbol) "find"
   (GTK_CALLBACK (widget userdata)
      (define label (string->number (((GtkButton widget) 'get-text))))
      (let loop ()
         (define id (+ (random-integer (size test-images)) 1))
         (if (= (ref test-labels id) label)
            (test id)
            (loop)))
      TRUE))


(define do-batch
   (GTK_CALLBACK (widget userdata)
      ;(mail 'walker 'batch)
      TRUE))
(gtk_builder_add_callback_symbol (builder 'ptr) "batch" (G_CALLBACK do-batch))

((builder 'add-callback-symbol) "save"
   (GTK_CALLBACK (widget userdata)
      (fasl-save (neuron1) "model/neuron1.fasl")
      (fasl-save (neuron2) "model/neuron2.fasl")
      (fasl-save (Loss)    "model/Loss.fasl")
      (fasl-save (gtk_label_get_text ((builder 'get-object) "COUNTER"))
                           "model/Count.fasl")
      (print "saved.") TRUE))



; ==============================================
; show window and run
(define-values (w h) (values '(0) '(0)))
(gtk_window_get_default_size window w h)
(gtk_window_resize window (car w) (+ (car h) 1))

((builder 'connect-signals))
(gtk_widget_show_all window)

; init plot
(plot "set terminal x11 window '"
      (number->string (gtk_socket_get_id socket) 16) "'")
(plot "set notitle")
(plot "unset mouse")
(plot "set yrange [0:1.1]")
(plot "set key right bottom")
(plot "clear")
(test #f)

(define quit
   (GTK_CALLBACK (widget userdata)
      (await (mail 'walker 'quit))
      (gtk_main_quit)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

(gtk_main)
(exit 1)

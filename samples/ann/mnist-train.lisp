#!/usr/bin/env ol

(import (otus ann))

; =================================================
(import (lib gl2))
(import (OpenGL EXT geometry_shader4))

(gl:set-window-title "Sample ANN (mnist database)")
(glShadeModel GL_SMOOTH)
(glClearColor 0.8 0.8 0.8 1)

(import (lib soil))
; -=( сразу нарисуем сплеш )=---------------------------
(glOrtho 0 1 1 0 0 1)
(glEnable GL_TEXTURE_2D)

(define id
   (let ((file (file->bytevector "splash.png")))
      (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_QUADS)
   ; рисуем на весь экран квадратик с текстурой
   (for-each (lambda (xy)
         (glTexCoord2f (car xy) (cdr xy))
         (glVertex2f (car xy) (cdr xy)))
      '((0 . 0) (1 . 0) (1 . 1) (0 . 1)))
(glEnd)
(glDisable GL_TEXTURE_2D)
(gl:SwapBuffers (await (mail 'opengl ['get 'context]))) ; todo: make a function
(glDeleteTextures 1 (list id)) ; и спокойно удалим сплеш текстуру

(define numbers
   (map (lambda (i)
      (let ((file (file->bytevector (fold string-append "" (list "media/" (number->string i 10) ".png")))))
         (SOIL_load_OGL_texture_from_memory file (size file) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0)))
   (iota 10)))

; создадим шейдер превращения точек в квадратики
(define po (glCreateProgram))
(define vs (glCreateShader GL_VERTEX_SHADER))
(define gs (glCreateShader GL_GEOMETRY_SHADER))
(define fs (glCreateShader GL_FRAGMENT_SHADER))

(glShaderSource vs 1 (list "
   #version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;
   }") #false)
(glCompileShader vs)
(glAttachShader po vs)

; more info: https://www.khronos.org/opengl/wiki/Geometry_Shader_Examples
(glShaderSource gs 1 (list "
   #version 120
   #extension GL_EXT_geometry_shader4 : enable

   void main()
   {
      gl_Position = gl_PositionIn[0];
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();

      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(1.0, 0.0, 0.0, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();

      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(0.0, 1.0, 0.0, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();

      gl_Position = gl_PositionIn[0] + gl_ModelViewProjectionMatrix * vec4(1.0, 1.0, 0.0, 0.0);
      gl_FrontColor = gl_FrontColorIn[0];
      EmitVertex();
   }") #false)
(glCompileShader gs)
(glAttachShader po gs)
(glProgramParameteri po GL_GEOMETRY_INPUT_TYPE GL_POINTS)
(glProgramParameteri po GL_GEOMETRY_OUTPUT_TYPE GL_TRIANGLE_STRIP) ; only POINTS, LINE_STRIP and TRIANGLE_STRIP is allowed
(glProgramParameteri po GL_GEOMETRY_VERTICES_OUT 4)

(glShaderSource fs 1 (list "
   #version 120 // OpenGL 2.1
   void main(void) {
      gl_FragColor = gl_Color;
   }
") #false)
(glCompileShader fs)
(glAttachShader po fs)

(glLinkProgram po)

(glDetachShader po fs)
(glDetachShader po gs)
(glDetachShader po vs)


(print "Please wait while loading a training database")

; --= mnist data =----------
(import (file gzip))
(import (owl parse))

(define-library (file mnist)
   (import (otus lisp)
      (file parser))
   (export
      train-labels-parser
      train-images-parser)
(begin

   (define uint32
      (let-parse* (
            (a0 byte)
            (a1 byte)
            (a2 byte)
            (a3 byte))
         (+     a3
            (<< a2  8)
            (<< a1 16)
            (<< a0 24))))

   (define train-labels-parser
      (let-parse* (
            (magic (times 4 byte))
            (verify (equal? magic '(#x00 #x00 #x08 #x01)) 'not-a-mnist-labels-file)
            (number-of-labels uint32)
            (labels (times number-of-labels byte)))
         {
            'magic magic
            'number-of-labels number-of-labels
            'labels labels
         }))

   (define train-images-parser
      (let-parse* (
            (magic (times 4 byte))
            (verify (equal? magic '(#x00 #x00 #x08 #x03)) 'not-a-mnist-images-file)
            (number-of-images uint32)
            (number-of-rows uint32)
            (number-of-columns uint32)
            ;(number-of-images (epsilon 1000)) ; tmp
            (images (times number-of-images (times (* number-of-rows number-of-columns) byte))))
         {
            'magic magic
            'number-of-images number-of-images
            'number-of-rows number-of-rows
            'number-of-columns number-of-columns
            'images images
         }))
))
(import (file mnist))

(define mnist-root (or
   (and (not (null? (command-line))) (car (command-line)))
   "/media/uri/1TB/DATA/mnist/"))

;; read the data
(define labels-file (try-parse gzip-parser (file->bytestream (string-append mnist-root "train-labels-idx1-ubyte.gz")) #f))

(define labels (try-parse train-labels-parser ((car labels-file) 'stream) #f))
(define labels (car labels))

(define images-file (try-parse gzip-parser (file->bytestream (string-append mnist-root "train-images-idx3-ubyte.gz")) #f))

(define images (try-parse train-images-parser ((car images-file) 'stream) #f))
(define images (car images))

(define images-count (length (images 'images)))

; ---------------- картинки прочитали? хорошо...
(import (scheme dynamic-bindings))

(define *l0-l1* (make-parameter #f))
(define *state* (make-parameter #f))

(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)
   (glUseProgram po)

   (define state (*state*))
   (when state

      ; цифра (в левой части поля)
      (define digit (ref state 1))

      (define m 28) ;(length digit))
      (define n 28) ;(length (car digit)))
      (glLoadIdentity)
      (glOrtho 0 (* m 2) n 0 -1 1)

      (glBegin GL_POINTS)
      (for-each (lambda (p)
            (define i (mod p m))
            (define j (div p m))

            (define cell (at digit 1 p))
            (glColor3f cell cell cell)
            (glVertex2f i j))
         (iota (* m n) 1))
      (glEnd)

      ; правая-верхняя часть поля
      (glLoadIdentity)
      (glOrtho -10 +10 -10 10 -1 1)

      (define label (ref state 2))
      (define guess (ref state 3))

      ; настоящая цифра
      (glUseProgram 0)
      (glEnable GL_TEXTURE_2D)

      (for-each (lambda (j)
            (if (eq? label j)
               (glColor3f 1 1 1)
               (glColor3f 0 0 0))

            (glBindTexture GL_TEXTURE_2D (lref numbers j))
            (glBegin GL_QUADS)
               (glTexCoord2f 0 1)
               (glVertex2f    j    7)
               (glTexCoord2f 1 1)
               (glVertex2f (+ j 1) 7)
               (glTexCoord2f 1 0)
               (glVertex2f (+ j 1) 8)
               (glTexCoord2f 0 0)
               (glVertex2f    j    8)
            (glEnd))
         (iota 10))

      (glDisable GL_TEXTURE_2D)
      (glUseProgram po)

      ; что хочет сказать нейросеть
      (glBegin GL_POINTS)
      (for-each (lambda (j)
            (define g (at guess 1 (+ j 1)))
            (glColor3f g g g)
            
            (glVertex2f j 9)
            (cond
               ((and (< g 0.5) (not (eq? label j)))
                  (glColor3f 0 (- 1 (* 2 g)) 0))
               ((and (> g 0.5) (and (eq? label j)))
                  (glColor3f 0 (- (* 2 g) 1)  0))
               ((and (< g 0.5) (and (eq? label j)))
                  (glColor3f (- 1 (* 2 g)) 0 0))
               ((and (> g 0.5) (not (eq? label j)))
                  (glColor3f (- (* 2 g) 1) 0 0))
               (else
                  (glColor3f 0 0 0)))
            (glVertex2f j 8))
         (iota 10))
      (glEnd))

   ; нарисуем первый слой нейносети
   (define l0-l1 (*l0-l1*))
   (when l0-l1
      (define l0 (car l0-l1))
      (define l1 (cdr l0-l1))

      ; первый слой нейросети
      (glLoadIdentity)
      (glOrtho (- (ref l0 1)) (ref l0 1) (* 1 (ref l0 2)) (* 1 (- (ref l0 2))) -1 1)

      (glBegin GL_POINTS)
      (for-each (lambda (i)
            (for-each (lambda (j)
                  (define color (at l0 i j))
                  (glColor3f color color color)

                  (glVertex2f i j))
               (iota (ref l0 2))))
         (iota (ref l0 1)))
      (glEnd)
      
      (glLoadIdentity)
      (glOrtho (- (ref l1 1)) (ref l1 1) (* 4 (- (ref l1 2))) (* 4 (ref l1 2)) -1 1)

      (glBegin GL_POINTS)
      (for-each (lambda (i)
            (for-each (lambda (j)
                  (define color (at l1 i j))
                  (glColor3f color color color)

                  (glVertex2f i j))
               (iota (ref l1 2))))
         (iota (ref l1 1)))
      (glEnd))

))


(gl:set-mouse-handler (lambda (button x y)
   (when (eq? button 1)
      (define l0-l1 (*l0-l1*))
      (when l0-l1
         (display "Dumping current network state to disk ...")
         (write-matrix (car l0-l1) "syn0")
         (write-matrix (cdr l0-l1) "syn1")
         (print "ok, syn0 and syn1 dumped."))
)))

; наша сеть будет иметь входной слой на rows*columns элементов
; внутренний слой на 128 элоементов
; и выходной на 10
(import (otus random!))

;; ; TODO: читать исходную картинку как [m*n], и добавить отдельный преобразующий слой без математики, который будет создавать
;; ; новую матрицу [1 m*n тот-же-вектор-флоатов] (без копирования)
;; ; это относится к топологии нейросети, а не к ее состоянию

; попробуем создать новую сеть через новый api
(define ann (make-ann
   (make-dense-layer 10 σ ; второй промежуточный слой
   (make-dense-layer 64 σ ; первый промежуточный слой ; 128
   (make-input-layer (* 28 28))))))

; прочитаем состояние из файлов
(read-matrix! (get-layer ann 1) "syn0")
(read-matrix! (get-layer ann 2) "syn1")

; обучение сети
(coroutine 'ann (lambda ()
   (print "запуcкаю обучение сети")
   (let this ((n 0))
      (let*((i (rand! images-count))
            (image (lref (images 'images) i))
            (label (lref (labels 'labels) i))

            (X (list->matrix image 255)) ; сразу нормализуем картинку в диапазон [0..1]
            (Y (list->matrix   ; а ожидаемый результат - в набор ответов (0,1,2,3,.. 9)
                  (map (lambda (p) (if (eq? p label) 1 0))
                     (iota 10))))

            ; процесс вычисления нейросети
            (eva (evaluate ann X))
            ; а теперь ее обучим:
            (_ (backpropagate! eva (sub Y (caar eva))))

            ; передадим наше состояние для отображения на экране
            (_ (*l0-l1* (cons
                  (get-layer ann 1)
                  (get-layer ann 2))))
            (_ (*state* [X label (caar eva)])))
         ; 
         (when (zero? (mod n 100))
            (print
               "     used memory: " (inexact (/ (* 8 (ref (syscall 1117) 3)) 1024 1024)) " MiB")
            (sleep 1))

         (this (++ n))))))

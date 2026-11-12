#!/usr/bin/env ol
(import (otus random!))
(import (scheme dynamic-bindings))

; read initial population
(import (owl parse))
(import (file xpm))

(define population (try-parse xpm-parser (file->bytestream
   (or
      (and (pair? (command-line)) (car (command-line)))
      "initial.xpm"))
   #true))

(unless population
   (print "invalid input file")
   (exit 1))

(define population (car population))

; the size of world
(define WIDTH  (+ 20 (population 'width)))
(define HEIGHT (+ 20 (population 'height)))

; the color palette in rgb ([0..1][0..1][0..1])
; todo: change to ff
(define colors (vector-map (lambda (color) (map (lambda (v) (inexact (/ v 255))) color))
   '[(255 0 0) (255 90 0) (255 154 0) (255 206 0) (255 232 8)
   (255 232  18) (255 232  38) (255 232  58) (255 232  78) (255 232  98) (255 232 118) (255 232 138) (255 232 158)
   (255 232 178) (255 232 198) (255 232 218) (255 232 238) (255 232 258)
   ]))

(define (color age)
   (ref colors (min age (size colors))))


(import (lib gl 3.2 compat))
(gl:set-window-title "Convey's The game of Life")

(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; создадим шейдер превращения точек в квадратики
(define po (gl:create-program
;; "#version 120 // OpenGL 2.1
;;    void main() {
;;       gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
;;       gl_FrontColor = gl_Color;
;;    }"
"#version 150 core
   #extension GL_ARB_explicit_attrib_location : enable
   layout(location = 0) in vec4 a_position;
   layout(location = 1) in vec3 a_color;

   out vec4 v_color;

   uniform mat4 u_MVP;

   void main() {
      gl_Position = u_MVP * a_position;
      v_color = vec4(a_color, 1.0);
      // vec4(1.0, 0.0, 0.0, 0.0); //
   }"

;; GL_POINTS GL_TRIANGLE_STRIP 4
"#version 150 core
   layout(points) in;
   layout(triangle_strip, max_vertices = 4) out;

   in vec4 v_color[];
   out vec4 f_color;

   uniform mat4 u_MVP;

   void main() {
      gl_Position = gl_in[0].gl_Position;
      f_color = v_color[0];
      EmitVertex();

      gl_Position = gl_in[0].gl_Position + u_MVP * vec4(1.0, 0.0, 0.0, 0.0);
      f_color = v_color[0];
      EmitVertex();

      gl_Position = gl_in[0].gl_Position + u_MVP * vec4(0.0, 1.0, 0.0, 0.0);
      f_color = v_color[0];
      EmitVertex();

      gl_Position = gl_in[0].gl_Position + u_MVP * vec4(1.0, 1.0, 0.0, 0.0);
      f_color = v_color[0];
      EmitVertex();

      EndPrimitive();
   }"

"#version 150 core
   in vec4 f_color;
   out vec4 frag_color;
   void main(void) {
      frag_color = f_color;
   }
"))

; create initial generation
(define userdata (make-parameter
   (let ((initial population))
      (fold (lambda (ff row y)
               (fold (lambda (ff col x)
                        (if (eq? col #\space)
                           ff
                           (put ff y (put (get ff y {}) x 1))))
                  ff row (iota (initial 'width))))
         #empty
         (initial 'bitmap)
         (iota (initial 'height))))))
;; (print (userdata))
;; (die 2)

(define (alive gen x y)
   ;; (print "-------------------- " x " " y)
   (let ((n (fold (lambda (st dx dy)
                     (+ st (if ((gen (+ y dy) {}) (+ x dx) #f) 1 0)))
               0
               '(-1  0 +1   -1  +1   -1  0 +1)
               '(-1 -1 -1    0   0   +1 +1 +1))))
      ;; (print "  -> " n)
      (cond
         ((eq? n 2)
            ((gen y {}) x #f))
         ((eq? n 3)
            #true))))

(define MinMax (make-parameter [0 (population 'width) 0 (population 'height)]))


(gl:set-renderer (lambda (mouse)
(let ((generation (userdata)))
   (glClear GL_COLOR_BUFFER_BIT)

   ; minmax, count
   (vector-apply
      (ff-fold (lambda (st y row)
            (ff-fold (lambda (st x age)
                  (vector-apply st (lambda (minx maxx miny maxy count)
                     [(min minx x) (max maxx x) (min miny y) (max maxy y) (+ count 1)])))
               st
               row))
         (vector-append (MinMax) [0]) generation)
      (lambda (minx maxx miny maxy count)
         (MinMax [minx maxx miny maxy])

         (glLoadIdentity)
         (glOrtho (- minx 5) (+ maxx 5) (- miny 3) (+ maxy 3) -1 1)
         (gl:set-window-title (string-append
            "Convey's The game of Life ("
            (number->string count)
            " cells)")) ))

   (glUseProgram po)
   (define mvp [
      #i0.0 #i0.0 #i0.0 #i0.0
      #i0.0 #i0.0 #i0.0 #i0.0
      #i0.0 #i0.0 #i0.0 #i0.0
      #i0.0 #i0.0 #i0.0 #i0.0 ])
   (glGetFloatv GL_MODELVIEW_MATRIX mvp)

   (define loc (glGetUniformLocation po "u_MVP"))
   (glUniformMatrix4fv loc 1 GL_FALSE mvp)

   (glBegin GL_POINTS)
      (ff-for-each (lambda (y row)
            (ff-for-each (lambda (x value)
                  (apply glVertexAttrib3f (cons 1 (color value)))
                  (glVertex2f x y))
               row))
         generation)
   (glEnd)

   (define neighbors
      (ff-fold (lambda (st y row)
            (ff-fold (lambda (st x age) ; keep alive cells age
                  (put st y (put (get st y {}) x age)))
               st row))
         (ff-fold (lambda (st y row) ; all neighbors
               (ff-fold (lambda (st x v)
                     (fold (lambda (st dx dy)
                              (define row (get st (+ y dy) {}))
                              (put st (+ y dy) (put row (+ x dx) 0)))
                        st
                        '(-1  0 +1  -1 +1  -1  0 +1)
                        '(-1 -1 -1   0  0  +1 +1 +1)))
                  st row))
            {} generation)
         generation))

   ; make new generation
   (userdata
      (ff-fold (lambda (st y row)
            (ff-fold (lambda (st x age)
                  (if (alive generation x y)
                     (put st y (put (get st y {}) x (+ age 1)))
                     st))
               st
               row))
         {}
         neighbors))

   )))

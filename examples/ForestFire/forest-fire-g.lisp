#!/usr/bin/env ol
(import (otus random!))

(define WIDTH 170)
(define HEIGHT 96)

; probabilities
(define p 20)
(define f 1000)

(import (lib gl-2))
(gl:set-window-title "Drossel and Schwabl 'forest-fire'")
(import (OpenGL EXT geometry_shader4))
(unless EXT_geometry_shader4
   (runtime-error "Geometry shaders are not supported."))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)
(glOrtho 0 WIDTH 0 HEIGHT 0 1)

; создадим шейдер превращения точек в квадратики
(define po (gl:create-program
"#version 120 // OpenGL 2.1
   void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      gl_FrontColor = gl_Color;
   }"

GL_POINTS GL_TRIANGLE_STRIP 4
"#version 120
   #extension GL_EXT_geometry_shader4 : enable

   // more info: https://www.khronos.org/opengl/wiki/Geometry_Shader_Examples
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
   }"

"#version 120 // OpenGL 2.1
   void main(void) {
      gl_FragColor = gl_Color;
   }
"))

;; (gl:set-resize-handler (lambda (width height)
;;    (glViewport 0 0 width height)
;;    (glPointSize (/ width WIDTH))))

(import (scheme dynamic-bindings))
(define userdata (make-parameter
   (make-vector (map (lambda (-) (make-vector (map (lambda (-) (rand! 2)) (iota WIDTH)))) (iota HEIGHT)))))

(gl:set-renderer (lambda (mouse)
   (let ((forest (userdata))
         (step (make-vector (map (lambda (-) (make-vector (repeat 0 WIDTH))) (iota HEIGHT)))))
      (glClear GL_COLOR_BUFFER_BIT)

      (glUseProgram po)
      (glBegin GL_POINTS)
         (for-each (lambda (y)
               (for-each (lambda (x)
                     (case (ref (ref forest y) x)
                        (0 ; An empty space fills with a tree with probability "p"
                           (if (zero? (rand! p))
                              (set-ref! (ref step y) x 1)))
                        (1
                           (glColor3f 0.2 0.7 0.2)
                           (glVertex2f x y)
                           ; A tree will burn if at least one neighbor is burning
                           ; A tree ignites with probability "f" even if no neighbor is burning
                           (if (or (eq? (ref (ref forest (- y 1)) (- x 1)) 2)  (eq? (ref (ref forest (- y 1))    x)    2)  (eq? (ref (ref forest (- y 1)) (+ x 1)) 2)
                                   (eq? (ref (ref forest    y   ) (- x 1)) 2)                                              (eq? (ref (ref forest    y   ) (+ x 1)) 2)
                                   (eq? (ref (ref forest (+ y 1)) (- x 1)) 2)  (eq? (ref (ref forest (+ y 1))    x)    2)  (eq? (ref (ref forest (+ y 1)) (+ x 1)) 2)
                                   (zero? (rand! f)))
                              (set-ref! (ref step y) x 2)
                              (set-ref! (ref step y) x 1)))
                        (2
                           (glColor3f 0.7 0.7 0.1)
                           (glVertex2f x y))
                           ; A burning cell turns into an empty cell
                           (set-ref! (ref step y) x 0)))
                  (iota WIDTH)))
            (iota HEIGHT))
      (glEnd)
      (userdata step))))

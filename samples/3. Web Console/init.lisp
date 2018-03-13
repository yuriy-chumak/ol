(import (lib opengl))
(opengl:init "test!")

; --
; https://github.com/kripken/emscripten/blob/master/tests/glbook/Chapter_2/Hello_Triangle/Hello_Triangle.c
(glClearColor 0.3 0.3 0.3 1)
(glClear GL_COLOR_BUFFER_BIT)

(define (CreateProgram vstext fstext)
(let ((po (glCreateProgram))
      (vs (glCreateShader GL_VERTEX_SHADER))
      (fs (glCreateShader GL_FRAGMENT_SHADER)))
   (if (= po 0)
      (runtime-error "Can't create shader program." #f))

   ; пример, как можно передать в функцию массив указателей на строки:
   ; vertex shader:
   ; http://steps3d.narod.ru/tutorials/lighting-tutorial.html
   (glShaderSource vs 1 (list (c-string vstext)) #false)
   (glCompileShader vs)
   (let ((isCompiled '(0)))
      (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

      (if (eq? (car isCompiled) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog vs))))
   (glAttachShader po vs)

   ; fragment shader:
   (glShaderSource fs 1 (list (c-string fstext)) #false)
   (glCompileShader fs)
   (let ((isCompiled '(0)))
      (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

      (if (eq? (car isCompiled) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog fs))))

   (glAttachShader po fs)

   (glLinkProgram po)
   (glDetachShader po fs)
   (glDetachShader po vs)

   po ; return result index
))

(define program (CreateProgram
"  attribute vec4 vPosition;
   void main()
   {
      gl_Position = vPosition;
   }"

"  precision mediump float;
   void main()
   {
      gl_FragColor = vec4( 0.0, 0.3, 0.0, 1.0 );
   }"))

(print "program: " program)

(define pos (glGetAttribLocation program "vPosition"))
(print "pos: " pos)

(define vpo '(0))
(glGenBuffers 1 vpo)
(print "vpo: " vpo)
(glBindBuffer GL_ARRAY_BUFFER (car vpo))

(define vertices '(0.0 0.5  -0.5 -0.5  0.5 -0.5))
(glBufferData GL_ARRAY_BUFFER (* 4 (length vertices)) vertices GL_STATIC_DRAW)


(glUseProgram program)
(glBindBuffer GL_ARRAY_BUFFER (car vpo))
(glVertexAttribPointer pos 2 GL_FLOAT GL_FALSE 0 #false)
(glEnableVertexAttribArray pos)
(glDrawArrays GL_TRIANGLES 0 3)

(print "ok.")

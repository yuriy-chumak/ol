#!/bin/ol

(define SHADER_NUM 1)
(define width 640)
(define height 480)


; http://www.geeks3d.com/20120102/programming-tutorial-simple-x11-x-window-code-sample-for-linux-and-mac-os-x/

; http://www.eg.bucknell.edu/~cs367/glx/xintro.html
(import (owl pinvoke) (owl io)
   (lib x11)
   (OpenGL version-2-0)
)

(define OR (lambda args (fold bor 0 args)))

;(main)
(define dpy (XOpenDisplay 0))
(define s (XDefaultScreen dpy))

(define vi (glXChooseVisual dpy s
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
   
      0 0 0  0  )))); None
(define cx (glXCreateContext dpy vi 0 1))

(define win (XCreateSimpleWindow dpy (XRootWindow dpy s)
   0 0 width height 1
   (XBlackPixel dpy s) (XWhitePixel dpy s)))
   
(XSelectInput dpy win (OR ExposureMask KeyPressMask))
(XMapWindow dpy win)

;(help)
(define (file->string path)
   (bytes->string
      (vec-iter
         (let ((vec (file->vector path)))
            (if vec vec
               (error "Unable to load: " path))))))


;(init)
(glXMakeCurrent dpy win cx)

(define po (glCreateProgram))
(print "po: " po)

(define vs (glCreateShader GL_VERTEX_SHADER))
(print "vs: " vs)


; пример, как можно передать в функцию массив указателей на строки:
(glShaderSource vs 2 (tuple (c-string "#version 120 // OpenGL 2.1\n")
                            (c-string "
    void main() {
        gl_Position = gl_Vertex; // - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewMatrix * gl_Vertex
    }")) null)
(glCompileShader vs)
  (define isCompiled "word")
  (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "word")
      (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)
      (print errorLog)
      (halt 0)))
(glAttachShader po vs)

;; полезные шейдеры:
;;  http://glslsandbox.com/e#19171.3 - цифровое табло
(define fs (glCreateShader GL_FRAGMENT_SHADER))
(glShaderSource fs 1 (tuple (c-string (file->string
  (case SHADER_NUM
    (2 "raw/geometry.fs")
    (3 "raw/water.fs")
    (4 "raw/18850")
    (5 "raw/minecraft.fs")
    (6 "raw/moon.fs")
    (0 "raw/black.fs")
    (1 "raw/itsfullofstars.fs"))))) null)
(glCompileShader fs)
  (define isCompiled "word")
  (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "word")
      (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)
      (print errorLog)
      (print "@")
      (halt 0)))
(glAttachShader po fs)

(glLinkProgram po)
(glDetachShader po fs)
(glDetachShader po vs)

  (define time (glGetUniformLocation po (c-string "time")))
  (define resolution (glGetUniformLocation po (c-string "resolution")))

(glShadeModel GL_SMOOTH)
(glClearColor 0 0 0 1.0)

(glXMakeCurrent dpy null null)







(define XEvent (raw type-vector-raw (repeat 0 192)))

(let loop ()
   (let process-events ()
      (if (> (XPending dpy) 0)
         (begin
            (print "xevent got")
            (XNextEvent dpy XEvent)
            (process-events))))

      (glXMakeCurrent dpy win cx)
      (glClear GL_COLOR_BUFFER_BIT)

      (glUseProgram po)

      (let* ((ss ms (clock)))
        (glUniform1f time (+ (/ ms 1000) (mod ss 3600)))) ; раз в час будем сбрасывать период
      (if (> resolution 0)
        (glUniform2f resolution width height))

      (glBegin GL_TRIANGLE_STRIP)
        (glVertex2f -1 -1)
        (glVertex2f +1 -1)
        (glVertex2f -1 +1)
        (glVertex2f +1 +1)
      (glEnd)

      ;(glEnableVertexAttribArray 0)
      ;(glVertexAttribPointer 0 4 GL_FLOAT 0 0 vertexPositions)
      ;(glDrawArrays GL_TRIANGLES 0 3)

      (glUseProgram 0)
      (glXSwapBuffers dpy win)
      (glXMakeCurrent dpy null null)
   (loop))
;(loop)

;    if ((e.type == ClientMessage) && 
;        (static_cast<unsigned int>(e.xclient.data.l[0]) == WM_DELETE_WINDOW))
;    {
;      break;
;    }

(print s)
(print "Ok.")

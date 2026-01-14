(define-library (lib gl-2 vr)
(import
   (scheme base)
   (lib gl config)
   (lib gl)
   (OpenGL 2.1))
(export
   VR_MODELVIEW
   VR_PROJECTION
   VR_MODELVIEW_MATRIX
   VR_PROJECTION_MATRIX

   VR_WIDTH  VR_HEIGHT
   ; todo: add (glGetIntegerv GL_DRAW_FRAMEBUFFER_BINDING &framebuffer) as public api

   gl:enable-vr
   gl:eye-matrix)

(begin
   ; glMatrixMode
   (define VR_PROJECTION (+ #x10000 GL_PROJECTION))
   (define VR_MODELVIEW  (+ #x10000 GL_MODELVIEW))

   ; glGetFloatv
   (define VR_PROJECTION_MATRIX (+ #x10000 GL_PROJECTION_MATRIX))
   (define VR_MODELVIEW_MATRIX  (+ #x10000 GL_MODELVIEW_MATRIX))

   (define VR_WIDTH 2047)
   (define VR_HEIGHT 511)

   (import (OpenGL ARB framebuffer_object)) ; TODO: try to avoid extension usage
   (import (owl io)) ; temp

   (import (otus async))

   ; #false - disable VR mode
   ; 'side-by-side
   ; 'cross-eye
   ; 
   ; 'VrApi (Oculus Vr API)
   ;  https://developers.meta.com/horizon/downloads/package/oculus-mobile-sdk/

   ; -=( vrApi )=---------------------------------
   (define (vrApi)
      ; native app is required
      (define MAIN (load-dynamic-library "libmain.so")) ; android shared code (todo: move to gl4es)
      (define anlProcessEvents (MAIN fft-void "anlProcessEvents"))
      (define anlSwapBuffers (MAIN fft-void "anlSwapBuffers"))

      (define vr-begin (MAIN fft-void "begin"))
      (define vr-update (MAIN fft-void "update" fft-int))
      (define vr-flush (MAIN fft-void "flush"))
      (define vr-end (MAIN fft-void "end"))

      (print "vr-begin: " vr-begin)

      (mail 'opengl ['set 'vr-begin vr-begin])
      (mail 'opengl ['set 'vr-eye vr-update])
      (mail 'opengl ['set 'vr-flush vr-flush])
      (mail 'opengl ['set 'vr-end vr-end])
      
      ;; ...
      (mail 'opengl ['set 'vr-mode 'vrApi])
      ; autorender for the native application must be true
      (mail 'opengl ['set 'autorender #T]))

   ; -=( headset )=-------------------------------

      ; create eye framebuffer
      (define (make-eye TEXW TEXH)
         (define framebuffer (box 0))
         (glGenFramebuffers 1 framebuffer)
         (define texture (box 0))
         (glGenTextures 1 texture)

         (glBindTexture GL_TEXTURE_2D (unbox texture))
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
         (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA TEXW TEXH 0 GL_RGBA GL_UNSIGNED_BYTE #f)
         (glBindTexture GL_TEXTURE_2D 0) ; done

         (glBindFramebuffer GL_FRAMEBUFFER (unbox framebuffer))
         (glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (unbox texture) 0)

         ; we have to create hardware depth buffer, if we want to use a depth
         (define depthrenderbuffer (box 0))
         (glGenRenderbuffers 1 depthrenderbuffer)
         (glBindRenderbuffer GL_RENDERBUFFER (unbox depthrenderbuffer))
         (glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT TEXW TEXH)
         (glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER (unbox depthrenderbuffer))
         (glBindFramebuffer GL_FRAMEBUFFER 0) ; done

         [(unbox framebuffer) (unbox texture)])


   (define (Headset) ; cheap lenses for android
      ;; ; 1. create two framebuffers, for the left and for the right eyes
      ;; ; todo: make buffer size depends on window dimensions
      ;; (define TEXW 1024)
      ;; (define TEXH 1024)

   
      ;; ; final render program
      ;; (define show (gl:create-program
      ;; "#version 120 // OpenGL 2.1
      ;;    varying vec2 st;
      ;;    void main() {
      ;;       gl_Position = gl_Vertex;
      ;;    }"
      ;; "#version 120 // OpenGL 2.1
      ;;    uniform sampler2D tex0;
      ;;    uniform sampler2D tex1;

      ;;    uniform vec2 screen;

      ;;    const float alpha = 0.2;
      ;;    const vec2 centre = vec2(0, 0);

      ;;    void main(void) {
      ;;       vec2 xy;
      ;;       float halfw = screen.x / 2.0;
      ;;       if (gl_FragCoord.x < halfw)
      ;;          xy = vec2(gl_FragCoord.x / halfw      , gl_FragCoord.y / screen.y);
      ;;       else
      ;;          xy = vec2(gl_FragCoord.x / halfw - 1.0, gl_FragCoord.y / screen.y);

      ;;       // stereo with distorsion shader
      ;;       vec2 p1 = vec2(2.0 * xy - 1.0) - centre;
      ;;       vec2 p2 = p1 / (1.0 - alpha * length(p1));
      ;;       vec2 p3 = (p2 + centre + 1.0) * 0.5;
      ;;          //p3.y = 1.0 - p3.y;
      ;;       if (gl_FragCoord.x < halfw)
      ;;          gl_FragColor = texture2D(tex0, p3);
      ;;       else
      ;;          gl_FragColor = texture2D(tex1, p3);
      ;;    }"
      ;; ))
      ;; (glUniform1i (glGetUniformLocation show "tex0") 0)
      ;; (glUniform1i (glGetUniformLocation show "tex1") 1)

      ;; (define eyes [(make-eye) (make-eye)])
      ;; (print "eyes: " eyes)

      ;; (define depth-test (box 0))
      ;; (define clear-color (box #i0 #i0 #i0 #i1))
      ;; (define window (box 0 0 0 0))

      ;; (mail 'opengl ['set 'vr-begin (lambda ()
      ;;    (print "vr-begin")
      ;;    (glGetIntegerv GL_VIEWPORT window) ; save viewport
      ;;    (glPushMatrix)
      ;;    (glPushMatrix)
      ;;    ; todo: save current program
      ;; )])
      ;; ; eyes preparations
      ;; (mail 'opengl ['set 'vr-eye (lambda (eye)
      ;;    (print "vr-eye")
      ;;    (glPopMatrix) (glPushMatrix) ; restore matrix
         
      ;;    (glBindFramebuffer GL_DRAW_FRAMEBUFFER (ref (ref eyes eye) 1))
      ;;    (glViewport 0 0 TEXW TEXH)
      ;;    ;...
      ;; )])

      ;; (mail 'opengl ['set 'vr-finish (lambda ()
      ;;    (print "vr-finish")
      ;;    #f ;; (glFinish)
      ;; )])
      ;; (mail 'opengl ['set 'vr-end (lambda (context)
      ;;    (print "vr-end")
      ;;    (glPopMatrix) (glPopMatrix) ; restore states
      ;;    (apply glViewport window)
      ;;    (glGetIntegerv GL_DEPTH_TEST depth-test)
      ;;    (glGetFloatv GL_COLOR_CLEAR_VALUE clear-color)

      ;;    ;(print "vrEnd")
      ;;    (glBindFramebuffer GL_DRAW_FRAMEBUFFER 0)
      ;;    (glClearColor 0 0 0 1)
      ;;    (glDisable GL_DEPTH_TEST) ; TODO: restore state
      ;;    (glClear GL_COLOR_BUFFER_BIT)

      ;;    (glUseProgram show)
      ;;    (glActiveTexture GL_TEXTURE0)
      ;;    (glBindTexture GL_TEXTURE_2D (ref (ref eyes 1) 2))
      ;;    (glActiveTexture GL_TEXTURE1)
      ;;    (glBindTexture GL_TEXTURE_2D (ref (ref eyes 2) 2))
      ;;    ; normal eye
      ;;    (when (eq? mode 'side-by-side)
      ;;       (glUniform1i (glGetUniformLocation show "tex0") 0)
      ;;       (glUniform1i (glGetUniformLocation show "tex1") 1))
      ;;    ; cross-eye
      ;;    (when (eq? mode 'cross-eye)
      ;;       (glUniform1i (glGetUniformLocation show "tex0") 1)
      ;;       (glUniform1i (glGetUniformLocation show "tex1") 0))

      ;;    (glUniform2f (glGetUniformLocation show "screen") (list-ref window 2) (list-ref window 3))
      ;;    (glBegin GL_QUADS)
      ;;       (glVertex2f  -1 +1)
      ;;       (glVertex2f  -1 -1)
      ;;       (glVertex2f  +1 -1)
      ;;       (glVertex2f  +1 +1)
      ;;    (glEnd)
      ;;    (glUseProgram 0) ; todo: restore saved program

      ;;    (glFinish)
      ;;    ; anlSwapBuffers?

      ;;    ; restore depth test and clear color
      ;;    (unless (eq? (unbox depth-test) 0)
      ;;       (glEnable GL_DEPTH_TEST))
      ;;    (apply glClearColor clear-color)
      ;; )])

      ;; (mail 'opengl ['set 'vr-mode 'headset])
      #false
   )

   ; -=( openXR )=--------------------------------

   ; -=( stereoscopic )=--------------------------
   ; side-by-side: left eye left imge, right eye right image
   (define (stereo mode)
      #f
   )

   ; todo: cross-eyed mode
   ; todo: vertical stereo-pair
   (define (vertical-stereopair)
      (print "(vertical-stereopair)")
      ; 1. create two framebuffers, for the left and for the right eyes
      ; todo: make buffer size depends on window dimensions
      (define W (+ VR_WIDTH 1))
      (define H (+ VR_HEIGHT 1))

      (define eyes [(make-eye W H) (make-eye W H)])
      (print "eyes: " eyes)

      (define window [(gl:get-window-width) (gl:get-window-height)]) ; todo: remove
      (define &depth-test (box 0))
      
      ;; ; final render program
      (define show (gl:create-program
      "#version 120 // OpenGL 2.1
         void main() {
            gl_Position = gl_Vertex;
         }"
      "#version 120 // OpenGL 2.1
         uniform vec2 view;
         uniform sampler2D tex0;
         uniform sampler2D tex1;
         void main() {
            vec2 st = gl_FragCoord.xy / vec2(view.x, view.y / 2.0);
            if (st.y < 1.0)
               gl_FragColor = texture2D(tex1, st);
            else {
               st.y -= 1.0;
               gl_FragColor = texture2D(tex0, st);
            }
         }" ))
         

      ; subscribe
      (mail 'opengl ['set 'vr-begin (lambda ()
         ; todo: save all matrices?
         ;; (glPushMatrix)
         ;; (glPushMatrix)
         ; todo?: save current program
         #f
      )])
      ; eyes preparations
      (mail 'opengl ['set 'vr-eye (lambda (eye)
         ;; (print "vr-eye " eye)
         ;; (glPopMatrix) (glPushMatrix) ; restore current matrix
         
         (glBindFramebuffer GL_DRAW_FRAMEBUFFER (ref (ref eyes eye) 1))
         (glViewport 0 0 W H)
      )])

      (mail 'opengl ['set 'vr-flush (lambda ()
         ;; (print "vr-flush")
         #f ;; ? (glFinish)
      )])

      (mail 'opengl ['set 'vr-end (lambda ()
         ;; (print "vr-end")
         ;; (glPopMatrix) (glPopMatrix) ; restore states

         (glGetIntegerv GL_DEPTH_TEST &depth-test)
         (glDisable GL_DEPTH_TEST)
         (glDisable GL_CULL_FACE)

         (vector-apply gl:window-dimensions (lambda (x y w h)
            (glViewport x y w h)))
         (glBindFramebuffer GL_DRAW_FRAMEBUFFER 0)

         (glUseProgram show)
         (glUniform2f (glGetUniformLocation show "view") (gl:get-window-width) (gl:get-window-height))
         (glUniform1i (glGetUniformLocation show "tex0") 0)
         (glUniform1i (glGetUniformLocation show "tex1") 1)

         (glActiveTexture GL_TEXTURE1)
         (glBindTexture GL_TEXTURE_2D (ref (ref eyes 2) 2))
         (glActiveTexture GL_TEXTURE0)
         (glBindTexture GL_TEXTURE_2D (ref (ref eyes 1) 2))
         (glBegin GL_QUADS)
            (for-each (lambda (x y)
                  (glVertex2f x y))
               '(-1  1  1 -1)
               '(-1 -1  1  1))
         (glEnd)

         (glUseProgram 0) ; todo: restore saved program
         (glFinish)

         (unless (eq? (unbox &depth-test) 0)
            (glEnable GL_DEPTH_TEST))
      )])

      ; done. enable
      (mail 'opengl ['set 'vr-mode 'vertical-stereopair])
   )

   (define (gl:enable-vr mode)
      (case mode
         (#false ; disable vr mode
            (mail 'opengl ['set 'vr-mode #false]))

         ; Oculus Go, Meta Quest 2
         ('Oculus ; vrApi
            (define vr (box 0))
            (define GL_VR  #x10C33)
            (glGetIntegerv GL_VR vr)
            (print "vr: " vr)

            (case (unbox vr)
               (0 (Headset))
               (1 (vrApi))
               (else
                  (print stderr "Such VR mode is not supported")
                  #false)))
         ('headset
            (Headset))
         ('side-by-side
            (stereo mode))
         ('cross-eyed
            (stereo mode))
         ; 'anaglyph 'crystaleyes 'vertical-stereopair 'vertical-anamorphic .......

         ('vertical-stereopair
            (vertical-stereopair))


            ;(mail 'opengl ...)

         ; vr modes cross-eye etc.
         (else
            (runtime-error "unknown vr mode" mode))))

      ;;    ; X. finally, enable vr mode
      ;;    (mail 'opengl ['set 'vr-mode mode])
      ;; else
      ;;    (mail 'opengl ['set 'vr-mode #false])))

   ;; (define dx #i-0.05)
   (define (gl:eye-matrix eye)
      #f)
   ;;    (case eye
   ;;       (1 [])
   ;;       (2 [])
   ;;       (else [
   ;;          1 0 0 0
   ;;          0 1 0 0
   ;;          0 0 0 1
   ;;          0 0 0 0 ]) ))

))

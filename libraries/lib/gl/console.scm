(define-library (lib gl console)
   (import (otus lisp)
      (lib gl)
      (OpenGL version-1-4)
      (lib freetype))

   (export
      glc:print

      glc:set-color
      RED GREEN BLUE
      WHITE BLACK


      create-window
      set-window-writer
      draw-all-windows
   )
   ; ...

(begin
   (setq cursor '(0 . 0)) ; текущее положение курсора, пока не используется
   (setq fcolor '(0)) ; текущий цвет, тоже пока не используется
   (setq cell-size '(0 . 0)) ; размер знакоместа, пока не используется
   (setq drawing-area (tuple 0 0 0 0))

   ; let's preparte the font
   (define ft (make-FT_Library))
   (setq error (FT_Init_FreeType ft))
   (unless (eq? error 0)
      (runtime-error "Can't access freetype library" error))

   ; load font
   (define face (make-FT_Face))
   (setq error (FT_New_Face ft (c-string "fonts/Anonymous Pro.ttf") 0 face))
   (unless (eq? error 0)
      (runtime-error "Can't load Anonymous Pro font" error))

   ; slot for character bitmaps
   (define slot (face->glyph face))

   (setq error (FT_Set_Pixel_Sizes face 0 16))

   (define (glc:set-color rgb)
      (glPixelMapusv GL_PIXEL_MAP_I_TO_R 256 (iota 256 0 (/ (* 65536 (car rgb)) 256)))
      (glPixelMapusv GL_PIXEL_MAP_I_TO_G 256 (iota 256 0 (/ (* 65536 (cadr rgb)) 256)))
      (glPixelMapusv GL_PIXEL_MAP_I_TO_B 256 (iota 256 0 (/ (* 65536 (caddr rgb)) 256))))
   (define RED '(1 0 0))
   (define GREEN '(0 1 0))
   (define BLUE '(0 0 1))
   (define WHITE '(1 1 1))
   (define BLACK '(0 0 0))


   (define (glc:print x y text)
      (glPixelStorei GL_UNPACK_ALIGNMENT 1)
      (glPixelZoom 1 -1)
      (define viewport '(0 0 0 0))
      (glGetIntegerv GL_VIEWPORT viewport)
      (let do ((x (+ (ref drawing-area 1) x))
               (y (+ (ref drawing-area 2) y))
               (text (string->runes text)))
         (unless (null? text)
            (let ((char (car text)))
               (case char
                  (#\space
                     (do (+ x 1) y (cdr text)))
                  (else
                     (FT_Load_Char face char FT_LOAD_RENDER)
                     (let ((bitmap (glyph->bitmap slot)))
                        (glWindowPos2iv (list
                           (+ (* x 9) (ref bitmap 1))
                           (- (- (lref viewport 3) (* y 16)) (- 16 (ref bitmap 3)))))
                        (glDrawPixels (ref bitmap 2) (ref bitmap 4) GL_COLOR_INDEX GL_UNSIGNED_BYTE (ref bitmap 5))
                        (do (+ x 1) y (cdr text)))))))))

   (fork-server 'windows (lambda ()
      (let this ((itself #empty))
         (let*((envelope (wait-mail))
               (sender msg envelope))
            (tuple-case msg
               ; low level interaction interface
               ((set key data)
                  (let ((itself (put itself key data)))
                     (this itself)))
               ((get key)
                  (mail sender (get itself key #false))
                  (this itself))
               ((debug)
                  (mail sender itself)
                  (this itself))

               ((create x y width height)
                  (let*((id (get itself 'id 1))
                        (itself (put itself 'id (+ id 1)))
                        (itself (put itself id (tuple x y width height #false))))
                     (mail sender id)
                     (this itself)))
               ((set-window-writer id writer)
                  (let*((window (get itself id #false))
                        (itself (unless window itself
                           (put itself id (set-ref window 5 writer)))))
                     (this itself)))

               ((draw)
                  (let loop ((ff (ff-iter itself)))
                     (unless (null? ff)
                        (let*((window (cdar ff))
                              (writer (if window (ref window 5))))
                           (if writer
                              (let ((vp '(0 0 0 0)))
                                 ;; (glGetIntegerv GL_VIEWPORT vp)
                                 ;; (glViewport
                                 ;;    (* (ref window 1) 9)
                                 ;;    (* (ref window 2) 16)
                                 ;;    (* (ref window 3) 9)
                                 ;;    (* (ref window 4) 16))
                                 ;; (glClear GL_COLOR_BUFFER_BIT)
                                 ;; (glViewport (lref vp 0) (lref vp 1) (lref vp 2) (lref vp 3)) ; restore viewport

                                 (set-ref! drawing-area 1 (ref window 1))
                                 (set-ref! drawing-area 2 (ref window 2))
                                 (writer)))
                           (loop (force (cdr ff))))))
                  (mail sender 'ok)
                  (this itself))

               (else
                  (print-to stderr "Unknown windows command: " msg)
                  (this itself)))))))

   (define (create-window x y width height)
      (interact 'windows (tuple 'create x y width height)))

   (define (set-window-writer id writer)
      (mail 'windows (tuple 'set-window-writer id writer)))

   (define (draw-all-windows)
      (let ((itself (interact 'windows (tuple 'debug))))
         ;; (let loop ((ff (ff-iter itself)))
         ;;    (unless (null? ff)
         ;;       (let*((window (cdar ff))
         ;;             (writer (if window (ref window 5))))
         ;;          (if writer (begin
         ;;             ;(set-ref! drawing-area 1 (ref window 1))
         ;;             ;(set-ref! drawing-area 2 (ref window 2))
         ;;             (writer)))
         ;;          (loop (force (cdr ff))))))
      #true))
))
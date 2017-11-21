(define *include-dirs* (cons ".." *include-dirs*))
;(define-library (lib sdl2)
 (import
  (otus lisp) (otus ffi))

; (export
;   ;SDL_error
;   SDL_GetError
;
;   ;SDL_main
;   SDL_Init
;      SDL_INIT_VIDEO
;
;   ;SDL_video
;   SDL_CreateWindow
;   )

;(begin
(define uname (uname))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))

(define WIDTH 1280)
(define HEIGHT 920)

(define % (dlopen (cond
   (win32? "SDL2.dll")
   (linux? "libsdl2.so")
   (else (runtime-error "No sdl2 library support" "Unknown platform")))))

(if (not %)
   (runtime-error "Can't load sdl2 library." (cond
      (win32?
         "Download dll from https://www.libsdl.org/download-2.0.php")
      (linux?
         "Use, for example, sudo apt install libsdl2"))))


; ===================================================
; helper function
(define bitwise-ior
   (case-lambda
      ((a b) (bor a b))
      ((a) a)
      ((a . bs) (fold bor a bs))))

; ------------------------
; SDL_error
(define SDL_GetError (dlsym % type-string "SDL_GetError"))


; ------------------------
; SDL_main
(define SDL_Init  (dlsym % type-integer "SDL_Init"  type-integer))
   (define SDL_INIT_VIDEO          #x00000020)


; ------------------------
; SDL_surface
(define SDL_Surface* type-vptr)

(define SDL_FreeSurface (dlsym % type-void "SDL_FreeSurface" SDL_Surface*))

; ------------------------
; SDL_video
(define SDL_Window* type-vptr)

(define SDL_WINDOWPOS_UNDEFINED_MASK    #x1FFF0000)
;(define SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)  (SDL_WINDOWPOS_UNDEFINED_MASK|(X))
(define SDL_WINDOWPOS_UNDEFINED         (bor SDL_WINDOWPOS_UNDEFINED_MASK 0))


(define SDL_CreateWindow (dlsym % SDL_Window* "SDL_CreateWindow" type-string type-integer type-integer type-integer type-integer type-integer))
   (define SDL_WINDOW_SHOWN #x00000004)

(define SDL_GetWindowSurface (dlsym % SDL_Surface* "SDL_GetWindowSurface" SDL_Window*))

; ------------------------
; SDL_render
(define SDL_Renderer* type-vptr)
(define SDL_Texture* type-vptr)
(define SDL_Rect* type-vptr)

(define SDL_CreateRenderer (dlsym % SDL_Renderer* "SDL_CreateRenderer" SDL_Window* type-integer type-integer))
   (define SDL_RENDERER_SOFTWARE #x00000001)          ;/**< The renderer is a software fallback */
   (define SDL_RENDERER_ACCELERATED #x00000002)       ;/**< The renderer uses hardware acceleration */
   (define SDL_RENDERER_PRESENTVSYNC #x00000004)      ;/**< Present is synchronized with the refresh rate */
   (define SDL_RENDERER_TARGETTEXTURE #x00000008)     ;/**< The renderer supports

(define SDL_RenderClear (dlsym % type-integer "SDL_RenderClear" SDL_Renderer*))
(define SDL_RenderPresent (dlsym % type-void "SDL_RenderPresent" SDL_Renderer*))
(define SDL_RenderCopy (dlsym % type-int "SDL_RenderCopy" SDL_Renderer* SDL_Texture* SDL_Rect* SDL_Rect*))

(define SDL_CreateTextureFromSurface (dlsym % SDL_Texture* "SDL_CreateTextureFromSurface" SDL_Renderer* SDL_Surface*))

; ------------------------
; SDL_event
(define SDL_Event* type-vector-raw)
(define (make-SDL_Event) (vm:new-raw-object type-vector-raw 56))

(define SDL_PollEvent (dlsym % type-fix+ "SDL_PollEvent" SDL_Event*))
   (define SDL_QUIT #x100)
;))

; ========================
; SDL_image
(define % (dlopen (cond
   (win32? "SDL2_image.dll")
   (linux? "libsdl2-image.so")
   (else (runtime-error "No sdl2 image library support" "Unknown platform")))))

(if (not %)
   (runtime-error "Can't load sdl2 image library." (cond
      (win32?
         "Download dll from https://www.libsdl.org/projects/SDL_image/")
      (linux?
         "Use, for example, sudo apt install libsdl2-image"))))

(define IMG_Init (dlsym % type-integer "IMG_Init" type-integer))
   (define IMG_INIT_JPG #x00000001)
   (define IMG_INIT_PNG #x00000002)
   (define IMG_INIT_TIF #x00000004)
   (define IMG_INIT_WEBP #x00000008)

(define IMG_Load (dlsym % SDL_Surface* "IMG_Load" type-string))

; test:
; ***************************************************
(if (less? (SDL_Init SDL_INIT_VIDEO) 0)
   (begin
      (print "Unable to Init SDL: " (SDL_GetError))
      (exit-owl 1)))

(unless (eq? (IMG_Init IMG_INIT_PNG) IMG_INIT_PNG)
   (begin
      (print "Unable to init SDL png image support: " (SDL_GetError))
      (exit-owl 1)))

(define window (SDL_CreateWindow "Create SDL2 Window sample"
   SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
   640 480 SDL_WINDOW_SHOWN))

;(define surface (SDL_GetWindowSurface window))
;(define loaded-surface (IMG_Load "SDL_logo.png"))
;(define optimized-surface (SDL_ConvertSurface 
(define renderer (SDL_CreateRenderer window -1 (bor SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))

(define picture (IMG_Load "SDL_logo.png"))
(define texture (SDL_CreateTextureFromSurface renderer picture))
(SDL_FreeSurface picture)

(call/cc (lambda (break)
   (let loop ()
      (let ((event (make-SDL_Event)))
         (let event-loop ()
            (unless (eq? (SDL_PollEvent event) 0)
               (let ((type (int32->ol event 0)))
                  (case type
                     (SDL_QUIT (break))
                     (else
                        #f))
                  (event-loop)))))
      (SDL_RenderClear renderer)
      (SDL_RenderCopy renderer texture #f #f)
      (SDL_RenderPresent renderer)
      (loop))))


;(SDL_DestroyRenderer renderer)
;(SDL_DestroyWindow window)
;(SDL_Quit)

(print "done.")

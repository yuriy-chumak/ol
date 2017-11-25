(define *include-dirs* (cons ".." *include-dirs*))
(define-library (lib sdl2)
 (import
  (otus lisp) (otus ffi))

 (export
   ;SDL_error
   SDL_GetError SDL_ClearError

   ;SDL_main
   SDL_Init
      SDL_INIT_VIDEO

   ;SDL_surface
   SDL_FreeSurface

   ;SDL_video
   SDL_CreateWindow
      SDL_WINDOWPOS_UNDEFINED_MASK
      SDL_WINDOWPOS_UNDEFINED

      SDL_WINDOW_FULLSCREEN
      SDL_WINDOW_OPENGL
      SDL_WINDOW_SHOWN

   SDL_GetWindowSurface
   SDL_GL_SetAttribute
      SDL_GL_RED_SIZE
      SDL_GL_GREEN_SIZE
      SDL_GL_BLUE_SIZE
      SDL_GL_ALPHA_SIZE
      SDL_GL_BUFFER_SIZE
      SDL_GL_DOUBLEBUFFER
      SDL_GL_DEPTH_SIZE
      SDL_GL_STENCIL_SIZE
      SDL_GL_ACCUM_RED_SIZE
      SDL_GL_ACCUM_GREEN_SIZE
      SDL_GL_ACCUM_BLUE_SIZE
      SDL_GL_ACCUM_ALPHA_SIZE
      SDL_GL_STEREO
      SDL_GL_MULTISAMPLEBUFFERS
      SDL_GL_MULTISAMPLESAMPLES
      SDL_GL_ACCELERATED_VISUAL
      SDL_GL_RETAINED_BACKING
      SDL_GL_CONTEXT_MAJOR_VERSION
      SDL_GL_CONTEXT_MINOR_VERSION
      SDL_GL_CONTEXT_EGL
      SDL_GL_CONTEXT_FLAGS
      SDL_GL_CONTEXT_PROFILE_MASK
      SDL_GL_SHARE_WITH_CURRENT_CONTEXT
      SDL_GL_FRAMEBUFFER_SRGB_CAPABLE
      SDL_GL_CONTEXT_RELEASE_BEHAVIOR
      SDL_GL_CONTEXT_RESET_NOTIFICATION
      SDL_GL_CONTEXT_NO_ERROR
   ;SDL_GL_GetAttribute
   SDL_GL_CreateContext
   SDL_GL_SetSwapInterval
   SDL_GL_SwapWindow

   ;SDL_render
   SDL_CreateRenderer
      SDL_RENDERER_SOFTWARE
      SDL_RENDERER_ACCELERATED
      SDL_RENDERER_PRESENTVSYNC
      SDL_RENDERER_TARGETTEXTURE

   SDL_RenderClear
   SDL_RenderPresent
   SDL_RenderCopy

   SDL_CreateTextureFromSurface

   ;SDL_event
   make-SDL_Event

   SDL_PollEvent
      SDL_QUIT

   ;SDL_timer
   SDL_Delay

   ;SDL_image   
   IMG_Init
      IMG_INIT_JPG
      IMG_INIT_PNG
      IMG_INIT_TIF
      IMG_INIT_WEBP

   IMG_Load
   )

(begin
(define uname (uname))

(define win32? (string-ci=? (ref uname 1) "Windows"))
(define linux? (string-ci=? (ref uname 1) "Linux"))

;(define WIDTH 1280)
;(define HEIGHT 920)

(define % (dlopen (cond
   (win32? "SDL2.dll")
   (linux? "libSDL2.so")
   (else (runtime-error "No sdl2 library support" "Unknown platform")))))

(if (not %)
   (runtime-error "Can't load sdl2 library." (cond
      (win32?
         "Download dll from https://www.libsdl.org/download-2.0.php")
      (linux?
         "Use, for example, sudo apt install libsdl2-2.0"))))


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
(define SDL_ClearError (dlsym % type-void "SDL_ClearError"))


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
(define SDL_GLContext type-vptr)

(define SDL_WINDOWPOS_UNDEFINED_MASK    #x1FFF0000)
;(define SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)  (SDL_WINDOWPOS_UNDEFINED_MASK|(X))
(define SDL_WINDOWPOS_UNDEFINED         (bor SDL_WINDOWPOS_UNDEFINED_MASK 0))


(define SDL_CreateWindow (dlsym % SDL_Window* "SDL_CreateWindow" type-string type-integer type-integer type-integer type-integer type-integer))
   (define SDL_WINDOW_FULLSCREEN #x00000001)
   (define SDL_WINDOW_OPENGL     #x00000002)
   (define SDL_WINDOW_SHOWN      #x00000004)

(define SDL_GetWindowSurface (dlsym % SDL_Surface* "SDL_GetWindowSurface" SDL_Window*))

(define SDL_GLattr type-int)
   (define SDL_GL_RED_SIZE 0)
   (define SDL_GL_GREEN_SIZE 1)
   (define SDL_GL_BLUE_SIZE 2)
   (define SDL_GL_ALPHA_SIZE 3)
   (define SDL_GL_BUFFER_SIZE 4)
   (define SDL_GL_DOUBLEBUFFER 5)
   (define SDL_GL_DEPTH_SIZE 6)
   (define SDL_GL_STENCIL_SIZE 7)
   (define SDL_GL_ACCUM_RED_SIZE 8)
   (define SDL_GL_ACCUM_GREEN_SIZE 9)
   (define SDL_GL_ACCUM_BLUE_SIZE 10)
   (define SDL_GL_ACCUM_ALPHA_SIZE 11)
   (define SDL_GL_STEREO 12)
   (define SDL_GL_MULTISAMPLEBUFFERS 13)
   (define SDL_GL_MULTISAMPLESAMPLES 14)
   (define SDL_GL_ACCELERATED_VISUAL 15)
   (define SDL_GL_RETAINED_BACKING 16)
   (define SDL_GL_CONTEXT_MAJOR_VERSION 17)
   (define SDL_GL_CONTEXT_MINOR_VERSION 18)
   (define SDL_GL_CONTEXT_EGL 19)
   (define SDL_GL_CONTEXT_FLAGS 20)
   (define SDL_GL_CONTEXT_PROFILE_MASK 21)
   (define SDL_GL_SHARE_WITH_CURRENT_CONTEXT 22)
   (define SDL_GL_FRAMEBUFFER_SRGB_CAPABLE 23)
   (define SDL_GL_CONTEXT_RELEASE_BEHAVIOR 24)
   (define SDL_GL_CONTEXT_RESET_NOTIFICATION 25)
   (define SDL_GL_CONTEXT_NO_ERROR 26)

(define SDL_GL_SetAttribute (dlsym % type-int "SDL_GL_SetAttribute" SDL_GLattr type-int))

(define SDL_GL_CreateContext (dlsym % SDL_GLContext "SDL_GL_CreateContext" SDL_Window*))

(define SDL_GL_SetSwapInterval (dlsym % type-int "SDL_GL_SetSwapInterval" type-int))

(define SDL_GL_SwapWindow (dlsym % type-void "SDL_GL_SwapWindow" SDL_Window*))
   

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

; ------------------------
; SDL_timer
(define SDL_Delay (dlsym % type-void "SDL_Delay" type-int32))

; ========================
; SDL_image
(define % (dlopen (cond
   (win32? "SDL2_image.dll")
   (linux? "libSDL2_image-2.0.so.0")
   (else (runtime-error "No sdl2 image library support" "Unknown platform")))))

(if (not %)
   (runtime-error "Can't load sdl2 image library." (cond
      (win32?
         "Download dll from https://www.libsdl.org/projects/SDL_image/")
      (linux?
         "Use, for example, sudo apt install libsdl2-image-2.0"))))

(define IMG_Init (dlsym % type-integer "IMG_Init" type-integer))
   (define IMG_INIT_JPG #x00000001)
   (define IMG_INIT_PNG #x00000002)
   (define IMG_INIT_TIF #x00000004)
   (define IMG_INIT_WEBP #x00000008)

(define IMG_Load (dlsym % SDL_Surface* "IMG_Load" type-string))
))
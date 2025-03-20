(define-library (lib sdl3lite)
(import
   (otus lisp)
   (otus ffi))

(export
   ; SDL_AppResult
   SDL_APP_CONTINUE
   SDL_APP_SUCCESS
   SDL_APP_FAILURE

   ; SDL_error
   SDL_GetError

   ; SDL_Initialize
   SDL_Init
      SDL_INIT_AUDIO
      SDL_INIT_VIDEO
      SDL_INIT_JOYSTICK
      SDL_INIT_HAPTIC
      SDL_INIT_GAMEPAD
      SDL_INIT_EVENTS
      SDL_INIT_SENSOR
      SDL_INIT_CAMERA
   SDL_Quit

   ; SDL_Log
   SDL_Log

   ; SDL_Window
   SDL_CreateWindow
      SDL_WINDOW_FULLSCREEN
      SDL_WINDOW_OPENGL
      SDL_WINDOW_OCCLUDED
      SDL_WINDOW_HIDDEN
      SDL_WINDOW_BORDERLESS
      SDL_WINDOW_RESIZABLE
      SDL_WINDOW_MINIMIZED
      SDL_WINDOW_MAXIMIZED
      SDL_WINDOW_MOUSE_GRABBED
      SDL_WINDOW_INPUT_FOCUS
      SDL_WINDOW_MOUSE_FOCUS
      SDL_WINDOW_EXTERNAL
      SDL_WINDOW_MODAL
      SDL_WINDOW_HIGH_PIXEL_DENSITY
      SDL_WINDOW_MOUSE_CAPTURE
      SDL_WINDOW_MOUSE_RELATIVE_MODE
      SDL_WINDOW_ALWAYS_ON_TOP
      SDL_WINDOW_UTILITY
      SDL_WINDOW_TOOLTIP
      SDL_WINDOW_POPUP_MENU
      SDL_WINDOW_KEYBOARD_GRABBED
      SDL_WINDOW_VULKAN
      SDL_WINDOW_METAL
      SDL_WINDOW_TRANSPARENT
      SDL_WINDOW_NOT_FOCUSABLE
   SDL_DestroyWindow
   SDL_GetWindowSurface

   ; SDL_Renderer
   SDL_CreateSoftwareRenderer
   SDL_CreateRenderer
   SDL_DestroyRenderer
   SDL_RenderPresent
   SDL_SetRenderDrawColor
   SDL_RenderClear
   SDL_RenderFillRect
   SDL_RenderTexture

   ; SDL_Event
   make-SDL_Event
   SDL_Event->type
      SDL_EVENT_FIRST
      SDL_EVENT_QUIT
   ;...
   SDL_PollEvent

)

(cond-expand   
   (Windows
      (begin
         (define SDL3 (load-dynamic-library "SDL3.dll"))
         (define sdl3-err "Download dll from https://github.com/JordanCpp/SDL3Lite")))
   (Linux
      (begin
         (define SDL3 (load-dynamic-library "libSDL3.so"))
         (define sdl3-err "Build one from https://github.com/JordanCpp/SDL3Lite")))
   (Darwin
      (begin
         (define SDL3 (load-dynamic-library "libSDL3.dylib"))
         (define sdl3-err "")))
   (else
      (runtime-error "unsupported platform" (uname))))

(begin

;(define WIDTH 1280)
;(define HEIGHT 920)
(unless SDL3
   (runtime-error "Can't load sdl3lite library." sdl3-err))

;; ; ===================================================
;; ; helper function
;; (define bitwise-ior
;;    (case-lambda
;;       ((a b) (bor a b))
;;       ((a) a)
;;       ((a . bs) (fold bor a bs))))

   (setq int fft-int)
   (setq void fft-void)
   (setq size_t fft-size_t)
   (setq bool fft-bool)
   (setq Uint8 fft-uint8)
   (setq SDL_InitFlags fft-uint32)
   (setq SDL_Window* type-vptr)
   (setq SDL_Surface* type-vptr)
   (setq SDL_Renderer* type-vptr)
   (setq SDL_Texture* type-vptr)
   (setq SDL_FRect* type-vptr) ; type-bytevector?

   (setq SDL_QuitEvent* type-bytevector)
   (setq SDL_DisplayEvent* type-bytevector)
   (setq SDL_Event* type-bytevector)

   ; ------------------------
   ; SDL_AppResult
   (define SDL_APP_CONTINUE 0)
   (define SDL_APP_SUCCESS  1)
   (define SDL_APP_FAILURE  2)

   ; -----------------------------------------------------
   ; SDL_error
   (define SDL_GetError (SDL3 type-string "SDL_GetError"))

   ; -----------------------------------------------------
   ; SDL_Initialize
   (define SDL_Init  (SDL3 int  "SDL_Init" SDL_InitFlags))
   (define SDL_Quit  (SDL3 void "SDL_Quit"))
   (define SDL_INIT_AUDIO      #x00000010)
   (define SDL_INIT_VIDEO      #x00000020)
   (define SDL_INIT_JOYSTICK   #x00000200)
   (define SDL_INIT_HAPTIC     #x00001000)
   (define SDL_INIT_GAMEPAD    #x00002000)
   (define SDL_INIT_EVENTS     #x00004000)
   (define SDL_INIT_SENSOR     #x00008000)
   (define SDL_INIT_CAMERA     #x00010000)

   ; ---------------------------------------------------------
   ; SDL_Log
   (define SDL_Log  (SDL3 void "SDL_Log" type-string #|...|#))

   ; ----------------------------
   ; SDL_Window
   (define SDL_CreateWindow (SDL3 SDL_Window* "SDL_CreateWindow" type-string int int size_t))
   (define SDL_DestroyWindow (SDL3 void "SDL_DestroyWindow" SDL_Window*))
   (define SDL_GetWindowSurface (SDL3 SDL_Surface* "SDL_GetWindowSurface" SDL_Window*))
   (define SDL_WINDOW_FULLSCREEN           #x0000000000000001)
   (define SDL_WINDOW_OPENGL               #x0000000000000002)
   (define SDL_WINDOW_OCCLUDED             #x0000000000000004)
   (define SDL_WINDOW_HIDDEN               #x0000000000000008)
   (define SDL_WINDOW_BORDERLESS           #x0000000000000010)
   (define SDL_WINDOW_RESIZABLE            #x0000000000000020)
   (define SDL_WINDOW_MINIMIZED            #x0000000000000040)
   (define SDL_WINDOW_MAXIMIZED            #x0000000000000080)
   (define SDL_WINDOW_MOUSE_GRABBED        #x0000000000000100)
   (define SDL_WINDOW_INPUT_FOCUS          #x0000000000000200)
   (define SDL_WINDOW_MOUSE_FOCUS          #x0000000000000400)
   (define SDL_WINDOW_EXTERNAL             #x0000000000000800)
   (define SDL_WINDOW_MODAL                #x0000000000001000)
   (define SDL_WINDOW_HIGH_PIXEL_DENSITY   #x0000000000002000)
   (define SDL_WINDOW_MOUSE_CAPTURE        #x0000000000004000)
   (define SDL_WINDOW_MOUSE_RELATIVE_MODE  #x0000000000008000)
   (define SDL_WINDOW_ALWAYS_ON_TOP        #x0000000000010000)
   (define SDL_WINDOW_UTILITY              #x0000000000020000)
   (define SDL_WINDOW_TOOLTIP              #x0000000000040000)
   (define SDL_WINDOW_POPUP_MENU           #x0000000000080000)
   (define SDL_WINDOW_KEYBOARD_GRABBED     #x0000000000100000)
   (define SDL_WINDOW_VULKAN               #x0000000010000000)
   (define SDL_WINDOW_METAL                #x0000000020000000)
   (define SDL_WINDOW_TRANSPARENT          #x0000000040000000)
   (define SDL_WINDOW_NOT_FOCUSABLE        #x0000000080000000)

   ; SDL_Renderer
   (define SDL_CreateSoftwareRenderer (SDL3 SDL_Renderer* "SDL_CreateSoftwareRenderer" SDL_Surface*))
   (define SDL_CreateRenderer (SDL3 SDL_Renderer* "SDL_CreateRenderer" SDL_Window* type-string))
   (define SDL_DestroyRenderer (SDL3 void "SDL_DestroyRenderer" SDL_Renderer*))
   (define SDL_RenderPresent (SDL3 bool "SDL_RenderPresent" SDL_Renderer*))
   (define SDL_SetRenderDrawColor (SDL3 bool "SDL_SetRenderDrawColor" SDL_Renderer* Uint8 Uint8 Uint8 Uint8))
   (define SDL_RenderClear (SDL3 bool "SDL_RenderClear" SDL_Renderer*))
   (define SDL_RenderFillRect (SDL3 void "SDL_RenderFillRect" SDL_Renderer* SDL_FRect*))
   (define SDL_RenderTexture (SDL3 bool "SDL_RenderTexture" SDL_Renderer* SDL_Texture* SDL_FRect* SDL_FRect*))

   ; SDL_Event
   (define (make-SDL_Event) (make-bytevector 16))
   (define (SDL_Event->type event) (bytevector->int32 event 0))
   (define SDL_EVENT_FIRST 0)
   (define SDL_EVENT_QUIT #x100)
   ; ...
   (define SDL_PollEvent (SDL3 int "SDL_PollEvent" SDL_Event*))
))

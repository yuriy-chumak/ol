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

   ; General keyboard/mouse state definitions
   SDL_RELEASED
   SDL_PRESSED

   ; The types of events that can be delivered.
   SDL_EventType
      SDL_FIRSTEVENT              ; /**< Unused (do not remove) */

      ; /* Application events */
      SDL_QUIT                    ; /**< User-requested quit */

      ; /* These application events have special meaning on iOS, see README-ios.md for details */
      SDL_APP_TERMINATING         ;/**< The application is being terminated by the OS
                                    ; Called on iOS in applicationWillTerminate()
                                    ; Called on Android in onDestroy()
      SDL_APP_LOWMEMORY           ;/**< The application is low on memory, free memory if possible.
                                    ; Called on iOS in applicationDidReceiveMemoryWarning()
                                    ; Called on Android in onLowMemory()
      SDL_APP_WILLENTERBACKGROUND ;/**< The application is about to enter the background
                                    ; Called on iOS in applicationWillResignActive()
                                    ; Called on Android in onPause()
      SDL_APP_DIDENTERBACKGROUND  ;/**< The application did enter the background and may not get CPU for some time
                                    ; Called on iOS in applicationDidEnterBackground()
                                    ; Called on Android in onPause()
      SDL_APP_WILLENTERFOREGROUND ;/**< The application is about to enter the foreground
                                    ; Called on iOS in applicationWillEnterForeground()
                                    ; Called on Android in onResume()
      SDL_APP_DIDENTERFOREGROUND  ;/**< The application is now interactive
                                    ; Called on iOS in applicationDidBecomeActive()
                                    ; Called on Android in onResume()

      ; /* Window events */
      SDL_WINDOWEVENT             ;/**< Window state change */
      SDL_SYSWMEVENT              ;/**< System specific event */

      ; /* Keyboard events */
      SDL_KEYDOWN                 ;/**< Key pressed */
      SDL_KEYUP                   ;/**< Key released */
      SDL_TEXTEDITING             ;/**< Keyboard text editing (composition) */
      SDL_TEXTINPUT               ;/**< Keyboard text input */
      SDL_KEYMAPCHANGED           ;/**< Keymap changed due to a system event such as an
                                    ; input language or keyboard layout change.

      ; /* Mouse events */
      SDL_MOUSEMOTION             ;/**< Mouse moved */
      SDL_MOUSEBUTTONDOWN         ;/**< Mouse button pressed */
      SDL_MOUSEBUTTONUP           ;/**< Mouse button released */
      SDL_MOUSEWHEEL              ;/**< Mouse wheel motion */

      ; /* Joystick events */
      SDL_JOYAXISMOTION           ;/**< Joystick axis motion */
      SDL_JOYBALLMOTION           ;/**< Joystick trackball motion */
      SDL_JOYHATMOTION            ;/**< Joystick hat position change */
      SDL_JOYBUTTONDOWN           ;/**< Joystick button pressed */
      SDL_JOYBUTTONUP             ;/**< Joystick button released */
      SDL_JOYDEVICEADDED          ;/**< A new joystick has been inserted into the system */
      SDL_JOYDEVICEREMOVED        ;/**< An opened joystick has been removed */

      ; /* Game controller events */
      SDL_CONTROLLERAXISMOTION           ;/**< Game controller axis motion */
      SDL_CONTROLLERBUTTONDOWN           ;/**< Game controller button pressed */
      SDL_CONTROLLERBUTTONUP             ;/**< Game controller button released */
      SDL_CONTROLLERDEVICEADDED          ;/**< A new Game controller has been inserted into the system */
      SDL_CONTROLLERDEVICEREMOVED        ;/**< An opened Game controller has been removed */
      SDL_CONTROLLERDEVICEREMAPPED       ;/**< The controller mapping was updated */

      ; /* Touch events */
      SDL_FINGERDOWN
      SDL_FINGERUP
      SDL_FINGERMOTION

      ; /* Gesture events */
      SDL_DOLLARGESTURE
      SDL_DOLLARRECORD
      SDL_MULTIGESTURE

      ; /* Clipboard events */
      SDL_CLIPBOARDUPDATE          ;/**< The clipboard changed */

      ; /* Drag and drop events */
      SDL_DROPFILE                  ;/**< The system requests a file open */
      SDL_DROPTEXT                  ;/**< text/plain drag-and-drop event */
      SDL_DROPBEGIN                 ;/**< A new set of drops is beginning (NULL filename) */
      SDL_DROPCOMPLETE              ;/**< Current set of drops is now complete (NULL filename) */

      ; /* Audio hotplug events */
      SDL_AUDIODEVICEADDED           ;/**< A new audio device is available */
      SDL_AUDIODEVICEREMOVED         ;/**< An audio device has been removed. */

      ; /* Render events */
      SDL_RENDER_TARGETS_RESET           ;/**< The render targets have been reset and their contents need to be updated */
      SDL_RENDER_DEVICE_RESET  ;/**< The device has been reset and all textures need to be recreated */

      ; /** Events ::SDL_USEREVENT through ::SDL_LASTEVENT are for your use,
      ;  *  and should be allocated with SDL_RegisterEvents()
      ;  */
      SDL_USEREVENT

      ; /**
      ;  *  This last event is only for bounding internal arrays
      ;  */
      SDL_LASTEVENT

   SDL_PollEvent

   ;SDL_mouse
   SDL_GetMouseState

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

(define sdl2 (load-dynamic-library (cond
   (win32? "SDL2.dll")
   (linux? "libSDL2-2.0.so.0") ;libSDL2-2.0.so.0 
   (else
      (runtime-error "sdl2: unknown platform" uname)))))

(if (not sdl2)
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
(define SDL_GetError (sdl2 type-string "SDL_GetError"))
(define SDL_ClearError (sdl2 fft-void "SDL_ClearError"))


; ------------------------
; SDL_main
(define SDL_Init  (sdl2 fft-int "SDL_Init"  fft-int))
   (define SDL_INIT_VIDEO          #x00000020)


; ------------------------
; SDL_surface
(define SDL_Surface* type-vptr)

(define SDL_FreeSurface (sdl2 fft-void "SDL_FreeSurface" SDL_Surface*))

; ------------------------
; SDL_video
(define SDL_Window* type-vptr)
(define SDL_GLContext type-vptr)

(define SDL_WINDOWPOS_UNDEFINED_MASK    #x1FFF0000)
;(define SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)  (SDL_WINDOWPOS_UNDEFINED_MASK|(X))
(define SDL_WINDOWPOS_UNDEFINED         (bor SDL_WINDOWPOS_UNDEFINED_MASK 0))


(define SDL_CreateWindow (sdl2 SDL_Window* "SDL_CreateWindow" type-string fft-int fft-int fft-int fft-int fft-int))
   (define SDL_WINDOW_FULLSCREEN #x00000001)
   (define SDL_WINDOW_OPENGL     #x00000002)
   (define SDL_WINDOW_SHOWN      #x00000004)

(define SDL_GetWindowSurface (sdl2 SDL_Surface* "SDL_GetWindowSurface" SDL_Window*))

(define SDL_GLattr fft-int)
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

(define SDL_GL_SetAttribute (sdl2 fft-int "SDL_GL_SetAttribute" SDL_GLattr fft-int))

(define SDL_GL_CreateContext (sdl2 SDL_GLContext "SDL_GL_CreateContext" SDL_Window*))

(define SDL_GL_SetSwapInterval (sdl2 fft-int "SDL_GL_SetSwapInterval" fft-int))

(define SDL_GL_SwapWindow (sdl2 fft-void "SDL_GL_SwapWindow" SDL_Window*))
   

; ------------------------
; SDL_render
(define SDL_Renderer* type-vptr)
(define SDL_Texture* type-vptr)
(define SDL_Rect* type-vptr)

(define SDL_CreateRenderer (sdl2 SDL_Renderer* "SDL_CreateRenderer" SDL_Window* fft-int fft-int))
   (define SDL_RENDERER_SOFTWARE #x00000001)          ;/**< The renderer is a software fallback */
   (define SDL_RENDERER_ACCELERATED #x00000002)       ;/**< The renderer uses hardware acceleration */
   (define SDL_RENDERER_PRESENTVSYNC #x00000004)      ;/**< Present is synchronized with the refresh rate */
   (define SDL_RENDERER_TARGETTEXTURE #x00000008)     ;/**< The renderer supports

(define SDL_RenderClear (sdl2 fft-int "SDL_RenderClear" SDL_Renderer*))
(define SDL_RenderPresent (sdl2 fft-void "SDL_RenderPresent" SDL_Renderer*))
(define SDL_RenderCopy (sdl2 fft-int "SDL_RenderCopy" SDL_Renderer* SDL_Texture* SDL_Rect* SDL_Rect*))

(define SDL_CreateTextureFromSurface (sdl2 SDL_Texture* "SDL_CreateTextureFromSurface" SDL_Renderer* SDL_Surface*))

; ------------------------
; SDL_event
(define SDL_Event* type-vptr)
(define (make-SDL_Event) (vm:new-raw-object type-vector-raw 56))

(define SDL_RELEASED 0)
(define SDL_PRESSED 1)

(define SDL_EventType fft-int)
   (define SDL_FIRSTEVENT 0)
   (define SDL_QUIT             #x100)
   (define SDL_APP_TERMINATING  #x101)
   (define SDL_APP_LOWMEMORY    #x102)
   (define SDL_APP_WILLENTERBACKGROUND #x103)
   (define SDL_APP_DIDENTERBACKGROUND  #x104)
   (define SDL_APP_WILLENTERFOREGROUND #x105)
   (define SDL_APP_DIDENTERFOREGROUND  #x106)
   (define SDL_WINDOWEVENT      #x200)
   (define SDL_SYSWMEVENT       #x201)
   (define SDL_KEYDOWN         #x300)
   (define SDL_KEYUP           #x301)
   (define SDL_TEXTEDITING     #x302)
   (define SDL_TEXTINPUT       #x303)
   (define SDL_KEYMAPCHANGED   #x304)
   (define SDL_MOUSEMOTION     #x305)
   (define SDL_MOUSEBUTTONDOWN #x306)
   (define SDL_MOUSEBUTTONUP   #x307)
   (define SDL_MOUSEWHEEL      #x308)
   (define SDL_JOYAXISMOTION    #x600)
   (define SDL_JOYBALLMOTION    #x601)
   (define SDL_JOYHATMOTION     #x602)
   (define SDL_JOYBUTTONDOWN    #x603)
   (define SDL_JOYBUTTONUP      #x604)
   (define SDL_JOYDEVICEADDED   #x605)
   (define SDL_JOYDEVICEREMOVED #x606)
   (define SDL_CONTROLLERAXISMOTION     #x650)
   (define SDL_CONTROLLERBUTTONDOWN     #x651)
   (define SDL_CONTROLLERBUTTONUP       #x652)
   (define SDL_CONTROLLERDEVICEADDED    #x653)
   (define SDL_CONTROLLERDEVICEREMOVED  #x654)
   (define SDL_CONTROLLERDEVICEREMAPPED #x655)
   (define SDL_FINGERDOWN   #x700)
   (define SDL_FINGERUP     #x701)
   (define SDL_FINGERMOTION #x702)
   (define SDL_DOLLARGESTURE   #x800)
   (define SDL_DOLLARRECORD    #x801)
   (define SDL_MULTIGESTURE    #x802)
   (define SDL_CLIPBOARDUPDATE  #x900)
   (define SDL_DROPFILE          #x1000)
   (define SDL_DROPTEXT          #x1001)
   (define SDL_DROPBEGIN         #x1002)
   (define SDL_DROPCOMPLETE      #x1003)
   (define SDL_AUDIODEVICEADDED   #x1100)
   (define SDL_AUDIODEVICEREMOVED #x1101)
   (define SDL_RENDER_TARGETS_RESET #x2000)
   (define SDL_RENDER_DEVICE_RESET  #x2001)
   (define SDL_USEREVENT    #x8000)
   (define SDL_LASTEVENT #xFFFF)

(define SDL_PollEvent (sdl2 fft-int "SDL_PollEvent" SDL_Event*))

; ------------------------
; SDL_mouse
(define SDL_GetMouseState (sdl2 fft-int "SDL_GetMouseState" type-vptr type-vptr))


; ------------------------
; SDL_timer
(define SDL_Delay (sdl2 fft-void "SDL_Delay" fft-int32))

; ========================
; SDL_image
(define sdl2-image (load-dynamic-library (cond
   (win32? "SDL2_image.dll")
   (linux? "libSDL2_image-2.0.so.0")
   (else
      (runtime-error "sdl20-image: unknown platform" uname)))))

(if (not sdl2-image)
   (runtime-error "Can't load sdl2 image library." (cond
      (win32?
         "Download dll from https://www.libsdl.org/projects/SDL_image/")
      (linux?
         "Use, for example, sudo apt install libsdl2-image-2.0"))))

(define IMG_Init (sdl2-image fft-int "IMG_Init" fft-int))
   (define IMG_INIT_JPG #x00000001)
   (define IMG_INIT_PNG #x00000002)
   (define IMG_INIT_TIF #x00000004)
   (define IMG_INIT_WEBP #x00000008)

(define IMG_Load (sdl2-image SDL_Surface* "IMG_Load" type-string))
))

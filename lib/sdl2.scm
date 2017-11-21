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
(define uname (syscall 63 #f #f #f))

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
(define SDL_Init  (dlsym % type-int+ "SDL_Init"  type-int+))
   (define SDL_INIT_VIDEO          #x00000020)


; ------------------------
; SDL_video
(define SDL_Window* type-vptr)
(define SDL_Surface* type-vptr)

(define SDL_WINDOWPOS_UNDEFINED_MASK    #x1FFF0000)
;(define SDL_WINDOWPOS_UNDEFINED_DISPLAY(X)  (SDL_WINDOWPOS_UNDEFINED_MASK|(X))
(define SDL_WINDOWPOS_UNDEFINED         (bor SDL_WINDOWPOS_UNDEFINED_MASK 0))


(define SDL_CreateWindow (dlsym % SDL_Window* "SDL_CreateWindow" type-string type-int+ type-int+ type-int+ type-int+ type-int+))
   (define SDL_WINDOW_SHOWN #x00000004)

(define SDL_GetWindowSurface (dlsym % SDL_Surface* "SDL_GetWindowSurface" SDL_Window*))

; ------------------------
; SDL_render


;))

; test:
; ***************************************************
(if (less? (SDL_Init SDL_INIT_VIDEO) 0)
   (begin
      (print "Unable to Init SDL: " (SDL_GetError))
      (exit-owl 1)))

(define window (SDL_CreateWindow "Create SDL2 Window sample"
   SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
   640 480 SDL_WINDOW_SHOWN))

(define surface (SDL_GetWindowSurface window))


(print "ok.")

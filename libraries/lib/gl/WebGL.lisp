(define setup:autorender-mode #false)

(define (native:create-context title)
   ['webGL])

(define (native:enable-context context) #true)
(define (native:disable-context context) #false)
(define (native:swap-buffers context) #f) ;(glFinish))


(define (native:process-events context handler)
   ;; TBD.
   #false)

(define (os:SetWindowTitle context title)
   ;; TBD.
   #false)
(define (os:SetWindowSize context width height) #false)
(define (os:HideCursor context) #false)
(define (os:GetMousePos context) #false)

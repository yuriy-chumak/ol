; https://rosettacode.org/wiki/Active_object#Ol
(import (scheme inexact)) ; sin, cos, etc.
(import (math infix-notation))

; elapsed time in seconds:
(define (time-s) (/ (time-ms) #i1000))

; Start Active Object
(define active-object
   (async (lambda ()
      (define Δt 50) ; 50ms
      (define start (time-s))

      ; active object internal loop
      (let loop ((K (lambda (x) 0))
                 (t₀ (- (time-s) start))
                 (S 0))
         (define envelope (check-mail)) ; check incoming messages
         (if envelope
         then
            (let*((sender msg envelope)) ; extract message and sender
               (case msg
                  (['Input K]
                     (loop K t₀ S))
                  ('Output
                     (mail sender S)
                     (loop K t₀ S))
                  ('stop
                     #false)
                  (else
                     (runtime-error "unhandled message" msg))))
         else
            (wait Δt)
            (define t₁ (- (time-s) start))
            (loop K t₁
                  ; infix notation of the formula for better readability
                  (\\  S + (K(t₁) + K(t₀)) * (t₁ - t₀) / 2  )
                  ;; (+ S
                  ;;    (* (+ (K t₁) (K t₀)) #i0.5
                  ;;       (- t₁ t₀))
                  ;; ))
               ))))))

; Setup Functions and Constants
(define f 0.5)
(define Pi #i3.14159265358979) ; Pi is inexact number
(define (K t) (sin (* 2 Pi f t)))

; Test the Active Object
(print "1. set active object input to sin (2π f t), where the frequency f=0.5Hz. The phase is irrelevant.")
(mail active-object ['Input K])

(print "2. wait 2s")
(wait 2000)

(print "3. set the input to constant 0")
(mail active-object ['Input (lambda (t) 0)])

(print "4. wait 0.5s")
(wait 500)

(print "5. Verify that now the object's output is approximately 0 (the sine has the period of 2s).")
(define output (await (mail active-object 'Output)))
(when (< (abs output) 0.01)
   (print "yes, the object's output is approximately 0"))

; Stop the Active Object
(mail active-object 'stop)

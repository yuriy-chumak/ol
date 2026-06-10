; default lib gl configuration
(define-library (lib gl config)
(export config
        set-gl-option)
(import
   (scheme core)
   (owl ff)
   (otus async)
   (scheme dynamic-bindings))
(begin
   (define options (make-parameter {
      'red 8    ; 8 bits for red color
      'green 8  ; 8 bits for green color
      'blue 8   ; 8 bits for blue color
      'depth 24 ;24 bits for depth buffer
   }))

   (define config (case-lambda
      ((name) ((options) name #false))
      ((name fail) ((options) name fail))
      ((set name value)
         (when (eq? set 'set)
            ((options (put (options) name value)) name #false)))
   ))

   (define (set-gl-option name value)
      ((options (put (options) name value)) name #false))

))



   ;; (setq cfg ['gl 'config])
   ;; (setq false #false)

   ;; (actor cfg (lambda ()
   ;;    (let cycle ((config {
   ;;       ; default OpenGL settings
   ;;       'red 8    ; 8 bits for red color
   ;;       'green 8  ; 8 bits for green color
   ;;       'blue 8   ; 8 bits for blue color
   ;;       'depth 24 ;24 bits for depth buffer

   ;;    }))
   ;;    (let*((envelope (wait-mail))
   ;;          (sender msg envelope))
   ;;       (case msg
   ;;          (['set key value]
   ;;             (cycle (put config key value)))
   ;;          (['get key value]
   ;;             (mail sender (get config key value))
   ;;             (cycle config))
   ;;          (else
   ;;             (cycle config)))))))

   ;; (define config
   ;;    (case-lambda
   ;;       ((key)
   ;;          (await (mail cfg ['get key false])))
   ;;       ((key value)
   ;;          (await (mail cfg ['get key value])))
   ;;       ((set key value)
   ;;          (mail cfg [set key value]))))

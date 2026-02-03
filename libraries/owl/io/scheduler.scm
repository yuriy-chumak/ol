(define-library (owl io scheduler)
   (export
      io-scheduler
      io-scheduler-name
      ;; io-scheduler-timeout ; the message "timeout accurs"
   )

   (import
      (scheme core)
      (only (owl list) remove)
      (only (scheme list) foldr) ; DEBUG
      (owl math) ; todo: use integer math for speed
      (owl lazy)
      (only (otus format) format) ; DEBUG
      (otus async))

(begin
   (define (sys:write fd buffer)
      (syscall 1 fd buffer #false))

   (define stderr (vm:cast 2 type-port))
   (define (print . args)
      (sys:write stderr (make-bytevector (foldr format '(#\newline) args))))

   (define max-wait-timeout (* 60 60 1000)); hour
   (assert (or (less? max-wait-timeout (vm:vmax))
               (eq? max-wait-timeout (vm:vmax))))

   (define (sys:poll rs ws timeout)
      (syscall 7 rs ws timeout))

   ;; ==================================================================================
   (define TIMEOUT ['alarm])

   ;; TODO: change to faster function (i.e. (gettimeofday), etc.)
   (define (time-ms)
      (let* ((ss ms (clock)))
         (+ (* ss 1000) ms)))

   ;; set thread scheduler runtime to 0, which triggers a thread change
   (define (set-ticker-value n) (syscall 1022 n))

   (define next-thread
      (λ () (set-ticker-value 0)))

   ;; (... (x . foo) ...) x → (... ...) (x . foo)
   (define (grabelt lst x)
      (let loop ((lst lst) (out #n))
         (if (null? lst)
            (values out #false)
            (let ((a (car lst)))
               (if (eq? x (car a))
                  (values (append (cdr lst) out) a)
                  (loop (cdr lst) (cons a out)))))))

   (define (push-alarm alarms time id)
      (if (null? alarms)
         (list (cons time id))
         (let ((a (car alarms)))
            (if (< (car a) time)
               (cons a (push-alarm (cdr alarms) time id))
               (cons (cons time id) alarms)))))

   (define (remove-alarm alarms envelope)
      (if (null? alarms)
         (begin
            (print "ERROR: fd read with timeout had no matching alarm")
            #null)
         (let ((this (car alarms)))
            (if (eq? (cdr this) envelope)
               (cdr alarms)
               (cons this
                  (remove-alarm (cdr alarms) envelope))))))

   (define (remove-alarms-by-fd alarms fd)
      (remove
         (λ (alarm)
            ;; alarm = (TIMEOUT . #(thread #(<read|write>-timeout fd ms)))
            (let*
               ((msg (cdr alarm))
                  (from req msg)
                  (op port ms req))
               (eq? port fd)))
         alarms))
   ;; alarm = (TIMEOUT . #(thread #(<read|write>-timeout fd ms)))
   (define (remove-alarms-by-thread alarms thread)
      (remove (λ (alarm)
            (eq? (ref (cdr alarm) 1) thread))
         alarms))

   (define (wakeup rs ws alarms fd reason)
      (cond
         ((eq? reason 1) ;; data ready to be read
            (let* 
               ((rs x (grabelt rs fd))
                  (fd envelope x)
                  (from message envelope))
               (mail from message)
               (values rs ws
                  (if (eq? (ref message 1) 'read-timeout)
                     (remove-alarm alarms envelope)
                     alarms))))
         ((eq? reason 2) ;; ready to be written to
            (let* ((ws x (grabelt ws fd))
                     (fd envelope x)
                     (from message envelope))
               (mail from message)
               (values rs ws
                  (if (eq? (ref message 1) 'write-timeout)
                     (remove-alarm alarms envelope)
                     alarms))))

         (else ;; error
            (let* ((rs x (grabelt rs fd))
                     (ws y (grabelt ws fd)))
               (if x (mail (cdr x) fd))
               (if y (mail (cdr y) fd))
               (values rs ws
                  (remove-alarms-by-fd alarms fd))))))

   ; rename to handle-muxer-message
   (define (muxer-add rs ws alarms envelope)
      (case (ref envelope 2)
         (['read fd]
            (values (cons (cons fd envelope) rs) ws alarms))
         (['read-timeout fd ms]
            (values (cons (cons fd envelope) rs) ws
               (push-alarm alarms (+ (time-ms) ms) envelope)))
         (['write fd]
            (values rs (cons (cons fd envelope) ws) alarms))
         (['write-timeout fd ms]
            (values rs (cons (cons fd envelope) ws)
               (push-alarm alarms (+ (time-ms) ms) envelope)))
         (['alarm ms]
            (values rs ws (push-alarm alarms (+ (time-ms) ms) envelope)))

         (['call thread] ; urgent mail delivery
            (mail thread 'call)
            (values rs ws
               (remove-alarms-by-thread alarms thread)))
         (else
            (print "ERROR: bad muxer message from " (ref envelope 1))
            (values rs ws alarms))))

   (define (muxer rs ws alarms)
      ; alarms list is sorted by timeout
      (if (null? alarms)
      then
         ;; No alarms, just maybe IO and messages
         (let ((envelope ((if (and (null? rs) (null? ws)) wait-mail check-mail))))
            (if envelope
               (let*((rs ws alarms (muxer-add rs ws alarms envelope)))
                     (muxer rs ws alarms))
            else
               (let*((timeout (if (single-thread?) #false 0))
                     (waked x (uncons (sys:poll rs ws timeout) #f)))
                  (if waked
                     (let* ((rs ws alarms (wakeup rs ws alarms waked x)))
                        (muxer rs ws alarms))
                     else
                        (next-thread)
                        (muxer rs ws alarms)))))
      else
         (let ((now (time-ms)))
            ;;; alarms and next is not up yet
            (if (< now (caar alarms))
               (let ((envelope (check-mail)))
                  (if envelope
                     (let* ((rs ws alarms (muxer-add rs ws alarms envelope)))
                        (muxer rs ws alarms))
                  else ; todo: if 0 then force next thread?
                     (let*((timeout (if (single-thread?) (min max-wait-timeout (- (caar alarms) now)) 0))
                           (waked x (uncons (sys:poll rs ws timeout) #f)))
                        (if waked
                           (let* ((rs ws alarms (wakeup rs ws alarms waked x)))
                              (muxer rs ws alarms))
                           else
                              (next-thread)
                              (muxer rs ws alarms)))))
            else
               ;; the bell tolls
               (let*((alarm (car alarms))
                     (time envelope alarm)
                     (id message envelope))
                  ;; all alarms return a pair with the original request
                  ;; all i/o thread activations return the original request
                  (mail id ['timeout message]) ; todo: change to internal timeout message
                  (case message
                     ; sleep for ms milliseconds
                     (['alarm ms]
                        (muxer rs ws (cdr alarms)))

                     (['read-timeout fd ms]
                        ;; remove both the alarm and the read request
                        (let* ((rs _ (grabelt rs fd)))
                           (muxer rs ws (cdr alarms))))

                     (['write-timeout fd ms]
                        ;; remove both the alarm and the write request
                        (let* ((ws _ (grabelt ws fd)))
                           (muxer rs ws (cdr alarms))))
                     (else
                        ;; bug. crash.
                        (car #false))))))))

   (define io-scheduler-name ['io-scheduler])
   (define io-scheduler
      (delay (muxer #n #n #n)))

))
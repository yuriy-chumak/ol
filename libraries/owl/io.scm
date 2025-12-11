;;;
;;; Simple direct blocking IO (replaces the old thread-based one)
;;;

(define-library (owl io)
   (export
      ;; asynchronous i/o and threading
      start-io-scheduler

      wait wait-mail ; wait N ms, wait N ms or mail
      bell ; just wake, without message (notify actor to check mailbox)
      call ; to wake and send a message (send an urgent mail and bell)
      wait-read ; 

      ;; general i/o
      ;; thread-oriented non-blocking io
      open-input-file         ;; path → fd | #false
      open-binary-input-file
      open-output-file        ;; path → fd | #false
      open-binary-output-file
      flush-port              ;; fd → _
      close-port              ;; fd → _

      ;; stream-oriented blocking (for the writing thread) io
;      blocks->port            ;; ll fd → ll' n-bytes-written, don't close fd
;      closing-blocks->port    ;; ll fd → ll' n-bytes-written, close fd

      file->bytevector  ;; bytevectors io, may be moved elsewhere later
      bytevector->file
      write-bytevector

      ;; file->blob write-blob
      file->list              ;; list io, may be moved elsewhere later

      port->bytestream       ;; fd → (byte ...) | thunk
      file->bytestream
      bytestream->port
      bytestream->file

      port->string
      file->string

      write-bytestream

      stdin stdout stderr
      display-to        ;; port val → bool
      print-to          ;; port val → bool
      display
      print
      write             ; (obj), (obj port)
      write-simple
      write-bytes       ;; port byte-list   → bool
      get-block         ;; fd n → bvec | eof | #false
      try-get-block     ;; fd n block? → bvec | eof | #false=error | #true=block
      lines             ;; fd → null | ll of string, read error is just null, each [\r]\n removed

      writer-to         ;; names → (port val → bool + io)

      system-print system-println system-stderr
      fasl-save         ;; obj path → done?
      fasl-load         ;; path default → done?
      deserialize-file

      ; virtual i/o
      open-input-string
      open-output-string
      get-output-string

      unbuffered-input-stream
      read-char
      read-line
   )

   (import
      (scheme core)
      (scheme list)
      (otus async)
      (owl io scheduler)
      (owl queue)
      (owl string)
      (only (owl parse) rune)
      (owl list-extra)
      (owl ff)
      (otus blobs)
      (otus format)
      (owl math)
      (owl parse)
      (otus fasl)
      (owl lazy)
      (scheme bytevector)
      (only (otus blobs) merge-chunks blob-leaves))

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      ;; standard io ports
      (define stdin  (vm:cast 0 type-port))
      (define stdout (vm:cast 1 type-port))
      (define stderr (vm:cast 2 type-port))

      ;; speedup optimization
      (define (sys:read fd maxlen)         (syscall 0 fd maxlen))
      (define (sys:write fd buffer length) (syscall 1 fd buffer length))

      ; low-level file open/close functions
      (define (sys:open path mode)
         (cond
            ((c-string path) =>
               (λ (path) (syscall 2 path mode)))))

      (define (sys:close fd)
         (syscall 3 fd))

      ; i/o scheduler
      ; todo?: rewrite as constructor
      (define (start-io-scheduler)
         (actor io-scheduler-name io-scheduler))

      ; wait for ms microseconds (or for bell or call)
      (define (wait ms)
         (await (mail io-scheduler-name ['alarm ms])))

      ; override wait-mail
      (define wait-mail
         (define io-scheduler io-scheduler-name)
         (define (wait-mail-ms ms default)
            (let*((answer (check-mail))
                  (answer (or answer
                              (if (mail io-scheduler ['alarm ms])
                                 (wait-mail-from io-scheduler)))))
               (if (eq? (ref answer 1) io-scheduler)
               then ; timeout expired!
                  (or (check-mail) (if default [#false default]))
               else ; got real message
                  answer)))
      
      (case-lambda
         (() (wait-mail)) ; just a regular "wait-mail"
         ((ms) (wait-mail-ms ms #f)) ; wait mail with timeout, returns a mail if got one or "#false"
         ((ms default) (wait-mail-ms ms default))                          ; ... or [#false default]
      ))

      ; returns #true if port is ready to be read, #false if timeout
      (define (wait-read port timeout)
         (define answer (await (mail io-scheduler-name ['read-timeout port timeout])))
         (not (eq? (ref answer 1) 'timeout))) ; timeout reached?      

      ; just wake the coroutine (if waiting)
      (define (bell whom)
         (mail io-scheduler-name ['call whom]))

      ; "call" is "urgent mail"
      ; send mail to the coroutine and wake it if required
      (define (call whom message)
         (when (mail whom message)
            (mail io-scheduler-name ['call whom])
            whom))

     ;; ==================================================================================


      ;;; Writing

      ;; #[0 1 .. n .. m] n → #[n .. m]
      (define (bvec-tail bvec n)
         (make-bytevector (map (lambda (p) (ref bvec p)) (lrange n 1 (size bvec)))))

      (define (try-write-block fd bvec len)
         (if (port? fd) (sys:write fd bvec len) #false))

      ;; bvec port → bool
      (define (write-really bvec fd)
         (when (port? fd)
            (let ((end (size bvec)))
               (if (eq? end 0)
                  #true
                  (let loop ()
                     (let ((wrote (sys:write fd bvec end)))
                        (cond
                           ((eq? wrote end) #true) ;; ok, wrote the whole chunk
                           ((eq? wrote 0) ;; 0 = EWOULDBLOCK
                              (sleep 2) ;; fixme: adjustable delay rounds
                              (loop))
                           (wrote ;; partial write
                              (write-really (bvec-tail bvec wrote) fd))
                           (else #false)))))))) ;; write error or other failure

      ;; how many bytes (max) to add to output buffer before flushing it to the fd
      (define output-buffer-size 4096)

      (define (open-input-file path) (sys:open path #o0000)) ; O_RDONLY
      (define (open-binary-input-file path)
                                   (sys:open path #o100000)); O_RDONLY|O_BINARY

      (define (open-output-file path) (sys:open path #o1102)) ; O_CREAT|O_TRUNC|O_RDWR
      (define (open-binary-output-file path)
                                    (sys:open path #o101102)) ; O_CREAT|O_TRUNC|O_RDWR+O_BINARY

      ;;; Reading

      (define input-block-size
         *blob-leaf-size*) ;; changing from 256 breaks vector leaf things

      (define (try-get-block fd block-size block?)
         (let ((res (sys:read fd block-size)))
            (if (eq? res #true) ;; would block
               (if block?
                  (begin
                     (if (eq? fd stdin)
                        (sleep 2)
                        (wait-read fd 10000)) ;10 seconds
                     (try-get-block fd block-size #true))
                  res)
               res))) ;; is #false, eof or bvec

      ;; get a block of size up to block size
      (define (get-block fd block-size)
         (try-get-block fd block-size #true))

      (define (bvec-append a b)
         (make-bytevector
            (append
               (bytevector->list a)
               (bytevector->list b))))

      ;; get a block of size block-size, wait more if less is available and not eof
      ;; fd n → eof-seen? eof|#false|bvec
      (define (get-whole-block fd block-size)
         (let ((this (get-block fd block-size)))
            (cond
               ((eof? this) (values #true this))
               ((not this) (values #false this))
               (else
                  (let ((n (size this)))
                     (if (eq? n block-size)
                        (values #false this)
                        (lets ((eof-seen? tail (get-whole-block fd (- block-size n))))
                           (cond
                              ((eof? tail) (values #true this))
                              ((not tail) (values #false this)) ;; next read will also fail, return last ok data
                              (else
                                 ;; unnecessarily many conversions if there are many partial
                                 ;; reads, but block size is tiny in file->blob making this
                                 ;; irrelevant
                                 (values eof-seen?
                                    (bvec-append this tail)))))))))))


      ;; deprecated
      (define (flush-port fd)
         ;(mail fd 'flush)
         42)

      (define (close-port fd)
         (sys:close fd))



      ;;;
      ;;; STREAM BASED IO
      ;;;

      (define socket-read-delay 2)

      ;; In case one doesn't need asynchronous atomic io operations, one can use
      ;; threadless stream-based blocking (for the one thred) IO.

      ;; write a stream of byte vectors to a fd and
      ;; (bvec ...) fd → ll' n-written, doesn't close port
      ;;                  '-> null if all written without errors
      (define (blocks->port ll fd)
         (let loop ((ll ll) (n 0))
            (cond
               ((pair? ll)
                  (if (bytevector? (car ll))
                     (if (write-really (car ll) fd)
                        (loop (cdr ll) (+ n (size (car ll))))
                        (values ll n))
                     (values ll n)))
               ((null? ll)
                  (values ll n))
               (else
                  (loop (ll) n)))))

      (define (closing-blocks->port ll fd)
         (lets ((r n (blocks->port ll fd)))
            (sys:close fd)
            (values r n)))

      ;;;
      ;;; Rendering and sending
      ;;;

      ;; splice lst to bvecs and call write on fd
      (define (printer lst len out fd)
         (cond
            ((eq? len output-buffer-size)
               (and
                  (write-really (make-bytevector (reverse out)) fd)
                  (printer lst 0 null fd)))
            ((null? lst)
               (write-really (make-bytevector (reverse out)) fd))
            (else
               ;; avoid dependency on generic math in IO
               (printer (cdr lst) (++ len) (cons (car lst) out) fd))))

      (define (writer-to serializer)
         (λ (to obj)
            (printer (serializer obj '()) 0 null to)))



      (define (write-bytevector port bvec)
         (write-really bvec port))

      (define (write-bytes port byte-list)
         (printer byte-list 0 null port))

      (define write-bytestream (case-lambda
         ((bytestream)
               (write-bytes stdout bytestream))
         ((bytestream port)
               (write-bytes port bytestream))))

      ;; write each leaf chunk separately (note, no raw type testing here -> can fail)
      ;; (define (write-blob vec port)
      ;;    (let loop ((ll (blob-leaves vec)))
      ;;       (cond
      ;;          ((pair? ll)
      ;;             (write-bytevector port (car ll))
      ;;             (loop (cdr ll)))
      ;;          ((null? ll) #true)
      ;;          (else (loop (ll))))))

      ; TODO: speedup!!
      (define (display-to to obj)
         (printer (format-any obj #n) 0 null to))

      (define display (case-lambda
         ((obj) (display-to stdout obj))
         ((obj port) (display-to port obj))))

      (define write (case-lambda
         ((obj) ((writer-to (make-writer {'datum #true})) stdout obj))
         ((obj port) ((writer-to (make-writer {'datum #true})) port obj))))

      (define write-simple (case-lambda
         ((obj) ((writer-to (make-writer {'datum #false})) stdout obj))
         ((obj port) ((writer-to (make-writer {'datum #false})) port obj))))


      ; TODO: speedup!
      (define (print-to to . args)
         (printer (foldr format-any '(#\newline) args) 0 #null to))

      (define (print . args)
         (printer (foldr format-any '(#\newline) args) 0 #null stdout))


      ;; fixme: system-X do not belong here
      (define (system-print str)
         (sys:write stdout str (size str)))

      (define (system-println str)
         (system-print str)
         (system-print "\n"))

      (define (system-stderr str) ; <- str is a raw or pre-rendered string
         (sys:write stderr str (size str)))

      ;;;
      ;;; Files <-> vectors
      ;;;

      ;; read all blocks for a port, all but possibly last one having input-block-size bytes
      (define (read-blocks port buff)
         (lets ((eof-seen? val (get-whole-block port input-block-size)))
            (cond
               (eof-seen?
                  (let ((buff (if (eof? val) buff (cons val buff))))
                     (merge-chunks
                        (reverse buff)
                        (fold + 0 (map size buff)))))
               ((not val)
                  #false)
               (else
                  (read-blocks port
                     (cons val buff))))))

      (define (explode-block block tail)
         (let ((end (size block)))
            (if (eq? end 0)
               tail
               (let loop ((pos (- end 1)) (tail tail))
                  (if (eq? pos -1)
                     tail
                     (loop (- pos 1) (cons (ref block pos) tail)))))))

      (define (read-blocks->list port buff)
         (let ((block (get-block port 4096)))
            (cond
               ((eof? block)
                  (foldr explode-block null (reverse buff)))
               ((not block)
                  ;; read error
                  (foldr explode-block null (reverse buff)))
               (else
                  (read-blocks->list port (cons block buff))))))

      (define (maybe-open-file path)
         (if (equal? path "-")
            stdin
            (open-input-file path)))
      (define (maybe-open-binary-file path)
         (if (equal? path "-")
            stdin
            (open-binary-input-file path)))

      (define (maybe-close-port port)
         (if (eq? port stdin)
            #true
            (close-port port)))

      ; bytevector:
      (define (port->bytevector port) ; path -> vec | #false
         (if port
            (let ((stat (syscall 4 port)))
               (if stat
                  ; todo: add handling of not full file reading
                  ; (sleep and read again and bytevector-append then)
                  (sys:read port (ref stat 8))))))

      (define (file->bytevector path) ; path -> vec | #false
         (define stat (syscall 4 (c-string path)))
         (let*((port (open-binary-input-file path))
               (file (sys:read port (ref stat 8))))
            (close-port port)
            file))

      ;; fixme: no way to poll success yet. last message should be ok-request, which are not there yet.
      ;; fixme: detect case of non-bytevectors, which simply means there is a leaf which is not of type (raw 11)
      (define (bytevector->file vec path)
         (define port (open-binary-output-file path))
         (when port
            (define outcome (sys:write port vec #false))
            (close-port port)
            outcome))


      ; BLOB:
      ;; (define (fd->blob port) ; path -> vec | #false
      ;;    (if port
      ;;       (read-blocks port null)))

      ;; (define (file->blob path) ; path -> vec | #false
      ;;    (let*((port (maybe-open-file path))
      ;;          (blob (fd->blob port)))
      ;;       (maybe-close-port port)
      ;;       blob))

      ; list:
      (define (file->list path) ; path -> vec | #false
         (let ((port (maybe-open-binary-file path)))
            (if port
               (let ((data (read-blocks->list port null)))
                  (maybe-close-port port)
                  data))))

      (define (stream-chunk buff pos tail)
         (if (eq? pos 0)
            (cons (ref buff pos) tail)
            (lets ((next x (vm:sub pos 1)))
               (stream-chunk buff next
                  (cons (ref buff pos) tail)))))

      (define (port->bytestream fd)
         (λ ()
            (let ((buff (get-block fd input-block-size)))
               (cond
                  ((eof? buff)
                     (maybe-close-port fd)
                     null)
                  ((not buff)
                     ;(print "bytes-stream-port: no buffer received?")
                     null)
                  (else
                     (stream-chunk buff (- (size buff) 1)
                        (port->bytestream fd)))))))

      (define (bytestream->port stream port)
         (cond
            ((null? stream)
               #true)
            ((pair? stream)
               (case (sys:write port (bytevector (car stream)) 1)
                  (0 ; busy
                     (sleep 1)
                     (bytestream->port stream port))
                  (else
                     (bytestream->port (cdr stream) port))))
            (else
               (bytestream->port (force stream) port))))

      (define (lines fd)
         (let loop ((ll (port->bytestream fd)) (out null))
            (cond
               ((pair? ll)
                  (lets ((byte ll ll))
                     (if (eq? byte #\newline)
                        (lcons
                           (list->string
                              (reverse
                                 (if (and (pair? out) (eq? #\return (car out)))
                                    (cdr out)
                                    out)))
                           (loop ll null))
                        (loop ll (cons byte out)))))
               ((null? ll)
                  (if (null? out)
                     null
                     (list
                        (list->string (reverse out)))))
               (else
                  (loop (ll) out)))))

      (define (file->bytestream path)
         (let ((port (maybe-open-binary-file path)))
            (if port
               (port->bytestream port))))

      (define (bytestream->file stream path)
         (let ((port (open-binary-output-file path)))
            (if port
               (let ((outcome (bytestream->port stream port)))
                  (close-port port)
                  outcome))))

      (define (fasl-save obj path)
         (bytestream->file
            (fasl-encode obj)
            path))

      (define (fasl-load path fail-val)
         (let ((bs (file->bytestream path)))
            (if bs
               (fasl-decode bs fail-val)
               fail-val)))

      (define (deserialize-file path fail)
         (let ((stream (file->bytestream path)))
            (if stream (deserialize stream fail) fail)))

      (define (port->string port)
         (bytes->string (port->bytestream port)))

      (define (file->string port)
         (bytes->string (file->bytestream port)))

      (define (open-output-string)
         (syscall 85))

      (define (get-output-string port)
         (syscall 8 port 0 0)
         (bytes->string (port->bytestream port)))

      (define (open-input-string str)
         (let ((port (open-output-string)))
            (display-to port str)
            (syscall 8 port 0 0)
            port))

      (define (unbuffered-input-stream port)
         (lambda ()
            (define in (syscall 0 port 1))
            (case in
               (#f #null)  ; port error
               (#t ; input is not ready
                  (if (eq? port stdin)
                     (sleep 2)
                     (wait-read port 3000)) ; 3 seconds wait
                  (unbuffered-input-stream port))
               (#eof      ; end-of-file
                  (unless (eq? port stdin)
                     (close-port port))
                  #null)
               (else
                  (cons (ref in 0) (unbuffered-input-stream port))))))

      (define read-char
         (define (read-char port)
            (let* ((l r p val (rune #null
                                 (unbuffered-input-stream port)
                                 0 ; not used, any value
                                 (λ (l r p v) ; ok
                                    (values l r p v)))))
               (when l
                  val)))
         (case-lambda
            ((port)
               (read-char port))
            (()
               (read-char stdin))))

      (define read-line
         (define (read-line port)
            (let* ((l r p val ((let-parse* (
                                    (runes (greedy* (rune-if (lambda (r) (not (eq? r #\newline))))))
                                    (newline (either (imm #\newline) (epsilon #eof))))
                                 (if (and (null? runes) (eq? newline #eof))
                                    #false
                                    (runes->string runes)))
                                 #null ; no left part of stream
                                 (unbuffered-input-stream port)
                                 0 ; start position in the stream
                                 (λ (l r p v) ; ok
                                    (values l r p v)))))
               (when l
                  val)))

         (case-lambda
            ((port)
               (read-line port))
            (()
               (read-line stdin))))

))

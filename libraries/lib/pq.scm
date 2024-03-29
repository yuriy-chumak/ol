(define-library (lib pq)
   (export
      PQsetdb
      PQsetdbLogin
      PQfinish

      PQstatus
      CONNECTION_OK
      CONNECTION_BAD
      CONNECTION_STARTED
      CONNECTION_MADE
      CONNECTION_AWAITING_RESPONSE
      CONNECTION_AUTH_OK
      CONNECTION_SETENV
      CONNECTION_SSL_STARTUP
      CONNECTION_NEEDED
      CONNECTION_CHECK_WRITABLE
      CONNECTION_CONSUME
      CONNECTION_GSS_STARTUP

      PQresultStatus
	   PGRES_EMPTY_QUERY
	   PGRES_COMMAND_OK
	   PGRES_TUPLES_OK
	   PGRES_COPY_OUT
	   PGRES_COPY_IN
	   PGRES_BAD_RESPONSE
	   PGRES_NONFATAL_ERROR
	   PGRES_FATAL_ERROR
	   PGRES_COPY_BOTH
	   PGRES_SINGLE_TUPLE

      PQerrorMessage

      PQexec
      PQexecParams
      PQclear

      PQnfields
      PQfname
      PQntuples
      PQgetvalue

      ; high-level interface
      pq:query
   )
   (import
      (scheme base)
      (otus ffi))

; Step 1 — Installing PostgreSQL
;  sudo apt install postgresql postgresql-contrib
;  sudo systemctl start postgresql.service
; Step 2 — Using PostgreSQL Roles and Databases
; Step 3 — Creating a New Role
;  sudo -u postgres createuser --interactive
; Step 4 — Creating a New Database
;  sudo -u postgres createdb template1
(begin
   (define PGconn* fft-void*)
   (define PGresult* fft-void*)
   (define ConnStatusType fft-long) ; enum
   (define ExecStatusType fft-long) ; enum
   (define Oid fft-unsigned-int)
   (define Oid* (fft* Oid))

   (define CONNECTION_OK                0)
   (define CONNECTION_BAD               1)
   (define CONNECTION_STARTED           2)
   (define CONNECTION_MADE              3)
   (define CONNECTION_AWAITING_RESPONSE 4)
   (define CONNECTION_AUTH_OK           5)
   (define CONNECTION_SETENV            6)
   (define CONNECTION_SSL_STARTUP       7)
   (define CONNECTION_NEEDED            8)
   (define CONNECTION_CHECK_WRITABLE    9)
   (define CONNECTION_CONSUME          10)
   (define CONNECTION_GSS_STARTUP      11)

	(define PGRES_EMPTY_QUERY 0)
	(define PGRES_COMMAND_OK 1)
	(define PGRES_TUPLES_OK 2)
	(define PGRES_COPY_OUT 3)
	(define PGRES_COPY_IN 4)
	(define PGRES_BAD_RESPONSE 5)
	(define PGRES_NONFATAL_ERROR 6)
	(define PGRES_FATAL_ERROR 7)
	(define PGRES_COPY_BOTH 8)
	(define PGRES_SINGLE_TUPLE 9)

   (define PG (or
      (load-dynamic-library "libpq.so")
      (runtime-error "No libpq found. Try to 'sudo apt install libpq-dev'")))


   (define PQsetdbLogin (PG PGconn* "PQsetdbLogin" type-string type-string type-string type-string type-string type-string type-string))
   (define (PQsetdb host port opt tty dbname)
      (PQsetdbLogin host port opt tty dbname #f #f))

   (define PQstatus (PG ConnStatusType "PQstatus" PGconn*))
   (define PQerrorMessage (PG type-string "PQerrorMessage" PGconn*))

   (define PQresultStatus (PG ExecStatusType "PQresultStatus" PGresult*))

   (define PQfinish (PG fft-void "PQfinish" PGconn*))

   (define PQexec (PG PGresult* "PQexec" PGconn* type-string))
   (define PQexecParams (PG PGresult* "PQexecParams" PGconn*
			type-string        ; command
         fft-int            ; nParams
         (fft* Oid)         ; paramTypes
         (fft* type-string) ; paramValues
         (fft* fft-int)     ; paramLengths
         (fft* fft-int)     ; paramFormats
         fft-int))          ; resultFormat

   (define PQclear (PG fft-void "PQclear" PGresult*))

   (define PQnfields (PG fft-int "PQnfields" PGresult*))
   (define PQfname (PG type-string "PQfname" PGresult* fft-int))
   (define PQntuples (PG fft-int "PQntuples" PGresult*))
   (define PQgetvalue (PG type-string "PQgetvalue" PGresult* fft-int fft-int))

   ; high-level interface
   (define (pq:query db query . arguments)
      (define args (map (lambda (arg)
            (cond
               ((number? arg)
                  (number->string arg))
               ((string? arg)
                  arg)
               (else
                  (runtime-error "unsupported argument type" arg))))
         arguments))

      (define res (PQexecParams db query (length args) #F args #F #F 0))
      (unless (eq? (PQresultStatus res) PGRES_TUPLES_OK)
         (define message (PQerrorMessage db))
         (PQclear res)
         (runtime-error "QUERY failed:" message))

      (define nFields (PQnfields res))

      (define (query n)
         (lambda ()
            (if (eq? n (PQntuples res))
            then
               (PQclear res)
               ;(PQclear (PQexec db "CLOSE portal"))
               #null
            else
               (let loop ((i 0) (out #null))
                  (if (eq? i nFields)
                     (cons out (query (++ n)))
                  else
                     (loop (++ i) (cons (PQgetvalue res n i) out)))))))
      (force (query 0)))
))

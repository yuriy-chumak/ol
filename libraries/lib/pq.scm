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
      PQclear

      PQnfields
      PQfname
      PQntuples
      PQgetvalue
   )
   (import
      (scheme core)
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
   (define PQclear (PG fft-void "PQclear" PGresult*))

   (define PQnfields (PG fft-int "PQnfields" PGresult*))
   (define PQfname (PG type-string "PQfname" PGresult* fft-int))
   (define PQntuples (PG fft-int "PQntuples" PGresult*))
   (define PQgetvalue (PG type-string "PQgetvalue" PGresult* fft-int fft-int))

))

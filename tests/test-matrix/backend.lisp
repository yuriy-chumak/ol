#!/usr/bin/env ol

; ====================================================================
; подключение и настройка базы даных
; --------------------------------------------------------------------
(define *features* (cons 'sqlite-log-debug *features*)) ; enable debug
(import (lib sqlite))

; simplify database access functions
; --------------------------------------------------------------------
(define-syntax db:query
   (syntax-rules (database)
      ((db:query . args)
         (sqlite:query database . args))))

(define-syntax db:value
   (syntax-rules (database)
      ((db:value . args)
         (sqlite:value database . args))))

(define db:map sqlite:map)

(define (db:apply statement f)
   (cond
      ((pair? statement)
         (apply f statement))
      (#true #false)
      (statement
         (f statement))))

; ------------------------
; - log colors:
(define RED "\e[0;31m")
(define GREEN "\e[0;32m")
(define YELLOW "\e[0;33m")
(define BLUE "\e[0;34m")
(define MAGENTA "\e[0;35m")
(define CYAN "\e[0;36m")
(define WHITE "\e[0;37m")
(define END "\e[0;0m")
(define (LOGD . args)
   (apply print-to (cons stderr args)))

; ---=( database handler) =-------------------------------------------
(actor 'database (lambda ()
   (define database (make-sqlite3)) ; make new sql database connection
   (sqlite3_open "database.sqlite" database)
   (db:query "PRAGMA foreign_keys = ON") ; enable foreign keys support

   (db:query "
      CREATE TABLE IF NOT EXISTS tests (
         session INTEGER  -- test session id (test cycle)
      ,  timestamp DATETIME DEFAULT CURRENT_TIMESTAMP

      ,  runner TEXT   -- test group (runner: FFI, REGRESSION, REFERENCE, etc.)
      ,  test TEXT     -- test name (filename)
      ,  platform TEXT -- test platform (x86_64, aarch64, ...)
      ,  target TEXT   -- test platform type (32-d, 64-r, etc)

      ,  status INTEGER -- test result
                           -- NULL: no result
                           -- 0: failed
                           -- 1: ok
      )")
   (db:query "
      CREATE UNIQUE INDEX IF NOT EXISTS tests_index ON
      tests (session, runner, test, platform, target)")

   ;; (define queued (file->string "img/queued.svg"))
   ;; (define ok (file->string "img/ok.svg"))
   ;; (define fail (file->string "img/fail.svg"))

   (define status-query (sqlite:prepare database "
      SELECT status
        FROM tests
       WHERE session = (SELECT MAX(session) FROM tests)
         AND runner = ?1
         AND test = ?2
         AND platform = ?3
         AND target = ?4"))

   (let loop ()
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (case msg
            (['new-test-session build] ; start new test cycle
               (mail sender
                  (db:value "INSERT INTO tests (session, status)
                                    VALUES ((SELECT COALESCE(MAX(session),0) + 1 FROM tests), ?1)
                              RETURNING session" build))
               (loop))

            (['put id runner test platform target status]
               (db:query "
                     INSERT OR REPLACE
                       INTO tests (session, timestamp,
                                    runner, test, platform, target,  status)
                     VALUES (?1, CURRENT_TIMESTAMP, ?2,?3,?4,?5, ?6)"
                     id runner test platform target status)
               (loop))

            ('get-platforms
               (mail sender
                  (db:map (db:query "SELECT DISTINCT platform
                                       FROM tests
                                      WHERE platform IS NOT NULL
                                      ORDER BY timestamp") idf))
               (loop))
            (['get-targets platform]
               (mail sender
                  (db:map (db:query "SELECT DISTINCT target
                                       FROM tests
                                      WHERE platform = ?
                                        AND target IS NOT NULL" platform) idf))
               (loop))

            ('get-runners
               (mail sender
                  (db:map (db:query "SELECT DISTINCT runner
                                       FROM tests
                                      WHERE runner IS NOT NULL") idf))
               (loop))
            (['get-tests runner]
               (mail sender
                  (db:map (db:query "SELECT DISTINCT test
                                       FROM tests
                                      WHERE runner = ?
                                        AND test IS NOT NULL" runner) idf))
               (loop))

            ; get test status
            (['status runner test platform target]
               (mail sender (db:value status-query runner test platform target))
               (sqlite3_reset status-query) ; cleanup query
               (loop))

            ('build-info
               (mail sender
                  (db:value "SELECT timestamp, status
                               FROM tests
                              WHERE session = (SELECT MAX(session) FROM tests)
                                AND test IS NULL"))
               (loop))

            (['test-status runner test]
               (mail sender (list->vector
                  (db:map (db:query "SELECT platform, target, status
                                       FROM tests
                                      WHERE session = (SELECT MAX(session) FROM tests)
                                        AND runner = ?1
                                        AND test = ?2" runner test)
                     (lambda (platform target status) {
                           'platform platform
                           'target target
                           'status status
                        }))))
               (loop))

            (['runner-status runner]
               (mail sender (list->vector
                  (db:map (db:query "SELECT test, platform, target, status
                                       FROM tests
                                      WHERE session = (SELECT MAX(session) FROM tests)
                                        AND runner = ?1" runner)
                     (lambda (test platform target status) {
                           'test test
                           'platform platform
                           'target target
                           'status status
                        }))))
               (loop))


            (else
               (loop)))))

   (sqlite3_close database)))

; -=( http run 8008 )=-------------------------------------
(import (lib http)
   (file json)
   (owl parse))
(http:run 8008 (lambda (fd request headers stream return)
   (LOGD "\nRequest: " BLUE request END)
   (LOGD "\nHeaders: " WHITE headers END)

   ; not a valid http (https or websocket or smth.)
   (unless (vector? request)
      (return #true))

   ; аналог функции write для сокета
   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))

   ; ----------------------------------
   ; http/ response
   (let*((al (ref request 2))   ; al - address line
         (pu (http:parse-url al)) ; pu - parsed url
         (path (ref pu 1))
         (args (ref pu 2))

         (content-type (headers 'content-type #f))
         (content-length (headers 'content-length "0"))
         (_ (print "content-type: " content-type))
         (_ (print "content-length: " content-length))

         (body stream (uncons (try-parse
                                 (let-parse* ((body (times (string->number content-length) byte))) body) stream) #f)))

      (define (respond color status)
         (LOGD color "Sending " status END)
         (send "HTTP/1.1 " status "\r\n")
         (send "Content-Type: text/html"        "\r\n"
               "Cache-Control: no-store"        "\r\n"
               "Connection: keep-alive"         "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\r\n"
               "Content-Length: 0"              "\r\n"
               "\r\n")
         (return stream))

      ; http/ response codes
      (define (send-200) (respond GREEN "200 OK"))        (define send-ok send-200)
      (define (send-404) (respond RED   "404 Not Found")) (define send-not-found send-404)

      ; отправить file в сокет
      (define (send-file name)
         (define bytes (file->bytevector name))

         (LOGD "Sending " GREEN name END " (" (size bytes) ")")
         (send "HTTP/1.1 " "200 OK" "\r\n")
         (send "Content-Type: " (cond
                     ((m/\.html?$/ name) "text/html")
                     ((m/\.png$/   name) "image/png")
                     (else "")) "\r\n"
               "Content-Length: " (size bytes)  "\r\n"
               ;; "Cache-Control: no-store"        "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\r\n"
               "Connection: keep-alive"         "\r\n"
               "\r\n")
         (write-bytevector fd bytes)
         (return stream))

      ; отправить json в сокет
      (define (send-json json)
         (define bytes (string->utf8 (stringify json)))

         (LOGD "Sending json (with 200 OK): " GREEN (size bytes) END " bytes length")
         (send "HTTP/1.1 " "200 OK" "\r\n")
         (send "Content-Type: application/json" "\r\n"
               "Content-Length: " (size bytes)  "\r\n"
               "Cache-Control: no-store"        "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\r\n"
               "Connection: keep-alive"         "\r\n"
               "\r\n")
         (write-bytevector fd bytes)
         (return stream))

      ; -- main --
      (cond
         ((= (ref request 1) "POST")
            (define session (number->string
               (await (mail 'database ['new-test-session (args 'build)]))))
            (LOGD GREEN "Sending " 200 END)
            (send "HTTP/1.1 " "200 OK"       "\r\n")
            (send "Content-Type: text/html"  "\r\n"
                  "Cache-Control: no-store"  "\r\n"
                  "Connection: keep-alive"   "\r\n"
                  "Server: " (car *version*) "/" (cdr *version*) "\r\n"
                  "Content-Length: " (string-length session)     "\r\n"
                  "\r\n" session)
            (return stream))

         ((= (ref request 1) "PUT")
            (print "body: " body)
            (define json (read-json body))
            (print "json: " json)
            (mail 'database ['put
                  (json 'session)
                  (json 'runner) (json 'name) (json 'platform) (json 'target)
                  (json 'status)])
            (send-200))

         ((= (ref request 1) "GET")
            (print "REQ: " path)
            (cond
               ((= path "/ol/test-matrix/")
                  (send-file "index.html"))
               ((= path "/ol/test-matrix/img/favicon.png")
                  (send-file "img/favicon.png"))
               ((= path "/favicon.ico")
                  (send-file "img/favicon.png"))
               ; 
               ((= path "/ol/test-matrix/runners")
                  (send-json (list->vector (await (mail 'database 'get-runners)))))
               ((= path "/ol/test-matrix/platforms")
                  (send-json (list->vector (await (mail 'database 'get-platforms)))))
               ((= path "/ol/test-matrix/targets")
                  (send-json (list->vector (await (mail 'database ['get-target (args 'platform)])))))
               ((= path "/ol/test-matrix/tests")
                  (send-json (list->vector (await (mail 'database ['get-tests (args 'runner)])))))
               ((= path "/ol/test-matrix/status")
                  (send-json (await (mail 'database ['status
                     (args 'runner)
                     (args 'test)
                     (args 'platform)
                     (args 'target)
                  ]))))
               ((= path "/ol/test-matrix/test-status")
                  (send-json (await (mail 'database ['test-status
                     (args 'runner)
                     (args 'test)
                  ]))))
               ((= path "/ol/test-matrix/runner-status")
                  (send-json (await (mail 'database ['runner-status
                     (args 'runner)
                  ]))))

               ((= path "/ol/test-matrix/build-info")
                  (print (await (mail 'database 'build-info)))
                  (send-json (apply (lambda (timestamp build) {
                                          'timestamp timestamp
                                          'build build
                                       })
                                 (await (mail 'database 'build-info)))))
               (else #f))
            (send-404))

         (else
            (send-not-found))))))

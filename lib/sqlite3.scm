;;; SQLite3 interface for OL
;;; https://github.com/yuriy-chumak/OL
;;; http://www.sqlite.org

;;; Copyright (c) 2014, Yuriy Chumak
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Use in source and binary forms are not permitted in projects under
;;;    GNU General Public Licenses and its derivatives.
;;;
;;; THIS SOFTWARE IS PROVIDED BY ART OBREZAN ''AS IS'' AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL ART OBREZAN BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-library (lib sqlite3)
  (export
  ; types
    ;sqlite3* sqlite3_value sqlite3_stmt
    make-sqlite3 make-sqlite3-stmt
    
  ; constants
    SQLITE-OK SQLITE-ERROR SQLITE-BUSY SQLITE-LOCKED
    SQLITE-DONE SQLITE-ROW
    SQLITE-STATIC SQLITE-TRANSIENT
    SQLITE-INTEGER SQLITE-FLOAT SQLITE-BLOB SQLITE-NULL SQLITE-TEXT
  
  ; creation/destruction
    sqlite3-open
    sqlite3-close
    
  ; statement management
    sqlite3-prepare-v2
    sqlite3-step
    sqlite3-reset
    sqlite3-finalize
    
  ; result set
    sqlite3-column-count
    sqlite3-column-name
    sqlite3-column-int
    sqlite3-column-bytes
    ;sqlite3_column_double
    sqlite3-column-text
    ;sqlite3_column_blob
  )

  (import
      (owl defmac) (owl io)
      (owl list) (owl string)
      (owl math) (owl vector)
      (owl error)
      (owl pinvoke))
  (begin

(define % (dlopen "sqlite3" 0))
(if (null? %)
   (begin
      (print "Can't load sqlite3 library. Will halt")
;      (case *OS*
;         (0 (print "Download dll from http://www.sqlite.org/download.html"))
;         (1 (print "apt-get install sqlite3"))) 
      (print "@")
      (exit-owl 1)))

; служебные 
(define (make-sqlite3)      (list->byte-vector '(0 0 0 0)))
(define (make-sqlite3-stmt) (list->byte-vector '(0 0 0 0)))

; todo: завести под это дело отдельный тип - что-то вроде type-int+-ref и т.д.
(define sqlite3*  type-int+)
(define sqlite3** type-vector-raw)
(define sqlite3_stmt*  type-int+)
(define sqlite3_stmt** type-vector-raw)
(define char** type-vector-raw) ; тут проблема!!!

(define sqlite3_value type-fix+)
(define sqlite3_stmt  type-int+)
(define sqlite3_int64 type-vector-raw)

(define SQLITE-OK 0)
(define SQLITE-ERROR 1)
(define SQLITE-BUSY 5)
(define SQLITE-LOCKED 6)

(define SQLITE-DONE 101)
(define SQLITE-ROW 100)

(define SQLITE-STATIC 0)
(define SQLITE-TRANSIENT -1)

(define SQLITE-INTEGER 1)
(define SQLITE-FLOAT 2)
(define SQLITE-BLOB 4)
(define SQLITE-NULL 5)
(define SQLITE-TEXT 3)

(define SQLITE-MISUSE 21)


(define sqlite3-open  (dlsym % (__cdecl type-fix+) "sqlite3_open"  type-string sqlite3**))
(define sqlite3-close (dlsym % (__cdecl type-fix+) "sqlite3_close" sqlite3*))

(define sqlite3-prepare-v2 (dlsym % (__cdecl type-fix+) "sqlite3_prepare_v2" sqlite3* type-string type-fix+ sqlite3_stmt** char**)) ; проблема с крайним параметром (char**) - надо этот результат сконвертировать снова в строку, новую
(define sqlite3-sql      (dlsym % (__cdecl type-string) "sqlite3_sql"        sqlite3_stmt*))
(define sqlite3-step       (dlsym % (__cdecl type-fix+) "sqlite3_step"       sqlite3_stmt*))
(define sqlite3-reset      (dlsym % (__cdecl type-fix+) "sqlite3_reset"      sqlite3_stmt*))
(define sqlite3-finalize   (dlsym % (__cdecl type-fix+) "sqlite3_finalize"   sqlite3_stmt*))

(define sqlite3-column-count (dlsym % (__cdecl type-fix+) "sqlite3_column_count" sqlite3_stmt*))
(define sqlite3-column-name  (dlsym % (__cdecl type-string) "sqlite3_column_name" sqlite3_stmt* type-fix+))
(define sqlite3-column-int   (dlsym % (__cdecl type-int+) "sqlite3_column_int" sqlite3_stmt* type-fix+))
(define sqlite3-column-bytes (dlsym % (__cdecl type-int+) "sqlite3_column_bytes" sqlite3_stmt* type-fix+))
;sqlite3_column_double
(define sqlite3-column-text  (dlsym % (__cdecl type-string) "sqlite3_column_text" sqlite3_stmt* type-fix+))
;(define sqlite3_column_blob  (dlsym % (__cdecl type-string) "sqlite3_column_blob" sqlite3_stmt* type-fix+))

))
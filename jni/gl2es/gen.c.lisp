#!/usr/bin/env ol

(import (file json))

(define PREFIX "real_")
(define GL2ES "gl2es_")

(print "
#include <GLES3/gl3.h>
#include <log.h>

#include <gles-2.0.h>
#define GL2ES(name) " GL2ES "##name
")

(define json (read-json stdin))
(ff-for-each (lambda (name arguments)
      (print "//")
      (define return (ref arguments 1))
      (define args (cdr (vector->list arguments)))

      ;; we have function implementation
      (define stat (syscall 4 (c-string (string-append "src/" (symbol->string name) ".c"))))

      ;; external function
      (for-each display (list return " " "(* " PREFIX name ")"))
      (display "(")
      (unless (null? args)
         (let loop ((args args))
            (display (car args))
            (unless (null? (cdr args))
               (display ", ")
               (loop (cdr args)))))
      (print ") = 0;")

      (print "__attribute__((visibility(\"default\")))")
      (print return " " name "(")
      (unless (null? args)
         (let loop ((args args))
            (display "    ")
            (display (car args))
            (unless (null? (cdr args))
               (print ", ")
               (loop (cdr args)))))
      (if stat
         (print ");")
      else
         (print ")")
         (print "{")

         ;;(print "   ILOG(\"" name "()\");");

         (if (string-eq? return "void")
            (display "   (void) ")
            (display "   return "))
         (for-each display (list PREFIX name "("))
         (unless (null? args)
            (let loop ((args args))
               (display (last (c/ / (car args)) #false))
               (unless (null? (cdr args))
                  (display ", ")
                  (loop (cdr args)))))
         (print ");")
         (print "}")))
   json)

(print "// ----------------------------------------------------------------- //
#define LINK(name) " PREFIX "##name = dlsym(GLES, #name)
extern void Init(void);

#include <dlfcn.h>
void* GLES = 0;
__attribute__((visibility(\"default\")))
void initialize_gl2es() {
   GLES = dlopen(\"libGLESv2.so\", RTLD_LAZY);
")

(ff-for-each (lambda (name arguments)
      (print "   LINK(" name ");"))
   json)
(print "   Init();")
(print "}")

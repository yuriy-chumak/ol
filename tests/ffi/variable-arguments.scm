#!/usr/bin/env -S ../../ffi
,load "definitions"

(print "
// variable arguments test (returns count of printed arguments):
int format(const char *format, ...)
{
   printf(format, ...);
}")

(let*((format (this fft-int "format" type-string))
      (typename (lambda (id)
         (case id
            (fft-int 'fft-int)
            (fft-long-long 'fft-long-long)
            (fft-float 'fft-float)
            (fft-double 'fft-double)
            (else (typename id 'unknown)))))
      (try (lambda (tag . args)
               (for-each display (list "   " tag " ("))
               (write (car args))
               (for-each (lambda (arg)
                     (if (pair? arg)
                        (for-each display (list
                           " (" (typename (car arg)) " . " (cdr arg) ")"))
                     else
                        (display " ") (write arg)))
                  (cdr args))
               (display ")>")
               (let ((out (apply format args)))
                  (print " = " out)))))

   (try "numbers" "%i %f %i %f"
      (cons fft-int 42)
      (cons fft-double -1e10)
      (cons fft-int -42)
      (cons fft-double +1e10))

   (try "utf-8 strings/symbols" "<%s/%s/%s/%s>"
      "ansi"
      "юнікод"
      '|σύμβολο|
      "ユニコード")
   (try "empty strings/symbols" "<%s/%s/%s>"
      "" (substring "λ" 1 1) '||)
)

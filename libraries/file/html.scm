(define-library (file html)
(import
   (otus lisp))

(export
   write-html

   html->string
)

; html is:
;   #('tag #(attributes) '(children))
; where child:
;   string if value or list of elements if not

(begin
   ; todo?: make a ff with keys #t for name, #n for value, #e for attributes
   (define (html-get-name root)
      (ref root 1))
   (define (html-get-attributes root)
      (ref root 2))
   (define (html-get-value root)
      (ref root 3))

   (define (html-get-attribute root name default-value)
      (get (html-get-attributes root) name default-value))

   (define (html-get-subtags root name)
      (filter (lambda (tag) (eq? (html-get-name tag) name)) (html-get-value root)))

   (define (html-get-subtag root name)
      (let ((subtags (html-get-subtags root name)))
         (unless (null? subtags)
            (car subtags))))


   ;; (define (html-get-int-attribute root name default-value)
   ;;    (let ((value (html-get-attribute root name #false)))
   ;;       (if value (string->number value 10) default-value)))

   ; printing the html:
   (define (print-html-with display html)
      (define (print-tag tag)
         (when tag
            (cond
               ((string? tag)
                  (display tag))
               ((list? tag)
                  (for-each print-tag tag))
               ((vector? tag)
                  (display "<")
                  (display (html-get-name tag))
                  (ff-for-each (lambda (key value)
                        (for-each display (list " " key "=\"" value "\"")))
                     (or (html-get-attributes tag) {}))
                  (if (or (eq? (html-get-value tag) #f)
                        (null? (html-get-value tag)))
                     (display "/>")
                  else
                     (display ">")
                     (define value (html-get-value tag))
                     (if (list? value)
                        (for-each print-tag value)
                        (print-tag value))
                     (display "</")
                     (display (html-get-name tag))
                     (display ">")))
               (else
                  (runtime-error "invalid HTML" tag)))))

      (print-tag html))

   (define write-html
      (define (write-html html port)
         (print-html-with (lambda (what) (display-to port what)) html))
      (case-lambda
         ((html) (write-html html stdout))
         ((html file) (if (port? file)
                        (write-html html file)
                     else
                        (define port (if (equal? file "-") stdout (open-output-file file)))
                        (write-html html port)
                        (unless (eq? port stdout)
                           (close-port port)))) ))

   (define (html->string html)
      (define port (open-output-string))
      (write-html html port)
      (get-output-string port))

))

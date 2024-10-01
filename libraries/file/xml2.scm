(define-library (file xml2)
(import
   (otus lisp)
   (only (scheme misc) string->number)
   (owl parse))

(export
   xml-parser
   xml-parse-file

   read-xml
   read-xml-file

   read-xml-port   ; same as read-xml
   read-xml-string ; same as read-xml
   read-xml-stream ; same as read-xml

   write-xml

   xml-get-root-element
   xml-get-attributes
   xml-get-name
   xml-get-attribute
   xml-get-value

   xml-get-subtags
   xml-get-subtag

   xml-print

   xml-get-int-attribute

   ; let's try new xml interface:
   xml:root

   xml:attributes
   xml:name
   xml:value

   xml:attribute

   xml->string
)

; parsed xml is:
;   #('xml #(attributes) body)
; where body:
;   #('tag #(attributes) '(children))
; where child:
;   vector if tag or string if value

; legend: #[] is vector
;         #() is ff (dictionary) with symbols as keys

(begin
   ; special symbols decoding
   ;(define @< (string->regex "r/&lt;/<"))

   ; transcodings
   (define windows-1251->utf-8 {
      #xC0 #\А  #xC1 #\Б  #xC2 #\В  #xC3 #\Г  #xC4 #\Д  #xC5 #\Е  #xC6 #\Ж  #xC7 #\З
      #xC8 #\И  #xC9 #\Й  #xCA #\К  #xCB #\Л  #xCC #\М  #xCD #\Н  #xCE #\О  #xCF #\П
      #xD0 #\Р  #xD1 #\С  #xD2 #\Т  #xD3 #\У  #xD4 #\Ф  #xD5 #\Х  #xD6 #\Ц  #xD7 #\Ч
      #xD8 #\Ш  #xD9 #\Щ  #xDA #\Ъ  #xDB #\Ы  #xDC #\Ь  #xDD #\Э  #xDE #\Ю  #xDF #\Я
      #xE0 #\а  #xE1 #\б  #xE2 #\в  #xE3 #\г  #xE4 #\д  #xE5 #\е  #xE6 #\ж  #xE7 #\з
      #xE8 #\и  #xE9 #\й  #xEA #\к  #xEB #\л  #xEC #\м  #xED #\н  #xEE #\о  #xEF #\п
      #xF0 #\р  #xF1 #\с  #xF2 #\т  #xF3 #\у  #xF4 #\ф  #xF5 #\х  #xF6 #\ц  #xF7 #\ч
      #xF8 #\ш  #xF9 #\щ  #xFA #\ъ  #xFB #\ы  #xFC #\ь  #xFD #\э  #xFE #\ю  #xFF #\я
      #xA8 #\Ё  #xB8 #\ё
   })


   ; utils:
   (define (between? lo x hi) ; fast version of (<= lo x hi)), where x is rune
      (and (or (less? lo x) (eq? lo x))
           (or (less? x hi) (eq? x hi))))

   (define (number? n) (between? #\0 n #\9))
   (define (character? n) (or
      (between? #\a n #\z)
      (between? #\0 n #\9)
      (between? #\A n #\Z)
      (eq? n #\-) (eq? n #\:)))
      ; ? xml standard: "any Unicode character, excluding the surrogate blocks, FFFE, and FFFF."
      ;(between? #x7F n #xD7FF)
      ;(between? #xE000 n #xFFFD)
      ;(between? #x10000 n #x10FFFF)))

   (define (whitespace? x)
      (has? '(#\tab #\space #\newline #\return) x))

   (define angles {
      #\< #true
      #\> #true
   })
   (define not-angles {
      #\< #false
      #\> #false
   })
   (define (non-angle x) (not-angles x #true))

   (define skip-whitespaces
      (get-any-of (get-greedy* (byte-if whitespace?))))

   (define (get-attribute encoding)
      (let-parses (
            (name (get-greedy+ (byte-if character?)))
            (= (get-imm #\=))
            (* (get-imm #\"))
            (value (get-greedy* ((encoding 'char-if) (lambda (x) (not (eq? x #\"))))))
            (* (get-imm #\"))
            (* skip-whitespaces))
         (cons
            (string->symbol (runes->string name))
            (runes->string value))))

   ; encodings
   (define ansi {
      ;; 'char byte
      'char-if byte-if
      '->string runes->string
   })
   (define utf-8 {
      ;; 'char rune
      'char-if rune-if
      '->string runes->string
   })
   (define windows-1251 {
      ;; 'char byte
      'char-if byte-if
      '->string (lambda (bytes)
            (runes->string (map (lambda (byte) (get windows-1251->utf-8 byte byte)) bytes)))
   })
   (define windows-1252 {
      'char-if (lambda (rule)
            (let-parse* (
                  (char (either
                           (let-parse* ( ; &#NN..N;
                                 (? (imm #\&))
                                 (? (imm #\#))
                                 (numbers (greedy+ (byte-if number?)))
                                 (? (imm #\;))
                                 (value (epsilon (fold (lambda (n x) (+ (* n 10) (- x #\0))) 0 numbers))))
                              value)
                           byte))
                  (verify (rule char) `("bad character '" char)))
               char))
      '->string runes->string
   })

   ; well, either the raw text or the set of subtags
   ; пока не будем смешивать вместе текст и теги - либо то либо другое
   (define (get-tag encoding)
      (get-greedy+
         ; parse tag with attributes
         (let-parse* (
               (< (imm #\<))
               (name (greedy+ (byte-if character?)))
               (? skip-whitespaces)
               ;; (? (epsilon (print "got tag " (list->string name))))
               (attributes (get-greedy* (get-attribute encoding)))
               (body (get-either
                  (get-word "/>" #null)        ; />
                  (let-parse* (
                        (? (get-imm #\>))      ; >
                        (spaces skip-whitespaces)   ; must skip
                        (body (greedy* (let-parse* (
                                 (body (either
                                          (get-tag encoding)
                                          (let-parse* (
                                                (body (greedy+ ((encoding 'char-if) non-angle))))
                                             ((encoding '->string) body)))))
                           body)))
                        ; closing tag
                        (? (get-word "</" #t)) ; </tag
                        (? (get-word (runes->string name) #t))
                        (? (get-imm #\>)))     ; >
                     (if (eq? (length body) 1)
                        (car body)
                        body))))
               (* skip-whitespaces))
            (vector
               (string->symbol (runes->string name))
               (pairs->ff attributes)
               body))))

   (define xml-parser
      (let-parse* (
            ;<?xml version="1.0" encoding="UTF-8"?>
            (? (word "<?xml " #t))
            (? skip-whitespaces)
            (attributes (get-greedy* (get-attribute ansi)))
            (* (get-word "?>" #true))

            (encoding (epsilon (begin
               (define encoding (cdr (or (assq 'encoding attributes) '(#f . ""))))
               (cond
                  ((string-eq? encoding "utf-8") utf-8)
                  ((string-eq? encoding "windows-1251") windows-1251)
                  ((string-eq? encoding "windows-1252") windows-1252)
                  (else utf-8)))))

            ; xml tags
            (* skip-whitespaces)
            (body (get-tag encoding)))
         ['xml (pairs->ff attributes) body]))

   (define (xml-parse-file filename)
      (let ((file (open-input-file filename)))
         (if file
            (let ((o (parse xml-parser (port->bytestream file) filename "xml parse error" #false)))
               (if o o
                  (close-port file)))))) ; no automatic port closing on error

   (define (xml-get-root-element xml)
      (let ((root (ref xml 3)))
         (if (list? root)
            (car root)
            root)))

   ; todo?: make a ff with keys #t for name, #n for value, #e for attributes
   (define (xml-get-name root)
      (ref root 1))
   (define (xml-get-attributes root)
      (ref root 2))
   (define (xml-get-value root)
      (ref root 3))

   (define (xml-get-attribute root name default-value)
      (get (xml-get-attributes root) name default-value))

   (define (xml-get-subtags root name)
      (filter (lambda (tag) (eq? (xml-get-name tag) name)) (xml-get-value root)))

   (define (xml-get-subtag root name)
      (let ((subtags (xml-get-subtags root name)))
         (unless (null? subtags)
            (car subtags))))


   (define (xml-get-int-attribute root name default-value)
      (let ((value (xml-get-attribute root name #false)))
         (if value (string->number value 10) default-value)))

   (define xml:root xml-get-root-element)

   (define xml:attributes xml-get-attributes)
   (define xml:name xml-get-name)
   (define xml:value xml-get-value)

   (define xml:attribute xml-get-attribute)

   ; printing the xml:
   (define (print-xml-with display xml)
      (define (print-tag tag)
         ;; (print "\n(print-tag " tag ")")
         (if (string? tag)
            (display tag)
         else
            (display "<")
            (display (xml-get-name tag))
            (ff-for-each (lambda (key value)
                  (for-each display (list " " key "=\"" value "\"")))
               (or (xml-get-attributes tag) {}))
            (if (or (eq? (xml-get-value tag) #f)
                    (null? (xml-get-value tag)))
               (display "/>")
            else
               (display ">")
               (define value (xml-get-value tag))
               (if (list? value)
                  (for-each print-tag value)
                  (print-tag value))
               (display "</")
               (display (xml-get-name tag))
               (display ">"))))

      ; header
      (display "<?xml")
      (ff-fold (lambda (? key value)
            (for-each display (list " " key "=\"" value "\"")))
         #f (xml-get-attributes xml))
      (display "?>\n")
      (print-tag (xml-get-root-element xml)))
      ; tags
      ;; (let loop ((root (xml-get-root-element xml)) (indent ""))
      ;;    (display indent)
      ;;    (display "<")
      ;;    (display (xml-get-name root))
      ;;    (ff-fold (lambda (? key value)
      ;;          (for-each display (list " " key "=\"" value "\"")))
      ;;       #f (xml-get-attributes root))

      ;;    (if (null? (xml-get-value root))
      ;;       (display "/>\n")
      ;;    else
      ;;       (let ((value (xml-get-value root)))
      ;;          (display ">")
      ;;          (if (string? value)
      ;;             (display value)
      ;;             (begin
      ;;                (display "\n")
      ;;                (for-each (lambda (child)
      ;;                      (loop child (string-append "   " indent)))
      ;;                   value)
      ;;                (display indent)))
      ;;          (display "</")
      ;;          (display (xml-get-name root))
      ;;          (display ">\n"))))

   (define (read-xml-stream stream)
      (when stream
         (define xml (try-parse xml-parser stream #f))
         (if xml (car xml))))

   (define (read-xml-port port)
      (when port
         (read-xml-stream (force (port->bytestream port)))))

   (define (read-xml-string str)
      (when str
         (read-xml-stream (str-iter-bytes str))))

   (define read-xml (case-lambda
      (() (read-xml-port stdin))
      ((source) (cond
         ((port? source) (read-xml-port source))
         ((string? source) (read-xml-string source))
         ((pair? source) (read-xml-stream source))))))

   (define (read-xml-file filename)
      (read-xml (if (equal? filename "-")
                     stdin
                     (open-input-file filename)))) ; note: no need to close port

   (define write-xml
      (define (write-xml xml port)
         (print-xml-with (lambda (what) (display-to port what)) xml))
      (case-lambda
         ((xml) (write-xml xml stdout))
         ((xml file) (if (port? file)
                        (write-xml xml file)
                     else
                        (define port (if (equal? file "-") stdout (open-output-file file)))
                        (write-xml xml port)
                        (unless (eq? port stdout)
                           (close-port port)))) ))

   (define (xml->string xml)
      (define port (open-output-string))
      (write-xml xml port)
      (get-output-string port))


   ; legacy support
   (define (xml-print xml) (print-xml-with display xml))
))

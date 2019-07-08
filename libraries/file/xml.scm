(define-library (file xml)
(import
   (otus lisp)
   (only (lang intern) string->symbol)
   (owl parse))

(export
   xml-parse-file

   xml-get-root-element
   xml-get-attributes
   xml-get-name
   xml-get-attribute
   xml-get-value

   xml-get-subtags
   xml-get-subtag

   xml-print
)

; parsed xml is:
;   #('xml #(attributes) body)
; where body:
;   #('tag #(attributes) '(children))
; where child:
;   tuple if tag or string if value

; legend: #[] is tuple
;         #() is ff (dictionary) with symbols as keys

(begin
   ; special symbols decoding
   ;(define @< (string->regex "r/&lt;/<"))


   ; utils:
   (define (between? lo x hi) ; fast version of (<= lo x hi)), where x is rune
      (and (or (less? lo x) (eq? lo x))
         (or (less? x hi) (eq? x hi))))

   ; xml standard: "any Unicode character, excluding the surrogate blocks, FFFE, and FFFF."
   (define (character? n) (or
      (between? #\a n #\z)
      (between? #\0 n #\9)
      (between? #\A n #\Z)))
      ;(between? #x7F n #xD7FF)
      ;(between? #xE000 n #xFFFD)
      ;(between? #x10000 n #x10FFFF)))

   (define (whitespace? x)
      (has? '(#\tab #\space #\newline #\return) x))

   (define skip-whitespaces
      (get-any-of (get-greedy* (get-rune-if whitespace?))))

   (define get-attribute
      (let-parses (
            (name (get-greedy+ (get-rune-if character?)))
            (= (get-imm #\=))
            (* (get-imm #\")) ;"
            (value (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\")))))) ;"
            (* (get-imm #\")) ;"
            (* skip-whitespaces))
         (cons
            (string->symbol (runes->string name))
            (runes->string value))))


   ; well, either the raw text or the set of subtags
   ; пока не будем смешивать вместе текст и теги - либо то либо другое
   (define (get-tag)
      (get-either
         (get-greedy*
            ; parse tag with attributes
            (let-parses (
                  (< (get-imm #\<))
                  (name (get-greedy+ (get-rune-if character?)))
                  (* skip-whitespaces)
                  (attributes (get-greedy* get-attribute))
                  (body (get-either
                     (get-word "/>" #null)
                     (let-parses (
                           (* (get-imm #\>))
                           (* skip-whitespaces)
                           (body (get-tag))
                           (* (get-word "</" #t)) ; </tag>
                           (* (get-word (runes->string name) #t))
                           (* (get-imm #\>)))
                        body)))
                  (* skip-whitespaces))
               (tuple
                  (string->symbol (runes->string name))
                  (list->ff attributes)
                  body)))
         (let-parses (
               (body (get-kleene* (get-rune-if (lambda (x) (not (eq? x #\<)))))))
            (if body (runes->string body)))))

   (define xml-parser
      (let-parses (
            (* (get-word "<?xml" #t)) ;<?xml version="1.0" encoding="UTF-8"?>
            (* skip-whitespaces)
            (attributes (get-greedy* get-attribute))
            (* (get-word "?>" #true))
            (* skip-whitespaces)
            (body (get-tag)))
         ['xml (list->ff attributes) body]))

   (define (xml-parse-file filename)
      (let ((file (open-input-file filename)))
         (if file
            (let ((o (parse xml-parser (port->byte-stream file) filename "xml parse error" #false)))
               (if o o
                  (close-port file)))))) ; no automatic port closing on error

   (define (xml-get-root-element xml)
      (car (ref xml 3)))

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

   ; printing the xml:
   (define (xml-print xml)
      ; header
      (display "<?xml")
      (ff-fold (lambda (? key value)
            (for-each display (list " " key "=\"" value "\"")))
         #f (xml-get-attributes xml))
      (display "?>\n")
      ; tags
      (let loop ((root (xml-get-root-element xml)) (indent ""))
         (display indent)
         (display "<")
         (display (xml-get-name root))
         (ff-fold (lambda (? key value)
               (for-each display (list " " key "=\"" value "\"")))
            #f (xml-get-attributes root))

         (if (null? (xml-get-value root))
            (display "/>\n")
            (let ((value (xml-get-value root)))
               (display ">")
               (if (string? value)
                  (display value)
                  (begin
                     (display "\n")
                     (for-each (lambda (child)
                           (loop child (string-append "   " indent)))
                        value)
                     (display indent)))
               (display "</")
               (display (xml-get-name root))
               (display ">\n")))))
))

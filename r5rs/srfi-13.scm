; String Libraries
(define-library (r5rs srfi-13)
; http://srfi.schemers.org/srfi-13/srfi-13.html

;; Abstract
;
;  R5RS Scheme has an impoverished set of string-processing utilities, which is a problem for
;  authors of portable code. This SRFI proposes a coherent and comprehensive set of string-processing
;  procedures; it is accompanied by a reference implementation of the spec. The reference
;  implementation is
;
;    portable
;    efficient
;    open source 
;
; -----
(export
   srfi-13
   
   ; Predicates
   string? string-null?
   ;string-every string-any

   ; Constructors
   make-string ;string string-tabulate

   ; List & string conversion
   ;string->list list->string
   ;reverse-list->string string-join

   ; Selection
   ;string-length
   ;string-ref
   ;string-copy
   ;substring/shared
   ;string-copy! 
   ;string-take string-take-right
   ;string-drop string-drop-right
   ;string-pad  string-pad-right 
   ;string-trim string-trim-right string-trim-both 

   ; Modification
   ;string-set! string-fill!

   ; Comparison
   ;string-compare string-compare-ci
   ;string<>     string=    string<    string>    string<=    string>=
   ;string-ci<>  string-ci= string-ci< string-ci> string-ci<= string-ci>=
   ;string-hash  string-hash-ci

   ; Prefixes & suffixes
   ;string-prefix-length    string-suffix-length
   ;string-prefix-length-ci string-suffix-length-ci
   ;
   ;string-prefix?    string-suffix?    
   ;string-prefix-ci? string-suffix-ci? 

   ; Searching
   ;string-index string-index-right
   ;string-skip  string-skip-right 
   ;string-count 
   ;string-contains string-contains-ci

   ; Alphabetic case mapping
   ;string-titlecase  string-upcase  string-downcase
   ;string-titlecase! string-upcase! string-downcase!

   ; Reverse & append 
   ;string-reverse string-reverse!
   ;string-append
   ;string-concatenate
   ;string-concatenate/shared string-append/shared
   ;string-concatenate-reverse string-concatenate-reverse/shared

   ; Fold, unfold & map
   ;string-map      string-map!
   ;string-fold     string-fold-right
   ;string-unfold   string-unfold-right
   ;string-for-each string-for-each-index

   ; Replicate & rotate
   ;xsubstring string-xcopy!

   ; Miscellaneous: insertion, parsing
   ;string-replace string-tokenize

   ; Filtering & deleting
   ;string-filter string-delete

   ; Low-level procedures
   ;string-parse-start+end
   ;string-parse-final-start+end
   ;let-string-start+end

   ;check-substring-spec
   ;substring-spec-ok?

   ;make-kmp-restart-vector kmp-step string-kmp-partial-search
   )
(import
   (scheme core))

(begin
   (setq srfi-13 #true)

   (define (string? s)
      (case (type s)
         (type-string #true)
         (type-string2 #true)
         (type-string-wide #true)
         (type-string-dispatch #true)
         (else #false)))

   (define (string-null? s)
      (and
         (string? s)
         (zero? (string-length s))))


   (define make-string (case-lambda
      ((len char) (runtime-error "Not implemented yet" 'make-string)) ;was: (list->string (repeat char n))), should be depndent of char, if it's rune - then type-string-wide otherwise type-string
      ((len) (vm:makeb type-string len))))

))

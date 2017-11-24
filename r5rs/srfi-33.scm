; The SRFI-33 integer bitwise-operation library

(define-library (r5rs srfi-33)
; http://srfi.schemers.org/srfi-33/srfi-33.txt

;; Abstract
;
; R5RS Scheme has no utilities for performing bitwise logical operations on
; integers or bitstrings, which is a problem for authors of portable code.  This
; SRFI proposes a coherent and comprehensive set of these functions; it is
; accompanied by a reference implementation of the spec in terms of a set of
; seven core operators. The reference implementation is
;  - portable
;  - efficient
;  - completely open, public-domain source
;
; The precise semantics of these operators is almost never an issue. A
; consistent, portable set of *names* and *parameter conventions*, however, is.
; Hence this SRFI.
;
; -----
(import
   (r5rs core)
   (owl math)
   (owl list))
(export
   srfi-33
   
   ;bitwise-not
   bitwise-and bitwise-ior
   bitwise-xor ;bitwise-eqv
   ;bitwise-nand bitwise-nor
   ;bitwise-andc1 bitwise-andc2
   ;bitwise-orc1 bitwise-orc2

   ;arithmetic-shift bit-count integer-length

   ;bitwise-merge
   ;bit-set? any-bits-set? all-bits-set?
   ;first-set-bit

   ;extract-bit-field test-bit-field? clear-bit-field
   ;replace-bit-field copy-bit-field
   )

(begin
   (setq srfi-33 #true)

   ; bitwise-not i -> exact-integer ; ambigous

   ; bitwise-and  i ... And
   (define bitwise-and
      (case-lambda
         ((a b) (band a b))
         ((a) a)
         ((a . bs) (fold band a bs))))

   ; bitwise-ior  i ... Or
   (define bitwise-ior
      (case-lambda
         ((a b) (bor a b))
         ((a) a)
         ((a . bs) (fold bor a bs))))

   ; bitwise-xor i ... Xor
   (define bitwise-xor
      (case-lambda
         ((a b) (bxor a b))
         ((a) a)
         ((a . bs) (fold bxor a bs))))

   ; bitwise-eqv ; ambigous

   ;bitwise-nand bitwise-nor
   ;bitwise-andc1 bitwise-andc2
   ;bitwise-orc1 bitwise-orc2

   ;arithmetic-shift bit-count integer-length

   ;bitwise-merge
   ;bit-set? any-bits-set? all-bits-set?
   ;first-set-bit

   ;extract-bit-field test-bit-field? clear-bit-field
   ;replace-bit-field copy-bit-field

))

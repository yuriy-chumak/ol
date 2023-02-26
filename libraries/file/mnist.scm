(define-library (file mnist)
   (import (otus lisp)
      (file parser))
   (export
      train-labels-parser
      train-images-parser)
(begin

   (define uint32
      (let-parse* (
            (a0 byte)
            (a1 byte)
            (a2 byte)
            (a3 byte))
         (+     a3
            (<< a2  8)
            (<< a1 16)
            (<< a0 24))))

   (define train-labels-parser
      (let-parse* (
            (magic (times 4 byte))
            (verify (equal? magic '(#x00 #x00 #x08 #x01)) 'not-a-mnist-labels-file)
            (number-of-labels uint32)
            ; (number-of-labels (epsilon 20)) ; debug purposes
            (labels (times number-of-labels byte)))
         {
            'magic magic
            'number-of-labels number-of-labels
            'labels labels
         }))

   (define train-images-parser
      (let-parse* (
            (magic (times 4 byte))
            (verify (equal? magic '(#x00 #x00 #x08 #x03)) 'not-a-mnist-images-file)
            (number-of-images uint32)
            (number-of-rows uint32)
            (number-of-columns uint32)
            ; (number-of-images (epsilon 1000)) ; debug purposes
            (images (times number-of-images (times (* number-of-rows number-of-columns) byte))))
         {
            'magic magic
            'number-of-images number-of-images
            'number-of-rows number-of-rows
            'number-of-columns number-of-columns
            'images images
         }))
))

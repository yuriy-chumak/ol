(import
   (data parse)
   (file json))

; todo: add full json test suite
(assert (parse json-parser (str-iter "{}"))
   ===> #empty)
(assert (parse json-parser (str-iter "[]"))
   ===> [])
(assert (parse json-parser (str-iter "{'something':[12,23,34],'old':{},'new':true}"))
   ===> { 'something [12 23 34]
            'old #empty
            'new #true })

;; (assert (car (try-parse json-parser (str-iter "123e7") #t))
;;    ===> 1230000000)
;; (assert (car (try-parse json-parser (str-iter "12e-7") #t))
;;    ===> 0.00000012)

(print "Ok.")

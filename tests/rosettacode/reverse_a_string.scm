; http://www.rosettacode.org/wiki/Reverse_a_string

(define (rev s)
   (runes->string (reverse (string->runes s))))

(print (rev "as⃝df̅"))

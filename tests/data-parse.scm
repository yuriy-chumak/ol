(import (data parse))

; byte
(print "parse: " (parse
   (let-parse* (
         (a byte)
         (b (byte))
         (c (byte 3))
         (d (byte (lambda (x) (> x 2))))
      )
      (list a b c d))
   '(1 2 3 4 5)))

; bytes
(print "parse: " (parse
   (let-parse* (
         (a (bytes "abc" 42))
         (b (bytes [#\d]))
         (c (bytes #u8(101 102)))
      )
      (list a b c))
   (string->stream "abcdef")))

; rune
(print "parse: " (parse
   (let-parse* (
         (a rune)
         (b (rune))
         (c (rune #\ι))
         (d (rune (lambda (x) (> x #\a))))
      )
      (list a b c d))
   (string->utf8stream "Γειάσου")))

; runes
(print "parse: " (parse
   (let-parse* (
         (a (runes "Γει" 42))
      )
      (list a))
   (string->utf8stream "Γειάσου")))

; eof
(print "parse: " (parse
   (let-parse* (
         (a (runes "Γειάσο" 42))
         (b eof)
      )
      (list a))
   (string->utf8stream "Γειάσου")))
(print "parse: " (parse
   (let-parse* (
         (a (runes "Γειάσου" 42))
         (b eof)
      )
      (list a))
   (string->utf8stream "Γειάσου")))

; this is a line command
(print "1")
;;;; this is a line comment
(print "2")
;; this is a ;; line comment
(print "3")
#| this is a block comment |#(print "4")
#| this
   is a
   block
   comment |#
(print "5")
#| this is a #| block comment |#
(print "6")
; this is a #| line |# comment
(print "7")
; this is a #| line #| comment
(print "8")
; this is a last file comment without newline

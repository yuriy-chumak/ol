
(define-record-type pare (kons x y)
   pare?
   (x kar)
   (y kdr))

(print
   (let ((x (kons 'lemon 'curry)))
      (if (pare? x)
         (kdr x)
         'chili)))

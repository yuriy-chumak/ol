#!/usr/bin/env -S ../../ffi
,load "definitions"

(print "
// reverse strings:")
(define reverse_string ((load-dynamic-library #f) type-string "reverse_string" type-string))
(define reverse_string_wide ((load-dynamic-library #f) type-string-wide "reverse_string_wide" type-string-wide))

(for-each (lambda (str)
      (define s (reverse_string str))
      (print "   reverse_string(" str "): " s " - "
         (if (string-eq? str (list->string (reverse (string->list s))))
            "ok. " "fail."))
      (define w (reverse_string_wide str))
      (print "   reverse_string_wide(" str "): " w " - "
         (if (string-eq? str (list->string (reverse (string->list w))))
            "ok. " "fail.")))
   '("hello"
     "привіт"
     "Совы (Strigiformes) суть релатівно чісленым рядом класы птахів обсягуюча веце як 200 видів."
     "ბუსნაირნი (ლათ. Strigiformes) — ფრინველთა რიგი. ფართოდაა გავრცელებული მსოფლიოში (ანტარქტიკული და ზოგიერთი ოკეანური კუნძულის გამოკლებით)."
     "フクロウ目（フクロウもく、梟目、学名 Strigiformes）は鳥類の1目である。"))

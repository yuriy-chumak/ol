; окошко с регистрами. обновляет, добывает, рисует
(fork-server 'config (lambda ()
(let loop ((ff #empty))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (tuple-case msg
      ((set option value)
         (loop (put ff option value)))
      ((get option)
         (mail sender (get ff option #f))
         (loop ff))
      (else
         (locate 1 23) (set-color RED)
         (print "Uknown config message " msg)
         (set-color GREY)
         (loop ff)))))))

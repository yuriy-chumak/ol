(async 'crasher (lambda ()
   (begin
      (wait-mail) ;; wait for a message before fault
      ((vm:alloc type-bytecode '(0))))))

(begin
   ;; link current thread to thread about to crash
   (link 'crasher)
   ;; trigger the crash
   (mail 'crasher 'itstime)
   ;; check that we get an error
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (print "'" sender " thread failed with '" (ref msg 1))))

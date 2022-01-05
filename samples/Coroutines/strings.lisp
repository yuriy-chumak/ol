#!/usr/bin/env ol

; all strings are collected in this coroutine in "strings" ff
(coroutine 'strings (lambda ()
(let this ((strings #empty))
(let* ((envelope (wait-mail))
       (sender msg envelope))
   (case msg
      (['set id string]
         (this (put strings id string)))
      (['get id]
         (mail sender (get strings id #false))
         (this strings))
      (else
         (print-to stderr "Unknown command " msg)
         (this strings)))))))

(define (strings:get id) (await (mail 'strings ['get id])))
(define (strings:change id new-string) (mail 'strings ['set id new-string]))


; strings keys
(define SAY_HELLO 0)
(define SAY_BYEBYE 1)
(define SOME_STRING 2)


; initial strings:
(strings:change SAY_HELLO "Say hello, dude!")
(strings:change SAY_BYEBYE "What? You'r leaving?")
(strings:change SOME_STRING "Something, something, something...")

; let's print
(print "ORIGINAL CONTENT:")
(print "hello: " (strings:get SAY_HELLO))
(print "bye: " (strings:get SAY_BYEBYE))
(print "etc: " (strings:get SOME_STRING))
(print "not existent: " (strings:get 42))
(print)

; let's change the strings
(strings:change SAY_HELLO "Say hello, bro!")
(strings:change SAY_BYEBYE (string-append (strings:get SAY_BYEBYE) " Yes, sorry."))

; print again
(print "CHANGED CONTENT:")
(print "hello: " (strings:get SAY_HELLO))
(print "bye: " (strings:get SAY_BYEBYE))
(print "etc: " (strings:get SOME_STRING))
(print "not existent: " (strings:get 42))
(print)


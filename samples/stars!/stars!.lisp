#!/usr/bin/env ol

(import (stars! rest))

(define answer (POST "http://127.0.0.1:4002/api/login" { 'login "user@1" 'password "123456"}))
(print answer)

(when answer
   (register (answer 'session))
   (print (GET "http://127.0.0.1:4002/api/races")))

(print "ok.")

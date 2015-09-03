#!/bin/ol
(import (lib http))

(http:run 8080 (lambda (request send)
   (send "<h1>hallo</h1>")
   (send (car request) "<br>" (cadr request))
   (send "<h4>-----</h4>")
))

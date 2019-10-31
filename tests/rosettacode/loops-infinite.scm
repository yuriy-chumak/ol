; http://www.rosettacode.org/wiki/Loops/Infinite

; actually can't test because this is infinite task, so just do this one time
(let loop ()
   (print "SPAM")
   #|(loop)|#)

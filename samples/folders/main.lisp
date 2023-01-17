#!/usr/bin/env ol

(import (lib c!))

(define current-directory (getcwd))
(print "current directory: " current-directory)

(let ((folder "ντοσιέ2"))
   (print "changing directory to unicode named " folder "... " (chdir folder)))
(print "current directory: " (getcwd))
(print "going back..." (chdir current-directory))
(newline)

(print "simple unordered files and folders iterator:")
(let ((dir (opendir ".")))
   (let loop ()
      (let ((info (readdir dir)))
         (when info
            (print "  " info)
            (loop))))
   (closedir dir))
(newline)

(print "another way to iterate unordered:")
(define selector (make-selector (lambda (filename)
   (print "  " filename)
   0))) ; 0 for "not interested in file/folder"
(scandir "." selector #f)
(newline)

(print "smarter way to iterate unordered:")
(define selector (make-selector (lambda (filename)
   1))) ; 1 for "interested to all of them"
(for-each (lambda (filename)
            (print "  " filename))
   (scandir "." selector #f))
(newline)

(print "let's collect only folders:")
(define selector (make-selector (lambda (filename)
   (if (folder? filename) 1 0))))
(for-each (lambda (filename)
            (print "  " filename))
   (scandir "." selector #f)) ; or jusy use `folder-selector` instread of custom folder select
(newline)

(print "only files larger than 1000 bytes:")
(define selector (make-selector (lambda (filename)
   (if (and (file? filename)
            (> (vector-ref (stat filename) 7) 1000)) 1 0))))
(for-each (lambda (filename)
            (print "  " filename))
   (scandir "." selector #f))
(newline)

(print "all in alphabetical order:")
(for-each (lambda (filename)
            (print "  " filename))
   (scandir "." #false alphasort))
(newline)

(print "all in custom order:")
(define comparer (make-comparer (lambda (nameA nameB)
   (let ((a (string-ref nameA 0))
         (b (string-ref nameB 0)))
      (cond ; let's reverse order:
         ((< a b) 1)
         ((= a b) 0)
         (else -1))))))
(for-each (lambda (filename)
            (print "  " filename))
   (scandir "." #false comparer))
(newline)

(print "that's all, folks. to be continued...")

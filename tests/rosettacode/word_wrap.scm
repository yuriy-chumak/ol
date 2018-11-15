; http://www.rosettacode.org/wiki/Word_wrap

(define (get-one-word start)
(let loop ((chars #null) (end start))
   (let ((char (car end)))
      (if (has? (list #\space #\newline) char)
         (values (reverse chars) (force (cdr end)))
         (loop (cons char chars) (force (cdr end)))))))

(define (get-all-words string)
(let loop ((words #null) (start (str-iter string)))
   (let* ((word next (get-one-word start)))
      (if (null? next)
         (reverse words)
         (loop (cons (runes->string word) words) next)))))

(define (get-one-line words n)
(let loop ((line #null) (words words) (i 0))
   (let*((word (car words))
         (len (string-length word)))
      (if (null? (cdr words))
         (values (reverse (cons word line)) #null)
         (if (> (+ i len) n 1)
            (values (reverse line) words)
            (loop (cons word line) (cdr words) (+ i len 1)))))))

(define (get-all-lines words n)
(let loop ((lines #null) (words words))
   (let* ((line words (get-one-line words n)))
      (if (null? words)
         (reverse (cons line lines))
         (loop (cons line lines) words)))))

(define (hyphenation width string)
(let*((words (get-all-words string))
      (lines (get-all-lines words width)))
   lines))

; testing:
(print "{0---------1+++++++++2---------3+++++++++4---------5+++++++++6---------7+++++++++}")
(for-each print (hyphenation 80 "In olden times when wishing still helped one, there lived a king whose daughters were all beautiful, but the youngest was so beautiful that the sun itself, which has seen so much, was astonished whenever it shone in her face.  Close by the king's castle lay a great dark forest, and under an old lime-tree in the forest was a well, and when the day was very warm, the king's child went out into the forest and sat down by the side of the cool fountain, and when she was bored she took a golden ball, and threw it up on high and caught it, and this ball was her favorite plaything. "))

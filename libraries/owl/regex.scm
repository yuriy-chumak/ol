;;
;; POSIX regular expressions
;;

;; this library implements a mostly complete POSIX-compatible
;; regular expressions. at the moment lib-regex tries to just
;; get all the features right. *lots* of non-constant-factor
;; optimizations are missing.

;;; spec: http://pubs.opengroup.org/onlinepubs/007908799/xbd/re.html
;;; syntax ref of portable scheme regexps (Dorai Sitaram): http://evalwhen.com/pregexp/index-Z-H-3.html#node_sec_3

;; todo: c/regex/ : str → head|F tail, c/regex/g : str → (part ...)
;; todo: it would be nice to be able to call an arbitrary function on the matched area easily (for example to be used in radamsa)
;; fixme: some variable and function names are now misleading
;; todo: later return an ast instead of the function from parser to allow some postprocessing
;; todo: add regexp flags (as implicit cloisters? (starting with case [i]nsensitive)) as postprocessing steps for the ast
;; todo: merge runs of known letters to a node using match-list like for submatches
;; todo: merge mergeable repetitions of equal asts
;; todo: lookbehind is missing
;; todo: add state to parsing to capture flags, greediness etc
;; todo: s/<from>/<to>/r = repeat at the match position while match
;; todo: s/<from>/<to>/R = repeat from beginning while match
;; todo: check handling of non-fixnum code points in the regex string itself (likely fails but no time to test atm)


(define-library (owl regex)
   (export
      get-sexp-regex
      string->regex
      string->replace-regex
      string->complete-match-regex
      )

   (import
      (scheme core)
      (scheme list)
      (srfi 1)
      (owl parse)
      (owl ff)
      (owl list)
      (owl math)
      (owl string)
      (otus blobs)
      (owl list-extra)
      (owl iff))

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      ;;;
      ;;; Matching functions
      ;;;

      ;; the regexp is represented by a function which does stream matching

      ;; "", match nothing with great success
      (define (epsilon ls buff ms cont)
         (cont ls buff ms))

      ;; $, match input being null
      (define (fini ls buff ms cont)
         (cond
            ((null? ls) (cont ls buff ms))
            ((pair? ls) #false)
            (else (fini (ls) buff ms cont))))

      ;; ., match anyting (note, POSIX requires this to not match some value(s?))
      (define (dot ls buff ms cont)
         (cond
            ((null? ls) #false)
            ((pair? ls)
               (cont (cdr ls) (cons (car ls) buff) ms))
            (else (dot (ls) buff ms cont))))

      ;; <char>
      (define (imm cp) ;; match match a specific (fixnum) code point
         (define (accept ls buff ms cont)
            (cond
               ((null? ls) #false)
               ((pair? ls)
                  (if (eq? (car ls) cp)
                     (cont (cdr ls) (cons cp buff) ms)
                     #false))
               (else
                  (accept (ls) buff ms cont))))
         (if (eq? (type cp) type-enum+)
            accept
            (runtime-error "match string cannot yet contain a " cp)))

      (define (pred fn) ;; match match a specific (fixnum) code point
         (define (accept ls buff ms cont)
            (cond
               ((null? ls) #false)
               ((pair? ls)
                  (if (fn (car ls))
                     (cont (cdr ls) (cons (car ls) buff) ms)
                     #false))
               (else
                  (accept (ls) buff ms cont))))
         accept)

      ;; [ab..n], store set in a ff (range 0-65535)
      (define (accept-ff ff)
         (λ (ls buff ms cont)
            (cond
               ((null? ls) #false)
               ((pair? ls)
                  (if (get ff (car ls) #false)
                     (cont (cdr ls) (cons (car ls) buff) ms)
                     #false))
               (else
                  ((accept-ff ff) (ls) buff ms cont)))))

      ; [ab..λ→..n], store in an integer ff (sparse number store)
      (define (accept-iff iff)
         (λ (ls buff ms cont)
            (cond
               ((null? ls) #false)
               ((pair? ls)
                  (if (iget iff (car ls) #false)
                     (cont (cdr ls) (cons (car ls) buff) ms)
                     #false))
               (else
                  ((accept-iff iff) (ls) buff ms cont)))))

      ; [ab..λ→..n], store in an integer ff (sparse number store)
      (define (reject-iff iff)
         (λ (ls buff ms cont)
            (cond
               ((null? ls) #false)
               ((pair? ls)
                  (if (iget iff (car ls) #false)
                     #false
                     (cont (cdr ls) (cons (car ls) buff) ms)))
               (else
                  ((reject-iff iff) (ls) buff ms cont)))))

      ;; (n ..) → ff of n → #true | #false, if one is outside of fixnum range
      (define (make-ff cs)
         (call/cc
            (λ (ret)
               (fold (λ (ff n)
                        (if (eq? (type n) type-enum+)
                           (put ff n #true)
                        else
                           (ret #false)))
                  #empty cs)))) ;; code point outside of fixnum range

      ;; todo: large ranges would be more efficiently matched with something like interval trees
      (define (make-char-class complement? cs)
         (cond
            ((null? cs) ;; should not come out of parser
               (runtime-error "empty char class: " cs))
            (complement?
               ;; always use an iff for now in [^...]
               (reject-iff
                  (fold (λ (iff val)
                           (iput iff val #true))
                     #empty cs)))
            ((null? (cdr cs))
               (imm (car cs)))
            ((make-ff cs) =>
               (λ (ff) (accept-ff ff)))
            (else
               (accept-iff
                  (fold (λ (iff val)
                           (iput iff val #true))
                     #empty cs)))))

      ;; <ra>|<rb>
      (define (rex-or ra rb)
         (λ (ls buff ms cont)
            (or (ra ls buff ms cont)
                (rb ls buff ms cont))))

      ;; <ra><rb>
      (define (rex-and ra rb)
         (λ (ls buff ms cont)
            (ra ls buff ms
               (λ (ls buff ms)
                  (rb ls buff ms cont)))))

      ;; note that all repetitions could be implemented with a generic repeater. here
      ;; we splice them to several smaller ones, mainly because small parsing functions
      ;; are prettier, and all mathmaticians would like to do with just star anyway,
      ;; so it will be given an important role.

      ;;; greedy base quantifiers

      ;; <rx>*
      (define (star rx)
         (λ (ls buff ms cont)
            (let loop ((ls ls) (buff buff) (last-ms ms))
               (or
                  (rx ls buff ms
                     (λ (ls buff next-ms)
                        (loop ls buff next-ms)))
                  (cont ls buff last-ms)))))

      ;; <rx>+
      (define (plus rx) (rex-and rx (star rx)))

      ;; <rx>?
      (define (quest rx) (rex-or rx epsilon))

      ;;; non-greedy (altruistic?) quantifiers

      (define (alt-star rx)
         (define (collect ls buff ms cont)
            (or (cont ls buff ms)
               (rx ls buff ms
                  (λ (ls buff ms) (collect ls buff ms cont)))))
         collect)

      ;; <rx>+?
      (define (alt-plus rx) (rex-and rx (alt-star rx)))

      ;; <rx>??
      (define (alt-quest rx) (rex-or epsilon rx))

      ;;; repetitions

      ;; todo: check if non-greedy repetitions (syntax could be foo{}? or foo?{}) defined?

      ;; <rx>{n}
      (define (exactly n rx)
         (cond
            ((eq? n 0) epsilon) ;; fixme: these could be handled by postprocessing later
            ((eq? n 1) rx)
            (else
               (λ (ls buff ms cont)
                  (define (want ls buff ms n)
                     (if (eq? n 0)
                        (cont ls buff ms)
                        (rx ls buff ms (λ (ls buff ms) (want ls buff ms (- n 1))))))
                  (want ls buff ms n)))))

      ;; <rx>{,n}
      (define (at-most n rx)
         (cond
            ((eq? n 0) epsilon) ; R{,0} = ""
            ((eq? n 1) (rex-or rx epsilon)) ; R{,1} = "" | R
            (else
               (λ (ls buff ms cont)
                  (define (maybe ls buff ms n)
                     (if (eq? n 0)
                        (cont ls buff ms)
                        (or
                           (rx ls buff ms (λ (ls buff ms) (maybe ls buff ms (- n 1))))
                           (cont ls buff ms))))
                  (maybe ls buff ms n)))))

      ;; r_ar_b..r_n
      (define-syntax cat
         (syntax-rules ()
            ((cat a) a)
            ((cat a b) (rex-and a b))
            ((cat a b ...) (cat a (cat b ...)))))

      ;; <rx>{n,}
      (define (at-least n rx)
         (cond
            ((eq? n 0) (star rx))
            ((eq? n 1) (plus rx))
            (else (cat (exactly n rx) (star rx)))))

      ;; ra|rb|..|rn
      (define-syntax union
         (syntax-rules ()
            ((union a) a)
            ((union a b) (rex-or a b))
            ((union a b ...) (union a (union b ...)))))

      ;; copy then range head .. old (conses) to out in reverse order
      (define (add-range head old out)
         (cond
            ((null? head) out)
            ((eq? head old) out)
            (else
               (add-range (cdr head) old (cons (car head) out)))))

      ;; find node = (id . start-pos) in ms, and update the cdr to hold the range between buff and the start-pos
      ;; (note, could also leave pointer pair to start and end to make range handling O(1) instead of O(n))
      (define (update-node ms node buff)
         (if (eq? node (car ms))
            (cons
               (cons (car node) ;; id
                  (add-range buff (cdr node) null))
               (cdr ms))
            (cons (car ms) (update-node (cdr ms) node buff))))

      ;; (<rx>)
      (define (chunk rex)
         (λ (ls buff ms cont)
            (lets
               ((id (+ 1 (caar ms)))   ;; my submatch id
                (node (cons id buff))) ;; leave marker with pointer to current matched position (start of range)
               (rex ls buff (cons node ms)
                  (λ (ls buffp ms)
                     (cont ls buffp    ;; update node with current matched position (end of range), or the range itself
                        (update-node ms node buffp)))))))

      ;; todo: are the lookahead and lookbehind allowed to capture submatches?

      (define (lookahead rex)
         (λ (ls buff ms cont)
            (rex ls buff ms
               (λ (lsp buffp msp) (cont ls buff ms))))) ;; there she blows

      (define (lookahead-not rex)
         (λ (ls buff ms cont)
            (if (rex ls buff ms (λ (a b c) #true))
               #false
               (cont ls buff ms))))

      ;; todo: lookback requires storing the unmatched part, which is not there yet when matcher starts from the middle of data
      ;; todo: the usual case is probaly (?<string), which should be handled separately as it is trivial
      ;; todo: not sure if the right thing (tm) would be to O(n) apply the lookbehind to all positions on the left (it being at least effectively undecidable how many characters it needs) or mirror the automata and run it once from the starting position
      ;; todo: mirroring the automata affects things like greediness of operators. how are these defined in the spec(s)?

      ; O(n) * rex (!)
      (define (lookback rex)
         (λ (ls buff ms cont)
            (let loop ((rev buff) (try null))
               (cond
                  ((rex try rev ms (λ (ls buff ms) (null? ls)))
                     (cont ls buff ms))
                  ((null? rev) #false)
                  (else
                     (lets ((char rev rev))
                        (loop rev (cons char try))))))))

      ; O(n) * rex (!)
      (define (lookback-not rex)
         (λ (ls buff ms cont)
            (let loop ((rev buff) (try null))
               (cond
                  ((rex try rev ms (λ (ls buff ms) #true))
                     #false)
                  ((null? rev)
                     (cont ls buff ms))
                  (else
                     (lets ((char rev rev))
                        (loop rev (cons char try))))))))

      ;; ((a . bs) ...) n → bs | #false
      (define (ranges-ref ls n)
         (if (null? ls)
            #false
            (let ((node (car ls)))
               (if (eq? n (car node))
                  (cdr node)
                  (ranges-ref (cdr ls) n)))))

      ;; match each node of already matched data (val) against input (ls) and write to output
      (define (match-list ls val buff)
         (cond
            ((null? val) (values ls buff))
            ((null? ls) (values #false #false))
            ((pair? ls)
               (lets ((next val val))
                  (cond
                     ((eq? next (car ls)) ;; this elem matched
                        (match-list (cdr ls) val (cons next buff)))
                     ((eq? (type next) type-int+) ;; try = for high code points
                        (if (= next (car ls))
                           (match-list (cdr ls) val (cons next buff))
                           (values #false #false)))
                     (else ;; no match
                        (values #false #false)))))
            (else
               (match-list (ls) val buff))))

      (define (matched n)
         (λ (ls buff ms cont)
            (let ((val (ranges-ref (reverse ms) n)))
               (if val
                  (lets ((ls buff (match-list ls val buff)))
                     (if buff (cont ls buff ms) #false))
                  #false))))

      ;;;
      ;;; Running the regexen
      ;;;

      (define start-node
         (cons 0 null))

      ;; ranges = ((nth-range . start-node) ...)
      (define blank-ranges
         (list start-node))


      (define (null-ll? ll)
         (cond
            ((null? ll) #true)
            ((pair? ll) #false)
            (else (null-ll? (ll)))))

      ;; rex str → bool (matches some prefix of ll)
      (define (rex-match-prefix? rex ll)
         (rex ll null blank-ranges
            (λ (ls buff ms) #true)))

      ;; rex ll → #false | #(ls buff ms), for replacing
      (define (rex-match-prefix rex ll)
         (rex ll null blank-ranges
            (λ (ls buff ms)
               [ls buff ms])))

      ;; rex str → bool (if matches anywhere)
      (define (rex-match-anywhere? rex ll)
         (cond
            ((null? ll)
               (rex-match-prefix? rex ll))
            ((pair? ll)
               (if (rex-match-prefix? rex ll)
                  #true
                  (rex-match-anywhere? rex (cdr ll))))
            (else (rex-match-anywhere? rex (ll)))))

      (define (iter x)
         (cond
            ((pair? x) x)
            ((null? x) x)
            ((string? x) (str-iter x))
            ((blob? x) (blob-iter x))
            (else (runtime-error "how do i iterate " x))))

      ;; todo: now that the matchers are constructed here, the terminals /[^]...[$]/ could be handled externally!
      (define (make-matcher rex start?)
         (if start?
            (λ (target)
               (rex-match-prefix? rex (iter target)))
            (λ (target)
               (rex-match-anywhere? rex (iter target)))))

      ;; another half-sensible but at the moment useful thing would be (m/<regex>/ iterable) -> #false | (head . tail)
      (define (rex-copy-match-prefix? rex ll)
         (rex ll null blank-ranges
            (λ (ls buff ms)
               [ls buff ms])))

      ;; rex str → bool (if matches anywhere)
      (define (rex-copy-match-anywhere? rex ll)
         (cond
            ((null? ll)
               (rex-copy-match-prefix? rex ll))
            ((pair? ll)
               (or (rex-copy-match-prefix? rex ll)
                   (rex-copy-match-anywhere? rex (cdr ll))))
            (else
               (rex-copy-match-anywhere? rex (ll)))))

      (define (make-copy-matcher rex start?)
         (if start?
            (λ (target)
               (let ((res (rex-copy-match-prefix? rex (iter target))))
                  (if res (reverse (ref res 2)))))
            (λ (target)
               (let ((res (rex-copy-match-anywhere? rex (iter target))))
                  (if res (reverse (ref res 2)))))))

      (define (flush out)
         (if (null? out)
            null
            (list (runes->string (reverse out)))))

      (define (rex-cut rex ll start? out)
         (cond
            ((null? ll)
               (flush out))
            ((pair? ll)
               (let ((res (rex-match-prefix rex ll)))
                  (cond
                     (res
                        (lets ((ls buff ms res))
                           ;; buff = reverse matched range
                           (cons (runes->string (reverse out)) ;; non-matched up to now
                              (if start?
                                 (list ls)
                                 (rex-cut rex ls #false null)))))
                     (start?
                        (list ll))
                     (else
                        (rex-cut rex (cdr ll) start? (cons (car ll) out))))))
            (else
               (rex-cut rex (ll) start? out))))


      ;; regex that cuts stuff to pieces at matches
      (define (make-cutter rex start?)
         (λ (target)
            (rex-cut rex (iter target) start?
               ; global? retain
               null)))


      ;;;
      ;;; Replacing
      ;;;

      ;; replacer is a function from code point streams to code point streams
      ;; it may either itself find all the matches and perform substitutions,
      ;; handle the first one, or something completely different.

      ;; fixme: trailing \ is handled wrong
      ;; copy and fill in submatches
      (define (replace rep ms tl)
         (foldr
            (λ (char tl)
               (cond
                  ((eq? char #\\)
                     (if (null? tl)
                        (cons char tl)
                        (let ((op (car tl)))
                           (cond
                              ((and (less? 47 op) (less? op 58)) ;; fixme: silly
                                 (let ((submatch (ranges-ref ms (- op 48))))
                                    (if submatch
                                       (append submatch (cdr tl))
                                       tl))) ;; todo: add a fail cont and complain about bad backreference
                              ((eq? op #\\) tl)
                              (else ;; todo: warn about unhandeld quote
                                 tl)))))
                  (else
                     (cons char tl))))
            tl rep))

      ;; todo: could be made lazy to allow string/vector operations without unwinding the whole thing to a list while operating on it
      ;; todo: merge with replace-first

      (define (rex-replace ll rex rep start? all?)
         (let loop ((ll ll))
            (cond
               ((null? ll) null)
               ((pair? ll)
                  (let ((match (rex-match-prefix rex ll)))
                     (cond
                        (match
                           (lets
                              ((ls buff ms match)
                               (ms (update-node ms start-node buff))) ;; save whole match to \0
                              (cond
                                 (start?
                                    (replace rep ms ls))           ;; do not proceed if ^ required
                                 (all?
                                    (replace rep ms (loop ls)))    ;; look for others
                                 (else
                                    (replace rep ms ls)))))        ;; replace only first unless /g
                        (start?
                           ;; stop if no match at beginning and ^
                           ll)
                        (else
                           ;; proceed to content
                           (cons (car ll) (loop (cdr ll)))))))
               (else ;; force
                  (loop (ll))))))

      ;; todo: use the flags of the regexp to choose what kind of replacer to make. now implicit /g
      (define (make-replacer rex rep all? start?)
         (λ (target)
            (cond
               ((string? target)
                  (runes->string (rex-replace (str-iter target) rex rep start? all?)))
               (else
                  (rex-replace (iter target) rex rep start? all?)))))



      ;;;
      ;;; Regexp string parsing
      ;;;

      (define get-dot  ;; .
         (let-parses ((foo (get-imm #\.))) dot))

      (define get-fini ;; $
         (let-parses ((foo (get-imm #\$))) fini))

      ;; maybe get a ?
      (define get-altp
         (get-either (get-imm #\?) (get-epsilon #false)))

      ;; → (rex → rex')
      (define get-star
         (let-parses
            ((skip (get-imm 42))
             (altp get-altp))
            (if altp alt-star star)))

      ;; a+ = aa*
      (define get-plus
         (let-parses
            ((skip (get-imm 43))
             (altp get-altp))
            (if altp alt-plus plus)))

      ;; a? = a{0,1} = (a|"")
      (define get-quest
         (let-parses
            ((skip (get-imm #\?))
             (altp get-altp))
            (if altp alt-quest quest)))

      (define (i x) x)

      (define special-chars '(#\( #\) #\| #\. #\/)) ;; kinda ugly. the parser should check for these first

      (define (imm-val imm val)
         (let-parses ((d (get-imm imm))) val))

      (define digit?     (λ (b) (and (less? 47 b) (less? b 58)))) ;; 0-9
      (define alpha?     (λ (b) (and (less? 96 b) (less? b 123))));; a-z
      (define big-alpha? (λ (b) (and (less? 64 b) (less? b 91)))) ;; A-Z

      (define alnum? (λ (b) (or (alpha? b) (big-alpha? b) (digit? b))))
      (define word? (λ (b) (or (eq? b #\_) (alnum? b))))            ;vtab formfeed
      (define space? (λ (b) (has? '(#\space #\tab #\return #\newline #x0b #x0c) b)))

      ;; shared automata parts corresponding to predefined character classes
      (define accept-digit (pred digit?))
      (define accept-dot (imm #\.))
      (define accept-nondigit (pred (λ (b) (not (digit? b)))))
      (define accept-alnum (pred alnum?))
      (define accept-word (pred word?))
      (define accept-nonword (pred (λ (b) (not (word? b)))))
      (define accept-space (pred space?))
      (define accept-nonspace (pred (λ (b) (not (space? b)))))

      ;; \<x>
      (define get-quoted-char
         (let-parses
            ((skip (get-imm #\\))
             (val
               (get-any-of
                  (imm-val #\d accept-digit)       ;; \d = [0-9]
                  (imm-val #\D accept-nondigit)    ;; \D = [^0-9]
                  (imm-val #\. accept-dot)         ;; \. = .
                  (imm-val #\w accept-word)        ;; \w = [_0-9a-zA-Z]
                  (imm-val #\n (imm #\newline))    ;; \n = newline
                  (imm-val #\r (imm #\return))     ;; \r = carriage return
                  (imm-val #\t (imm #\tab))        ;; \t = tab
                  (imm-val #\W accept-nonword)     ;; \W = [^_0-9a-zA-Z]
                  (imm-val #\s accept-space)       ;; \s = [ \t\r\n\v\f]
                  (imm-val #\S accept-nonspace)    ;; \S = [ \t\r\n\v\f]
                  (imm-val #\/ (imm #\/)))))       ;; \/ = /
            val))

      ;; strings are already sequences of unicode code points, so no need to decode here
      ;; accept any non-special char
      (define get-plain-char
         (let-parses
            ((val get-rune) ;; really get-code-point since the input is already decoded
             (verify (not (has? special-chars val)) "bad special char"))
            (imm val)))

      (define (quoted-imm val)
         (let-parses
            ((quote (get-imm #\\))
             (val (get-imm val)))
            val))

      (define get-reference ;; \0-\9
         (let-parses
            ((skip (get-imm #\\))
             (d get-byte)
             (verify (digit? d) "bad digit"))
            (matched (- d #\0))))

      (define get-digit
         (let-parses
            ((b get-byte)
             (verify (digit? b) #false))
            (- b #\0)))

      (define get-number
         (let-parses
            ((digits (get-greedy+ get-digit)))
            (fold (λ (n d) (+ (* n 10) d)) 0 digits)))

      ;; \<suff> → code-point (not acceptor as in get-quoted-char)

      ;; byte → #false | hex-value
      (define digit-values (alist->ff
         (foldr append #null
            (list
               (map (lambda (d i) (cons d i)) (iota 10 #\0) (iota 10 0))  ;; 0-9
               (map (lambda (d i) (cons d i)) (iota  6 #\A) (iota 6 10))  ;; A-F
               (map (lambda (d i) (cons d i)) (iota  6 #\a) (iota 6 10))  ;; a-f
               ))))

      (define (char->hex b)
         (digit-values b #f))

      (define get-hex
         (let-parses
            ((b get-byte)
             (verify (char->hex b) #false))
            (char->hex b)))

      (define get-8bit
         (let-parses ((hi get-hex) (lo get-hex)) (bor (<< hi 4) lo)))

      (define get-16bit
         (let-parses ((hi get-8bit) (lo get-8bit)) (bor (<< hi 8) lo)))

      (define get-32bit
         (let-parses ((hi get-16bit) (lo get-16bit)) (bor (<< hi 16) lo)))

      ;; todo: what is the quotation used for 32-bit \xhhhhhhhh?
      (define parse-quoted-char-body
         (get-any-of
            ;; the usual quotations
            (imm-val #\a  7)  ;; \a = 7
            (imm-val #\b  8)  ;; \b = 8
            (imm-val #\t  9)  ;; \t = 9
            (imm-val #\n 10)  ;; \n = 10
            (imm-val #\v 11)  ;; \v = 11
            (imm-val #\f 12)  ;; \f = 12
            (imm-val #\r 13)  ;; \r = 13
            (get-imm #\[)     ;; \[ = [
            (get-imm #\\)     ;; \\ = \
            (get-imm #\])     ;; \] = ]
            (get-imm #\^)     ;; \^ = ^
            (let-parses ((skip (get-imm #\x)) (char get-8bit)) char)    ;; \xhh
            (let-parses ((skip (get-imm #\u)) (char get-16bit)) char))) ;; \uhhhh

      (define parse-quoted-char
         (let-parses
            ((skip (get-imm #\\))
             (val parse-quoted-char-body))
            val))

      ;; todo: should probably also disallow \ to avoid accidental broken quotations
      ;; a quoted character or anything other than ]
      (define parse-char-class-char
         (get-either
            parse-quoted-char
            (let-parses
               ((char get-rune)
                (verify (not (eq? char #\])) #false))
               char)))

      ;; get a range or a single letter of a char class (treat single letter as ranges of length 1)
      (define char-class-elem
         (let-parses
            ((b parse-char-class-char)
             (c
               (get-either
                  (let-parses
                     ((skip (get-imm #\-))
                      (c parse-char-class-char)
                      (verify (<= b c) "bad range"))
                     c)
                  (get-epsilon b))))
            (lrange b 1 (+ c 1))))

      (define get-maybe-caret
         (get-either
            (get-imm #\^)  ;; hack, returned 94 on match is also true
            (get-epsilon #false)))

      (define get-char-class
         (let-parses
            ((open (get-imm #\[))
             (comp? get-maybe-caret)
             (charss (get-greedy+ char-class-elem)) ;; todo: [] might also be useful
             (close (get-imm #\])))
            (make-char-class comp?
               (foldr append null charss))))

      ;; n m|inf → (R → R{n,m})
      (define (make-repeater n m)
         (cond
            ((eq? m 'inf)
               (λ (rx) (at-least n rx)))
            ((= n m)
               (if (eq? n 0)
                  epsilon
                  (λ (rx) (exactly n rx))))
            ((< n m) ;; <= enforced when parsing but ok to double-check as this is only done once
               (if (eq? n 0)
                  (λ (rx) (at-most m rx))
                  (λ (rx) (rex-and (exactly n rx) (at-most (- m n) rx)))))
            (else
               (runtime-error "make-repeater: bad range: " (list n 'to m)))))

      (define get-range
         (let-parses
            ((skip (get-imm #\{))   ; <{>...}
             (start
               (get-either get-number (get-epsilon 0))) ; <{[n]>...}
             (end
               (get-either
                  (let-parses
                     ((skip (get-imm #\,)) ; <{[n],>
                      (val (get-either get-number (get-epsilon 'inf)))) ; <{[n],[n]>...}
                     val)
               (get-epsilon start))) ; <{[n]>..}
             (verify (or (eq? end 'inf) (<= start end)) "bad range") ;; → can print error message with exact location in input if failes
             (skip (get-imm #\}))) ; <{...}>
            (make-repeater start end)))

      ;; parse a sequence of regexp terms with implicit catenation
      (define (get-catn get-regex)
         (let-parses
            ((regex ;; parse a single regexp thing
               (get-any-of
                  get-dot
                  get-fini
                  ;; todo: merge the parenthetical ones later
                  (let-parses ;; (?:...), non-capturing submatch
                     ((open (get-imm #\())
                      (skip (get-imm #\?))
                      (skip (get-imm #\:)) ;; read ?: explicitly while testing. there are really many more alternatives.
                      (rex (get-regex))
                      (close (get-imm #\))))
                     rex)
                  (let-parses ;; (?=<regex>) → match if regex also would match
                     ((open (get-imm #\())
                      (skip (get-imm #\?))
                      (skip (get-imm #\=))
                      (rex (get-regex))
                      (close (get-imm #\))))
                     (lookahead rex))
                  (let-parses ;; (?!<regex>) → match if regex would not match
                     ((open (get-imm #\())
                      (skip (get-imm #\?))
                      (skip (get-imm #\!))
                      (rex (get-regex))
                      (close (get-imm #\))))
                     (lookahead-not rex))
                  (let-parses ;; (?<=<regex>) → match if regex matches on the left of current position
                     ((open (get-imm #\()) ; #\(
                      (skip (get-imm #\?))
                      (skip (get-imm #\<)) ; #\<
                      (skip (get-imm #\=)) ; #\=
                      (rex (get-regex))
                      (close (get-imm #\))))
                     (lookback rex))
                  (let-parses ;; (?<!<regex>) → match if regex matches on the left of current position, not
                     ((open (get-imm #\())
                      (skip (get-imm #\?))
                      (skip (get-imm #\<))
                      (skip (get-imm #\!))
                      (rex (get-regex))
                      (close (get-imm #\))))
                     (lookback-not rex))
                  (let-parses ;; (...) → match and store
                     ((open (get-imm #\())
                      (rex (get-regex))
                      (close (get-imm #\))))
                     (chunk rex))
                  get-char-class
                  get-reference
                  get-quoted-char
                  get-plain-char))
             (repetition
               (get-any-of
                  get-star
                  get-plus
                  get-quest
                  get-range
                  (get-epsilon i)))
             (tail
               (get-any-of
                  (let-parses ;; join tail of exp with implicit catenation
                     ((tl (get-catn get-regex)))
                     (λ (head) (rex-and head tl)))
                  (get-epsilon i)))) ;; nothing
           (tail (repetition regex))))

      ;; get a sequence of regexps with zero or more | in between and merge them
      (define (get-regex)
         (let-parses
            ((hd (get-catn get-regex))
             (tl (get-greedy* (let-parses ((skip (get-imm 124)) (rex (get-catn get-regex))) rex))))
            (fold rex-or hd tl)))

      (define get-matcher-regex
         (let-parses
            ((skip (get-imm #\m)) ;; [m]atch
             (skip (get-imm #\/))  ;; opening /
             (start? (get-either (get-imm #\^) (get-epsilon #false))) ;; maybe get leading ^ (special)
             (rex (get-regex))
             (skip (get-imm #\/))) ;; closing /
            (make-matcher rex start?)))

      ;; a parser for terms like ab{1,3}a* with implicit ^ and $
      (define get-body-regex
         (let-parses ((rex (get-regex)))
            (make-matcher (rex-and rex fini) #true)))

      (define get-copy-matcher-regex
         (let-parses
            ((skip (get-imm #\g)) ;; [g]rab
             (skip (get-imm #\/))  ;; opening /
             (start? (get-either (get-imm #\^) (get-epsilon #false))) ;; maybe get leading ^ (special)
             (rex (get-regex))
             (skip (get-imm #\/))) ;; closing /
            (make-copy-matcher rex start?)))

      (define get-cutter-regex
         (let-parses
            ((skip (get-imm #\c))  ;; [c]ut
             (skip (get-imm #\/))  ;; opening /
             (start? (get-either (get-imm #\^) (get-epsilon #false))) ;; maybe get leading ^ (special)
             (rex (get-regex))
             (skip (get-imm #\/)) ;; closing /
            ;(flags get-cut-flags) ;; [r]emove (default), keep as [p]refix, keep as [s]uffix
                                   ;; (c/X/[r] "fooXbarXbaz") → '("foo" "bar" "baz")
                                   ;; (c/X/p   "fooXbarXbaz") → '("foo" "Xbar" "Xbaz")
                                   ;; (c/X/s    "fooXbarXbaz") → '("fooX" "barX" "baz")
            )
           (make-cutter rex start?)))

      (define get-replace-char
         (get-either
            (let-parses ;; quoted
               ((skip (get-imm #\\))
                (char (get-imm #\b)))
               char)
            (let-parses ;; something other than /
               ((char get-rune)
                (verify (not (eq? char #\/)) #false))
               char)))

      (define get-maybe-g
         (get-either
            (get-imm #\g)
            (get-epsilon #false)))

      ;; for testing, s/<regex>/<str>/[g]
      (define get-replace-regex
         (let-parses (
               (skip (get-imm #\s))  ;; opening s
               (skip (get-imm #\/))  ;; opening /
               (start? (get-either (get-imm #\^) (get-epsilon #false))) ;; maybe get leading ^ (special)
               (rex (get-regex))
               (skip (get-imm #\/))  ;; delimiting /
               (rep (get-greedy* get-replace-char))
               (skip (get-imm #\/)) ;; closing /
               (all? get-maybe-g)) ;; fixme: add other search/replace match than g
            (make-replacer rex rep all? start?)))

      ; global regex parser
      (define get-sexp-regex
         (let-parse*(
               (from (lambda (l r p ok) (ok l r p l)))
               (regex (get-any-of
                  get-replace-regex        ;; s/.../.../
                  get-matcher-regex        ;; m/.../
                  get-cutter-regex         ;; c/.../
                  get-copy-matcher-regex)) ;; g/<regex>/ -> like /<regex>/ but returns a list of the matched data
               (to (lambda (l r p ok) (ok l r p l))))
            ; unicode regex form
            (define form (bytes->string
               (let loop ((l to) (name #n))
                  (cond
                     ((eq? l from)
                        name)
                     ((integer? (car l))
                        (loop (cdr l) (cons (car l) name)))
                     (else
                        (loop (cdr l) name))))))
            ; create new regex representation with stored form
            (lambda args
               (define name form)
               (apply regex args)) ))

      ;; str -> rex|#false, for conversion of strings to complete matchers
      (define (string->complete-match-regex str)
         (parse get-body-regex (str-iter-bytes str) #false #false #false))

      ;; str → rex|#false, same as is used in owl parser
      (define (string->extended-regexp str)
         (parse get-sexp-regex (str-iter-bytes str) #false #false #false))

      ;; testing
      (define (string->replace-regex str)
         (parse get-replace-regex (str-iter-bytes str) #false #false #false))

      ;; POSIX (ERE)
      (define string->regex
         string->extended-regexp)

))

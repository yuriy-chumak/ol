(define-library (otus blobs)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol blob bytevector vector))
   (description "
      Otus-Lisp BLOB support library")

   ; blobs are one-dimensional data structures indexable by natural numbers,
   ; having O(n log_256 n) access and memory use (effectively O(1)). They are
   ; mainly intended to be used for static data requiring efficient (modulo
   ; owl) iteration and random access.
   ;
   ; in owl, vectors are implemented as complete 256-ary trees. small vectors
   ; fitting to one node of the tree are of raw or allocated type 11, meaning
   ; they usually take 8+4n or 4+n bytes of memory, depending on whether the
   ; values are normal descriptors or fixnums in the range 0-255.
   ;
   ; large vectors are 256-ary trees. each dispatch node in the tree handles
   ; one byte of an index, and nodes starting from root each dispatch the
   ; highest byte of an index. when only one byte is left, one reads the
   ; reached leaf node, or the leaf node stored to the dispatch node.
   ;
   ; thus reading the vector in order corresponds to breadth-first walk
   ; of the tree. notice that since no number > 0 has 0 as the highest
   ; byte, the first dispatch position of the root is always free. this
   ; position contains the size of the vector, so that it is accessable
   ; in O(1) without space overhead or special case handling. leaf nodes
   ; have the size as part of the normal owl object header.

   ;; order example using binary trees
   ;
   ;           (0 1)                 bits 0 and 1, only 1 can have children
   ;              |                  dispatch the top bit
   ;            (2 3)                bits from top, 10 11, numbers ending here 2 and 3
   ;            /   \                dispatch top and second bit
   ;           /     \
   ;       (4 5)     (6 7)           bits from top, (100 101) (110 111)
   ;       /  |       |  \
   ;      /   |       |   \
   ; (9 8) (10 11) (12 13) (14 15)   etc
   ;
   ; vectors use the same, but with 256-ary trees, which works well because
   ; it is half of owl's fixnum base, so dispatching can be done easily without
   ; shifting, and not too wide to make array mutations too bulky later.

   (export
      blob?
      blob-len            ; v → n
      blob-ref            ; v x p → v[p] | error
      blob->list
      blob-iter
      blob-iterr
      blob-iter-range      ; vec x start x end -> ll
      blob-fold
      blob-foldr
      blob-range           ; vec x start x end -> vec'
      blob-map             ; (val → val') x vec → vec'

      list->blob
      blob->list ; todo: remove

      ; these assume a sorted vector (as used by pred) having matches in one continuous range
      ;vec-match-range         ; vec x val-pred -> lo x hi | #false x #false
      ;vec-match-range-between ; vec x pred x hi x lo -> lo x hi | #false x #false

      ;vec-equal?         ; v x v → bool
      ;vec-render         ; v x tail → tail'

      merge-chunks          ; exported for use in lib-io (may be moved later)
      leaf-data
      blob-leaves

      ;; vec-cat             ;  vec x vec → vec
      ;; vec-rev
      *blob-leaf-size*)     ; needed for vector IO

   (import
      (scheme core)
      (scheme bytevector)
      (owl lazy)
      (owl list)
      (owl list-extra)
      (owl math))

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define (blob? o) ; == raw or a variant of major type 11?
         (case (type o)
            (type-vector-leaf #true)
            (type-vector-dispatch #true)
            (type-bytevector #true) ; bytevectors is vectors too?
            (else #false)))

      (define ncar car)
      (define ncdr cdr)

      ;; number of bits each vector tree node dispatches from index
      ; (define *blob-bits* (>> *fixnum-bits* 1))
      (define *blob-bits* 16) ;; legacy

      (define *blob-leaf-size* (<< 1 *blob-bits*))
      (define *blob-leaf-max* (- *blob-leaf-size* 1))

      ;;;
      ;;; Vector search
      ;;;

      ;; dispatch low 8 bits of a fixnum, returning the subnode
      (define (vec-dispatch-1 v n)
         (case (type v)
            (type-vector-dispatch ; vector dispatch node with #[Leaf D0 ... D255]
               (lets ((n _ (vm:add (vm:and n *blob-leaf-max*) 2))) ;; jump over header and leaf
                  (ref v n)))
            (else
               (runtime-error "Bad vector node in dispatch-1: type " (type v)))))

      ; dispatch the high half bits of a fixnum, returning the subnode
      (define (vec-dispatch-2 v d) ; -> v'
         (case (type v)
            (type-vector-dispatch
               (lets
                  ((p _ (vm:shr d *blob-bits*))
                   (p _ (vm:add p 2)))
                  (ref v p)))
            (type-vector-leaf
               (runtime-error "Leaf vector in dispatch-2: " v))
            (else
               (runtime-error "Bad vector node in dispatch-2: obj " (type v)))))

      ; dispatch 8-bit parts (256-way tree)
      ; note, the highest one must know whether it must dispatch one or two bytes

      (define (vec-seek v ds)
         (lets ((d ds ds))
            (if (null? ds)
               (if (less? d *blob-leaf-size*) ; just one byte at top digit?
                  (vec-dispatch-1 v d)
                  (vec-dispatch-1 (vec-dispatch-2 v d) d))
               (vec-dispatch-1 (vec-seek v ds) d))))

      ; vec x fixnum -> local value
      (define (vec-ref-digit v n)
         (case (type v)
            (type-bytevector
               (ref v (vm:and n *blob-leaf-max*)))
            (type-vector-dispatch
                (vec-ref-digit (ref v 1) n)) ; read the leaf of the node
            (type-vector-leaf
                (if (eq? n *blob-leaf-max*)
                   (ref v *blob-leaf-size*)
                   (lets ((n _ (vm:add (vm:and n *blob-leaf-max*) 1)))
                     (ref v n))))
            (else
               (runtime-error "bad vector node in vec-ref-digit: type " (type v)))))

      ; find the node holding the last digit and read it
      (define (vec-ref-big v n)
         (vec-ref-digit
            (vec-dispatch-2
               (vec-seek v (ncdr n))
               (ncar n))
            (ncar n)))

      ; vec x n -> vec[n] or fail
      (define (blob-ref v n)
         (case (type n)
            (type-fix+
               (cond
                  ((bytevector? v)
                     (ref v n))
                  ((less? n *blob-leaf-size*)
                     (vec-ref-digit v n))
                  (else
                     (vec-ref-digit (vec-dispatch-2 v n) (vm:and n *blob-leaf-max*)))))
            (type-int+
               (vec-ref-big v n))
            (else
               (runtime-error "blob-ref: bad index: " n))))

      ;;; searching the leaves containing a pos

      ;; todo: switch blob-ref to use blob-leaf-of for int+ indeces

      (define (vec-leaf-big v n)
         (vec-dispatch-2 (vec-seek v (ncdr n)) (ncar n)))

      (define (blob-leaf-of v n)
         (case (type n)
            (type-fix+
               (cond
                  ((eq? (type v) type-bytevector) v)
                  ((less? n *blob-leaf-size*) v)
                  (else (vec-dispatch-2 v n))))
            (type-int+
               (vec-leaf-big v n))
            (else
               (runtime-error "blob-leaf-of: bad index: " n))))


      ;; others

      (define (blob-len vec)
         (case (type vec)
            (type-bytevector
               (size vec))
            (type-vector-dispatch
               (ref vec 2))
            (type-vector-leaf
               (size vec))
            (else
               (runtime-error "blob-len: not a blob " (list vec 'of 'type (type vec))))))



      ;;;
      ;;; Vector construction
      ;;;

      ; note, a blank vector must use a raw one, since there are no such things as 0-tuples (?)
      (define (make-leaf rvals raw?)
         (let ((vals (reverse rvals)))
            (if raw?
               ;; the leaf contains only fixnums 0-255, so make a compact leaf
               (vm:makeb type-bytevector vals) ;; make node and reverse
               ;; the leaf contains other values, so need full 4/8-byte descriptors
               (vm:make type-vector-leaf vals))))

      (define (byte? val)
         (and
            (eq? (type val) type-fix+)
            (eq? val (vm:and val 255))))

      ;; list -> list of leaf nodes
      (define (chunk-list lst out leaves n raw? len)
         (cond
            ((eq? n *blob-leaf-size*) ; flush out to leaves
               (let ((leaf (make-leaf out raw?)))
                  (chunk-list lst null (cons (make-leaf out raw?) leaves) 0 #true (+ len n))))
            ((null? lst) ; partial (last) leaf
               (if (null? out)
                  (values (reverse leaves) len)
                  (values (reverse (cons (make-leaf out raw?) leaves)) (+ len n))))
            ((pair? lst)
               (if raw?
                  (chunk-list (cdr lst) (cons (car lst) out) leaves (+ n 1) (byte? (car lst)) len)
                  (chunk-list (cdr lst) (cons (car lst) out) leaves (+ n 1) #false len)))
            (else (chunk-list (lst) out leaves n raw? len))))

      (define (grab l n)
         (let loop ((l l) (n n) (taken null))
            (cond
               ((null? l) (values (reverse taken) l))
               ((eq? n 0) (values (reverse taken) l))
               (else
                  (loop (cdr l) (- n 1) (cons (car l) taken))))))

      (define (merge-each l s)
         (cond
            ((null? l) null)
            ((null? s) l)
            ((number? (car l))
               (cons (car l)
                  (merge-each (cdr l) s)))
            (else
               (lets ((these s (grab s *blob-leaf-size*)))
                  (cons
                     (vm:make type-vector-dispatch (cons (car l) these))
                     (merge-each (cdr l) s))))))

      (define (merger l n)
         (if (null? l)
            null
            (lets ((these l (grab l n)))
               (if (null? l)
                  these
                  (merge-each these (merger l (* n n)))))))

   ; start with power 1 and blank root
   ; grab power nodes from list -> these others
   ;   if others is null, return these
   ;   otherwise recurse on others -> others
   ;   for each of these
   ;     grab a max of 256 things from others -> below others
   ;     make a dispatch node for these and below
   ;     loop and return the list of these

      (define (cut-at lst pos out)
         (cond
            ((null? lst)
               (values (reverse out) null))
            ((eq? pos 0)
               (values (reverse out) lst))
            (else
               (cut-at (cdr lst) (- pos 1) (cons (car lst) out)))))

      (define (levels lst width)
         (lets ((here below (cut-at lst width null)))
            (if (null? below)
               (list here)
               (cons here (levels below (* width *blob-leaf-size*)))))) ; everything below the first level branches 256-ways

      (define (merge-levels lst)
         (foldr
            (λ (this below)
               ;; this = list of leaves which will be as such or in dispatch nodes
               ;;        on this level of the tree
               ;; below = possible list of nodes up to 256 of which will be attached
               ;;         as subtrees to each leaf of this level, starting from left
               (let loop ((below below) (this this))
                  (cond
                     ((null? below) this)
                     ((null? this)
                        (runtime-error "out of leaves before out of data: " (length below)))
                     ;((number? (car this)) ;; skip size field at roo
                     ;   (cons (car this) (loop below (cdr this))))
                     (else
                        (lets ((here below (cut-at below *blob-leaf-size* null)))
                           ;; attach up to 256 subtrees to this leaf
                           (cons
                              (vm:make type-vector-dispatch (cons (car this) here))
                              (loop below (cdr this))))))))
            null (levels lst *blob-leaf-max*)))

      ; handle root here, since it is special in having 255 subtrees only (0-slot is empty and has size)
      (define (merge-chunks ll len)
         (cond
            ((null? ll)
               ;; no leaves, no data
               [])
            ((null? (cdr ll))
               ;; just one leaf, so it is also the vector
               (car ll))
            (else
               ;; the top node is special in that it has the size field
               ;; others can be computed easily recursively
               (lets
                  ((low (car ll))                  ;; first leaf data, places 0-255
                   (fields (cdr ll))    ;; fill in the length of the vector at dispatch position 0
                   (subtrees (merge-levels fields))) ;; construct the subtrees
                  (vm:make type-vector-dispatch (ilist low len subtrees))))))


      (define (list->blob l)
         (cond
            ((null? l)
               [])
            (else
               ;; leaves are chunked specially, so do that in a separate pass. also
               ;; compute length to avoid possibly forcing a computation twice.
               (lets ((chunks len (chunk-list l null null 0 #true 0)))
                  ;; convert the list of leaf vectors to a tree
                  (merge-chunks chunks len)))))

      ;;;
      ;;; Vector iterators
      ;;;

      ;; iter - iterate forwards (leaves from left to right, tree breadth first left to right)

      (define (iter-raw-leaf v p tl)
         (if (eq? p 0)
            (cons (ref v p) tl)
            (lets ((n _ (vm:sub p 1)))
               (iter-raw-leaf v n (cons (ref v p) tl)))))

      (define (iter-leaf v p tl)
         (if (eq? p 0)
            tl
            (lets ((n _ (vm:sub p 1)))
               (iter-leaf v n (cons (ref v p) tl)))))

      (define (iter-leaf-of v tl)
         (case (type v)
            (type-vector-dispatch (iter-leaf-of (ref v 1) tl))
            (type-bytevector
               (let ((s (size v)))
                  (if (eq? s 0)
                     tl
                     (iter-raw-leaf v (- s 1) tl))))
            (else tl))) ; size field -> number

      (define (blob-iter v)
         (let loop ((end (blob-len v)) (pos 0))
            (let ((this (blob-leaf-of v pos)))
               (iter-leaf-of this
                  (λ () (let ((pos (+ pos *blob-leaf-size*))) (if (< pos end) (loop end pos) null)))))))

      (define (iter-leaf-range v p n t)
         (if (eq? n 0)
            t
            (pair (blob-ref v p)
               (iter-leaf-range v (+ p 1) (- n 1) t))))

      (define (iter-range-really v p n)
         (let ((start (band p *blob-leaf-max*)))
            (cond
               ((eq? start 0)
                  ;; read leaf from beginning
                  (if (> n *blob-leaf-max*)
                     ;; iter a full leaf (usual suspect)
                     (iter-leaf-of (blob-leaf-of v p)
                        (λ () (iter-range-really v (+ p *blob-leaf-size*) (- n *blob-leaf-size*))))
                     ;; last leaf reached, iter prefix and stop
                     (iter-leaf-range (blob-leaf-of v p) 0 n null)))
               ((eq? n 0) null)
               ((less? n (- *blob-leaf-size* start))
                  ;; the whole range is in a part of this leaf
                  (iter-leaf-range (blob-leaf-of v p) start n null))
               (else
                  ;; this is the first leaf. iter a suffix of it.
                  (lets
                     ((n-here (- *blob-leaf-size* start))
                      (n-left (- n n-here)))
                     (iter-leaf-range (blob-leaf-of v p) start n-here
                        (λ () (iter-range-really v (+ p n-here) n-left))))))))

      (define (blob-iter-range v p e)
         (if (<= e (blob-len v))
            (cond
               ((< p e)
                  (iter-range-really v p (- e p)))
               ((= p e) null)
               (else (runtime-error "blob-iter-range: bad range " (cons p e))))
            (runtime-error "blob-iter-range: end outside of vector: " e)))

      ;; iterate back to front

      ;; todo: blob-iterr could also chunk whole leaves directly with fixnums like blob-iterr
      (define (iterr-raw-leaf v last tl)
         (if (eq? last 0)
            tl
            (lets ((last (- last 1)))
               (cons (ref v last)
                  (λ () (iterr-raw-leaf v last tl))))))

      (define (iterr-leaf v p tl)
         (if (eq? p 1)
            (cons (ref v p) tl)
            (cons (ref v p) (λ () (iterr-leaf v (- p 1) tl)))))

      (define (iterr-any-leaf v tl)
         (case (type v)
            (type-vector-dispatch (iterr-any-leaf (ref v 1) tl))
            (type-bytevector (iterr-raw-leaf v (size v) tl))
            (type-vector-leaf (iterr-leaf v (size v) tl))
            (else
               tl))) ; size field in root is a number → skip

      (define (blob-iterr-loop v p)
         (if (eq? type-fix- (type p))
            null
            (iterr-any-leaf (blob-leaf-of v p)
               (λ () (blob-iterr-loop v (- p *blob-leaf-size*))))))

      (define (blob-iterr v)
         (lets
            ((end (blob-len v))
             (last (band end *blob-leaf-max*)))
            (cond
               ((eq? last 0) ; vec is empty or ends to a full leaf
                  (if (eq? end 0) ; blank vector
                     null
                     (blob-iterr-loop v (- end 1)))) ; start from previous leaf
               (else
                  (blob-iterr-loop v (- end 1))))))

      ;; vector folds

      (define (blob-fold  op st vec) (lfold  op st (blob-iter  vec)))
      (define (blob-foldr op st vec) (lfoldr op st (blob-iterr vec)))

      ;; list conversions

      (define (blob->list vec)
         (cond
            ((bytevector? vec)
               ;; convert raw vectors directly to allow this to be used also for large chunks
               ;; which are often seen near IO code
               (bytevector->list vec))
            (else
               (blob-foldr cons null vec))))

      (define (leaf-data leaf)
         (if (eq? (type leaf) type-bytevector)
            leaf
            (ref leaf 1)))

      ;;;
      ;;; vector map
      ;;;

      ;; fixme: blob-map <- placeholder
      (define (blob-map fn vec)
          (list->blob (lmap fn (blob-iter vec))))

      ;;;
      ;;; Vector ranges
      ;;;

      ;; fixme: proper blob-range not implemented
      (define (blob-range-naive vec from to) ; O(m log n)
         (list->blob
            (map (λ (p) (blob-ref vec p)) (lrange from 1 to))))

      (define blob-range blob-range-naive)


      ;; blob → a stream of leaves
      (define (blob-leaves vec)
         (let ((end (blob-len vec)))
            (let loop ((pos 0))
               (if (< pos end)
                  (let ((data (leaf-data (blob-leaf-of vec pos))))
                     (pair data (loop (+ pos *blob-leaf-size*))))
                  null))))

      ;; fixme: temporary vector append!
      ;; (define (blob-cat a b)
      ;;    (list->blob
      ;;       (append
      ;;          (blob->list a)
      ;;          (blob->list b))))

      ;; (define (blob-rev a)
      ;;    (list->blob
      ;;       (blob-iterr a)))
))

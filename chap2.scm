;; #lang racket
;; only for the picture language, where I've used racket


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;;;;;;;;; Exercise 2.2 ;;;;;;;;;;;
;; Line segments and points ;;

(define (make-segment point-a point-b)
  ;; As a matter of convenience, line segments will be defined from left to
  ;; right - i.e. solely dependent on the x-coord.
  (if (> (x-point point-b)
         (x-point point-a))
      (cons point-a point-b)
      (cons point-b point-a)))

(define (start-segment line-x)
  (car line-x))

(define (end-segment line-x)
  (cdr line-x))

(define (make-point x y)
  (cons x y))

(define (x-point point-a)
  (car point-a))

(define (y-point point-a)
  (cdr point-a))

(define (midpoint-segment seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (make-point (average x1 x2)
                  (average y1 y2)))))

(define (average a1 a2)
  (/ (+ a1 a2) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display "start: " (x-point p))
  (display ",")
  (display "end: " (y-point p))
  (display ")")
  (newline))

(define (print-segment s)
  (print-point (start-segment s))
  (print-point (end-segment s)))

;;;;;;;;;;;;; Ex 2.3 ;;;;;;;;
;; Rectangles ;;;;;;;;;;;;

(define (perimeter-r r)
  (let ((width (width-r r))
        (height (height-r r)))
    (* 2 (+ height width))))

(define (area-r r)
  (let ((width (width-r r))
        (height (height-r r)))
    (* width height)))

;; Both the above functions are top-level abstractions independent of the
;; implements below


;; The following may seem complicated, but it seems to be the easiest way to me
;; to represent *all* rectangles in a plane - even rotated ones. All we need is
;; the base-segment and the height from that base.

(define (width-r r)
  (length-seg (base-seg r)))

(define (height-r r)
  (length-seg (left-side r)))

(define (length-seg seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (sqrt (+ (square (- x1 x2))
               (square (- y1 y2)))))))

(define (square x)
  (* x x))

(define (base-seg r)
  (car r))

(define (left-side r)
  (cdr r))

(define (make-rectangle base-seg height)
  (let ((p1 (start-segment base-seg))
        (p2 (end-segment base-seg)))
    (let ((x1 (x-point p1))
          (y1 (y-point p1))
          (x2 (x-point p2))
          (y2 (y-point p2)))
      (let ((theta (atan (/ (- y2 y1)
                            (- x2 x1)))))
        (let ((new-x (- x1 (* height (sin theta))))
              (new-y (+ y1 (* height (cos theta)))))
          (cons base-seg
                (make-segment
                 p1
                 (make-point new-x new-y))))))))

;;;;;;;;; Implementing a weird cons ;;;;;;;;;

(define (_cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error ("Argument not 0 or 1 - CONS" m)))))
  dispatch)

(define (_car z) (z 0))

(define (_cdr z) (z 1))

;;;;;;;;; Ex 2.4 ;;;;;;;;;

(define (__cons x y)
  (lambda (m) (m x y)))

(define (__car z)
  (z (lambda (p q) p)))

(define (__cdr z)
  (z (lambda (p q) q)))

;;;;;;;; Ex 2.5 ;;;;;;;;;;;;;;

(define (___cons x y)
  (if (and (integer? x)
           (integer? y)
           (positive? x)
           (positive? y))
      (* (pow 2 x)
         (pow 3 y))
      (error ("This method is only defined for non-negative integer values of x and y - ___CONS"))))

(define (pow a b)
  (if (= b 1)
      a
      (* a (pow a (- b 1)))))

(define (___car z)
  (if (= 1 (gcd z 2))
      0
      (+ 1 (___car (/ z 2)))))

(define (___cdr z)
  (if (= 1 (gcd z 3))
      0
      (+ 1 (___car (/ z 3)))))

;;;;;;;;; Ex 2.6 ;;;;;;;;;;
;;;; Church numerals ;;;;;;

(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add-church m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

;; To test these functions, try calling the following
;; ((zero (lambda (n) (+ n 1))) 0)
;; Replace 'zero' with 'one' or 'two'

;; We can define the following generic un-church function

(define (un-church num)
  ((num (lambda (n) (+ n 1))) 0))

;; (un-church (add-church one two)) gives 3 baby!!!!
;; This was based on the observation that we're calling f repeatedly

;;;;;;;; Interval Arithmetic ;;;;;;;;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
      (error ("The divisor interval spans 0" y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;;;;;; Ex 2.7 ;;;;;;;;;;

(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

;;;;;;; Ex 2.8 ;;;;;;;;;;

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;;;;;;; Ex 2.9 ;;;;;;;;;;

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Let i be the interval (il iu) Let j be the interval (jl ju). When we add i
;; and j, the width of the result is ((iu + ju) - (il + jl)) / 2. Which is the
;; same as ((iu - il)/2 + (il + jl)/2). So the widths are added. When we
;; subtract j from i, the width of the result is ((iu - jl) - (il - ju)) /2.
;; Which is the same as ((iu - il)/2 + (ju - jl)/2). Again the widths are added.

;; When we multiply two intervals - the resultant width is a function of the
;; absolute values of the lower and upper bounds.

;;;;;;;;;; Ex 2.10 - Implemented above ;;;;;

;;;;;;;;;; Ex 2.11 ;;;;;;;;;;;

;; (define (_mul-interval x y)
;;   (let ((x1 (lower-bound x))
;;         (x2 (upper-bound x))
;;         (y1 (lower-bound y))
;;         (y2 (upper-bound y)))
;;     (cond ((and (positive? x1)
;;                 (positive? x2)
;;                 (positive? y1)
;;                 (positive? y2))
;;            (make-interval (* x1 y1) (* x2 y2)))
;;           ... etc...)))

;;;;;;;;; Ex 2.12 ;;;;;;;;;;;;

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (* (- 1 p) c)
                 (* (+ 1 p) c)))

(define (percent i)
  (/ (width-interval i)
     (center i)))

;; Ex 2.13 ;;

;; The percentage tolerances get added when multiplying the intervals
;; (define i1 (make-center-percent 1 0.1))
;; (define i2 (make-center-percent 10 0.05))
;; (percent (mul-interval i1 i2)) gives us 0.149..

;; Ex 2.14 - 2.16 ;;

;; This is about propogation of errors. The more number of
;; operations, the higher the error propogation. In the first case we're
;; multiplying, adding and dividing two intervals. Every time we multiply or
;; divide, the error doubles. In the second case, the "one" variable has 0
;; percent width. So the error does not propogate unnecessarily. Therefore the
;; second solution is better.

;; scheme@(guile-user)> (define ione (make-interval 1 1))
;; scheme@(guile-user)> (percent ione)
;; $4 = 0
;; scheme@(guile-user)> (percent (div-interval ione ione))
;; $5 = 0.0
;; scheme@(guile-user)> (define i2 (make-center-percent 1 0.0000001))
;; scheme@(guile-user)> (percent (div-interval i2 i2))
;; $6 = 2.000000000612583e-7
;; scheme@(guile-user)> (define i3 (make-center-percent 1 0.05))
;; scheme@(guile-user)> (percent (div-interval i3 i3))
;; $7 = 0.09975062344139651

;; My guess is - we need a system of "factorisation" that removes as much as
;; possible such sources of errors.


;; Ex 2.17 ;;

(define (_last-pair l)
  (if (<= 1 (length l))
      l
      (_last-pair (cdr l))))

;; Ex 2.18 ;;

(define (_reverse l)
  (define (reverse-helper l1 l2)
    (if (null? l1)
        l2
        (reverse-helper (cdr l1)
                        (cons (car l1) l2))))
  (reverse-helper l '()))

(define (__reverse l)
  (if (null? l)
      l
      (append (__reverse (cdr l))
              (list (car l)))))

;; Ex 2.19 ;;
;; Change counting revisited ;;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; Note - I have NEVER seen a 0.5p coin in the UK - but ok - we follow this
;; through

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-vals)
  (car coin-vals))

(define (except-first-denomination coin-vals)
  (cdr coin-vals))

(define (no-more? coin-vals)
  (null? coin-vals))

;; Ex 2.20 ;;
;; same-parity function

(define (same-parity lead . tail)
  (define (sp-help f li lo)
    (cond ((null? li) lo)
          ((f (car li)) (sp-help f (cdr li)
                                 (append lo (list (car li)))))
          (else (sp-help f (cdr li) lo))))
  (sp-help (if (even? lead) even? odd?)
           tail
           (list lead)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (_map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (_map proc (cdr items)))))

;; Ex 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (_square-list items)
  (map (lambda (x) (* x x)) items))

;; Ex 2.23 The main thing here is how to get a multiline working One way might
;; be through let. Another (used here) is via cond.

(define (_for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (_for-each proc (cdr items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; Ex 2.27
;; Deep reverse

(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))

(define (_deep-reverse x)
  (if (pair? x)
      (reverse (map _deep-reverse x))
      x))

;; Ex 2.28
;; Fringe - a bit tough

(define nil '())

(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? (car l))) (cons (car l)
                                     (fringe (cdr l))))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))

(define flatten fringe)

;; Ex 2.29
;; Binary mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)

(define (right-branch mobile)
  (list-ref mobile 1))

(define branch-length car)

(define (branch-structure branch)
  (list-ref branch 1))

(define (total-weight mobile)
  (let ((lbs (branch-structure (left-branch mobile)))
        (rbs (branch-structure (right-branch mobile))))
    (+ (branch-weight lbs)
       (branch-weight rbs))))

(define (branch-weight branch-structure)
  (if (not (pair? branch-structure))
      branch-structure
      (total-weight branch-structure)))

(define (balanced? mobile)
  (let ((lbl (branch-length (left-branch mobile)))
        (rbl (branch-length (right-branch mobile)))
        (lbw (branch-weight (branch-structure (left-branch mobile))))
        (rbw (branch-weight (branch-structure (right-branch mobile))))
        (lbs (branch-structure (left-branch mobile)))
        (rbs (branch-structure (right-branch mobile))))
    (and (= (* lbw lbl)
            (* rbw rbl))
         (if (not (pair? lbs))
             #t
             (balanced? lbs))
         (if (not (pair? rbs))
             #t
             (balanced? rbs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (_scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (_scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Ex 2.30 ;;;;;;;

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (_square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (_square-tree (car tree))
                    (_square-tree (cdr tree))))))

;; Ex 2.31 ;;;;
;; Tree map abstraction

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree))))))

(define (__square-tree tree) (tree-map square tree))

;; Ex 2.32 ;;
;; Subsets ;;

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset))
                          rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Sequences as conventional interfaces ;;;;;;;;;;

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) (if (odd? tree)
                                (square tree)
                                0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (range n)
  (define (iter-range n ans)
    (if (< n 0)
        ans
        (iter-range (- n 1) (cons n ans))))
  (iter-range (- n 1) nil))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (range-interval low high)
  (if (> low high)
      nil
      (cons low (range-interval (+ low 1) high))))

;; Ex 2.33 ;;

(define (__map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))

(define (_append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (_length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0 sequence))

;; Ex 2.34 ;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

;; Ex 2.35 ;;

(define (_count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       t)))

;; Ex 2.36 ;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Ex 2.37 ;;

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (sub-vec)
         (dot-product v sub-vec))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (sub-vec)
           (matrix-*-vector cols sub-vec))
         m)))

;; Ex 2.38 ;;
;; Fold-left ;;

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

;; scheme@(guile-user)> (fold-left / 1 (list 1 2 3))
;; $39 = 1/6
;; scheme@(guile-user)> (fold-right / 1 (list 1 2 3))
;; $40 = 3/2

;; scheme@(guile-user)> (fold-right list nil (list 1 2 3))
;; $42 = (1 (2 (3 ())))
;; scheme@(guile-user)> (fold-left list nil (list 1 2 3))
;; $43 = (((() 1) 2) 3)

;; Fold-left and fold-right should gave the same answer if the operation being
;; applied is commutative and associative. So (fold-left + 0 '(1 2 3)) =
;; (fold-right + 0 '(1 2 3))

;; Ex 2.39 ;;

(define (___reverse sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil
              sequence))

(define (____reverse sequence)
  (fold-left (lambda (x y)
               (cons y x))
             nil sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (accumulate append
;;             nil
;;             (map (lambda (i)
;;                    (map (lambda (j) (list i j))
;;                         (range-interval 1 (- i 1))))
;;                  (range-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; This is equivalent to mapcat in clojure.

(define mapcat flatmap)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define true #t)
(define false #f)

(define (make-pair-sum pair)
  (append pair (list (+ (car pair)(cadr pair)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (range-interval 1 (- i 1))))
                (range-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x)
            (not (= x item)))
          seq))

;;;;;;;;;;;;;
;; Ex 2.40 ;;

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (range-interval 1 (- i 1))))
           (range-interval 1 n)))

(define (_prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;;;;;;;;;;;;;
;; Ex 2.41 ;;

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (range-interval 1 (- j 1))))
                      (range-interval 1 (- i 1))))
           (range-interval 1 n)))

(define (sum-ordered-triples n s)
  (filter (lambda (l)
            (= s (accumulate + 0 l)))
          (ordered-triples n)))

;;;;;;;;;;;;;
;; Ex 2.42 ;;

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions)) ;; We don't really need k here so removed
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (range-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position row column existing-positions)
  (cons (list row column) existing-positions))

(define (safe? positions)
  (or (= 1 (length positions)) ;; There is only 1 queen on the board

      (let ((last-queen (car positions))
            (other-queens (cdr positions)))

        ;; We check if any other queen is in the same row
        ;; And check for diagonal attacks
        ;; By definition no other queen would be in the same column, so don't need
        ;; to check for that

        (null? (filter (lambda (position)
                         (or  (= (car position)
                                 (car last-queen))

                              (= (abs (- (car last-queen)
                                         (car position)))
                                 (abs (- (cadr last-queen)
                                         (cadr position))))))
                       other-queens)))))

;;;;;;;;;;
;; Some other useful functions

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc x)
  (+ 1 x))

(define (repeated proc n)
  (if (= n 1)
      proc
      (compose proc (repeated proc (dec n)))))

(define (dec x)
  (- x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Picture Language ;;;;;;;;;;;;;;;;;;;;;;;

;; This work is done in racket by importing the package sicp-pict
;; Only 2 exercises worked on

;; (define (push comb)
;;   (lambda (pict n a)
;;     ((repeated
;;       (lambda (p) (comb pict p a))
;;       n)
;;      pict)))

;; (define right-push (push beside))

;; (require sicp-pict) ;; Comment this out in normal scheme

;; (define e2 (beside einstein (flip-vert einstein)))

;; (define e4 (below e2 e2))

;; (define (flipped-pairs painter)
;;   (let ((painter2 (beside painter (flip-vert painter))))
;;     (below painter2 painter2)))

;; (define _e4 (flipped-pairs einstein))

;; ;;;;;;;;;;;;;
;; ;; Ex 2.49 ;;

;; (define frame-outline
;;   (segments->painter
;;    (list (make-segment (make-vect 0 0)
;;                        (make-vect 1 0))
;;          (make-segment (make-vect 1 0)
;;                        (make-vect 1 1))
;;          (make-segment (make-vect 1 1)
;;                        (make-vect 0 1))
;;          (make-segment (make-vect 0 1)
;;                        (make-vect 0 0)))))

;; (define x-painter
;;   (segments->painter
;;    (list (make-segment (make-vect 0 0)
;;                        (make-vect 1 1))
;;          (make-segment (make-vect 1 0)
;;                        (make-vect 0 1)))))

;; (define diamond
;;   (segments->painter
;;    (list (make-segment (make-vect 0.5 0)
;;                        (make-vect 1 0.5))
;;          (make-segment (make-vect 1 0.5)
;;                        (make-vect 0.5 1))
;;          (make-segment (make-vect 0.5 1)
;;                        (make-vect 0 0.5))
;;          (make-segment (make-vect 0 0.5)
;;                        (make-vect 0.5 0)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;
;; ;; Ex 2.44 ;;

;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (dec n))))
;;         (beside painter (below smaller smaller)))))

;; (define (up-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (up-split painter (dec n))))
;;         (below painter (beside smaller smaller)))))

;; (define (corner-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((up (up-split painter (dec n)))
;;             (right (right-split painter (dec n))))
;;         (let ((top-left (beside up up))
;;               (bottom-right (below right right))
;;               (corner (corner-split painter (dec n))))
;;           (beside (below painter top-left)
;;                   (below bottom-right corner))))))

;; (define (square-limit painter n)
;;   (let ((quarter (corner-split painter n)))
;;     (let ((half (beside (flip-horiz quarter) quarter)))
;;       (below (flip-vert half) half))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;;;;;;;;;;;;
;; Ex 2.54 ;;

(define (_equal? list1 list2)
  (cond ((and (null? list1)
              (null? list2))
         #t)

        ((or (null? list1)
             (null? list2))
         #f)

        ((and (pair? (car list1))
              (pair? (car list2)))
         (and (_equal? (car list1) (car list2))
              (_equal? (cdr list1) (cdr list2))))

        (else (and (eq? (car list1) (car list2))
                   (_equal? (cdr list1) (cdr list2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symbolic differentiation

;;;;;;;;;;;;;;;;;;;;
;; Ex 2.56 + 2.57 ;;

;; Symbolic differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        ((exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp)
                                                                   (dec (exponent exp))))
                                (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum . xs)
  (let ((nums (filter number? xs))
        (vars (filter (compose not number?) xs)))
    (let ((part-sum (accumulate + 0 nums)))
      (cond ((null? vars) part-sum)
            ((and (= 0 part-sum)
                  (= 1 (length vars)))
             (car vars))
            ((= 0 part-sum) (append '(+) vars))
            (else (append '(+) (list part-sum) vars))))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product . xs)
  (let ((nums (filter number? xs))
        (vars (filter (compose not number?) xs)))
    (let ((part-prod (accumulate * 1 nums)))
      (cond ((= 0 part-prod) 0)
            ((null? vars) part-prod)
            ((and (= 1 part-prod)
                  (= 1 (length vars)))
             (car vars))
            ((= 1 part-prod) (append '(*) vars))
            (else (append '(*) (list part-prod) vars))))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (append '(+) (cddr s))))

(define (augend s) (caddr s))


(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (append '(*) (cddr p))))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (base ex) (cadr ex))

(define (exponent ex) (caddr ex))

;;;;;;;;;;;;;
;; Ex 2.58 ;;

(define (_deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((_sum? exp) (_make-sum (_deriv (_addend exp) var)
                                (_deriv (_augend exp) var)))
        ((_product? exp) (_make-sum
                          (_make-product (_multiplier exp)
                                        (_deriv (_multiplicand exp) var))
                          (_make-product (_deriv (_multiplier exp) var)
                                       (_multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (_make-sum a1 a2)
  (cond ((=number? a1 0) a2) ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (_sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (_addend exp) (car exp))

(define (_augend exp) (caddr exp))

(define (_make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (_product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (_multiplier exp) (car exp))

(define (_multiplicand exp) (caddr exp))

;; Part (b) We use memq The following solution works because the order of
;; precedence is defined in the deriv function itself. Sum is called before
;; product. Then the derivs of the addend and augend are applied separately.
;; This ensures the product is applied BEFORE sum.

(define (operation expr)
  (if (memq '+ expr)
      '+
      '*))

(define (__sum? expr)
  (eq? '+ (operation expr)))

(define (split-before expr sym)
  (define (iter expr result)
    (if (eq? (car expr) sym)
        result
        (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '())))
    (if (= (length result) 1)
        (car result)
        result)))

(define (__addend expr)
  (split-before expr '+))

(define (__multiplier expr)
  (split-before expr '*))

(define (split-after expr sym)
  (let ((result (cdr (memq sym expr))))
    (if (= (length result) 1)
        (car result)
        result)))

(define (__augend expr)
  (split-after expr '+))

(define (__multiplicand expr)
  (split-after expr '*))

(define (__product? expr)
  (eq? '* (operation expr)))

(define (__deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((__sum? exp) (_make-sum (__deriv (__addend exp) var)
                                (__deriv (__augend exp) var)))
        ((__product? exp) (_make-sum
                          (_make-product (__multiplier exp)
                                         (__deriv (__multiplicand exp) var))
                          (_make-product (__deriv (__multiplier exp) var)
                                         (_multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;;;;;;;;;;;;
;; Ex 2.59 ;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;;;;;;;;;;;;;
;; Ex 2.60 ;;

(define (_adjoin-set x set)
  (cons x set))

(define (_union-set set1 set2)
  (append set1 set2))

;; The above two procedures are simpler. Intersection and element-of will not be any easier 

;;;;;;;;;;;;;;;;;;;;;
;; Sets as ordered list ;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;;;;;;;;;;;;
;; Ex 2.61 ;;

(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-set set1 (cdr set2))))
                      ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets as binary trees ;;;;

(define (entry tree) (car tree))
(define (left-branch tree) (card tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


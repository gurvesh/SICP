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

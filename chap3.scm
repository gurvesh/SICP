(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficint funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;;;;;;;;;;;;
;; Ex 3.1 ;;

(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))

;;;;;;;;;;;;
;; Ex 3.2 ;;

(define (make-monitored proc)
  (let ((calls 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
          calls
          (begin (set! calls (+ calls 1))
                 (proc arg))))))

;;;;;;;;;;;;
;; Ex 3.3 ;;

(define (make-account balance password)
  (let ((previous-incorrect-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 (set! previous-incorrect-attempts 0)
                 balance)
          "Insufficint funds"))
    (define (deposit amount)
      (set! previous-incorrect-attempts 0)
      (set! balance (+ balance amount))
      balance)
    (define (caught-a-fraud amount)
      (set! previous-incorrect-attempts (+ 1 previous-incorrect-attempts))
      (when (= previous-incorrect-attempts 7)
        (call-the-cops amount))
      (display "Incorrect Password")
      (newline))
    (define (dispatch password-entered m)
      (cond ((not (eq? password-entered password)) caught-a-fraud)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (call-the-cops amount)
  (display "You're in trouble")
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rand-update x)
  (let ((a (expt 2 32))
        (b 1103515245)
        (m 12345))
    (modulo (+ (* a x) b) m)))

(define rand-init (rand-update 137))

(define rand (let ((x rand-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= 1 (gcd (rand) (rand))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1)
                              (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(test)

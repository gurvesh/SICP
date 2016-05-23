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
  (let ((a 16807)
        (b 0)
        (m (- (expt 2 31) 1)))
    (modulo (+ (* a x) b) m)))

(define rand-init (rand-update 1))

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

;;;;;;;;;;;;
;; Ex 3.5 ;;

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral pred
                           lower_x upper_x
                           lower_y upper_y
                           trials)
  (define (region-test)
    (let ((x (random-in-range lower_x upper_x))
          (y (random-in-range lower_y upper_y)))
      (pred x y)))

  (* (monte-carlo trials region-test)
     (* (- upper_x lower_x)
        (- upper_y lower_y))))

(define (unit-circle-pred x y)
  (>= 1 (+ (expt x 2)
           (expt y 2))))

(define (estimate-pi-integral trials)
  (estimate-integral unit-circle-pred
                     -1.0 1.0
                     -1.0 1.0
                     trials))

;;;;;;;;;;;;
;; Ex 3.6 ;;

(define rand2
  (let ((x rand-init))
    (define (generate-on-old-chain) ;; If we define it as a var, then set! will
                                    ;; only get called the first time that var
                                    ;; is accessed
      (set! x (rand-update x))
      x)
    (define (seed-new seed)
      (set! x (rand-update seed)))
    (define (dispatch msg)
      (cond ((eq? msg 'generate) (generate-on-old-chain)) ;; Note that we call
                                                          ;; the function.
            ((eq? msg 'reset) seed-new)
            (else (error "Unknown message: RAND2" msg))))
    dispatch))

;;;;;;;;;;;;
;; Ex 3.7 ;;

(define (make-joint existing-ac
                    existing-pw
                    new-pw)
  (define (dispatch password-entered msg)
    (cond ((not (eq? password-entered new-pw))
           (existing-ac "" 'withdraw)) ;; We make use of the existing
                                       ;; fraud-detection, and pass it a dummy
                                       ;; password. Note this stage can only
                                       ;; occur after the joint-account is
                                       ;; active
          ((or (eq? msg 'withdraw)
               (eq? msg 'deposit))
           (existing-ac existing-pw msg))
          (else (error "Unknown request: MAKE-JOINT" msg))))
  (when (number? ((existing-ac existing-pw 'deposit) 0))
    ;; Only proceed if the token deposit succeeds - which will be known if a
    ;; number is returned.
    dispatch))

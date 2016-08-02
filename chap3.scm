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
        "Insufficient funds"))
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

(define (random range)
  (round (* range (/ (rand)(- (expt 2 31) 1)))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral pred
                           lower_x upper_x
                           lower_y upper_y
                           trials)
  (define (region-test)
    (let ((x (/ (random-in-range (* trials lower_x)
                                 (* trials upper_x))
                trials))
          (y (/ (random-in-range (* trials lower_y)
                                 (* trials upper_y))
                trials)))
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

;;;;;;;;;;;;
;; Ex 3.8 ;;

(define f
  (let ((counter -1))
    (lambda (x)
      (begin
        (set! counter (+ counter 1))
        (if (even? counter) x 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mutable List Structure ;;;;;;;

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (_append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;;;;;;;;;;;;
;; Ex 3.16 ;;

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; (define z (list 'a 'b 'c))
;; (count-pairs z)
;;  3
;; (set-car! (cdr z) (cddr z))
;; (count-pairs z)
;;  4
;; z
;;  (a (c) c)

;;;;;;;;;;;;;
;; Ex 3.17 ;;

(define (correct-count-pairs xs)
  (let ((pairs-traversed '()))
    (define (count-pairs-helper xs)
      (cond ((not (pair? xs)) 0)
            ((memq xs pairs-traversed) 0)
            (else (begin (set! pairs-traversed
                               (cons xs pairs-traversed))
                         (+ (count-pairs-helper (car xs))
                            (count-pairs-helper (cdr xs))
                            1)))))
    (count-pairs-helper xs)))

;;;;;;;;;;;;;
;; Ex 3.18 ;;

;; This is a constant space solution - but only works for a tail loop

(define (check-cycle xs)
  (define (inf-cdr-check ys)
    (cond ((null? (cdr ys)) #f)
          ((eq? (cdr ys) xs) #t)
          (else (inf-cdr-check (cdr ys)))))
  (inf-cdr-check xs))

;; A more generic solution - that detects cycles not in the tail
;; position as well.  See
;; http://community.schemewiki.org/?sicp-ex-3.18

(define (has-loop? lis)
   (define (iter searchlist seen)
     (cond ((not (pair? searchlist)) #f)
           ((memq searchlist seen) #t)
           (else (or (iter (car searchlist) (cons searchlist seen))
                     (iter (cdr searchlist) (cons searchlist seen))))))
   (iter lis '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queues ;;;;;;;;;

;; Select and modify front and rear pointers:

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;; Selectors:

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;; Constructor:

(define (make-queue)
  (cons '() '()))

;; Modifiers:

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;;;;;;;;;;;;;
;; Ex 3.21 ;;

(define (print-queue q)
  (front-ptr q))

;;;;;;;;;;;;;
;; Ex 3.22 ;;

(define (make-queue-state)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else (set-front-ptr! (cdr front-ptr))
                  front-ptr)))
    (define (print-queue) front-ptr)
    (define (front-queue)
      (car front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Unknown operation: QUEUE" m))))
    dispatch))

;;;;;;;;;;;;;
;; Ex 3.23 ;;

;; DEQUE -> Double ended queue The important part is to realise that now I need
;; TWO pointers per data item (I couldn't realise this on my own). Usually there
;; is one pointer, which points to the next item in the list. Now I need an
;; additional one that also points to the previous item (if any). As I will
;; maintain the rear-ptr at all times, so its not that hard. The rear-ptr now
;; will contain a reference to the previous items too.

;; So my data representation should be (item . ptr-prev . ptr-next)

(define (make-deque) (cons '() '()))

;; I'll use the front-ptr and rear-ptr functions from the queue representation.
;; But the rear-ptr now will be a list.

(define empty-deque? empty-queue?) ;; Reuse queue fn

(define (front-insert-deque! deque item)
  (let ((new-triple (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-triple)
           (set-rear-ptr! deque new-triple))
          (else
           ;; First set the ptr-next of the new triple to the item already at the front
           (set-cdr! (cdr new-triple) (front-ptr deque))

           ;; Next set the ptr-prev of the old item at the front, to the new item
           ;; The ptr to be changed is the 2nd item in old queue
           (set-car! (cdr (front-ptr deque)) new-triple)

           ;; Finally set the front-ptr of the deque to the new item
           (set-front-ptr! deque new-triple)))))

(define (rear-insert-deque! deque item)
  (let ((new-triple (cons item (cons  '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-triple)
           (set-rear-ptr! deque new-triple))
          (else
           ;; First set the ptr-next of the item already at the end to the new item
           (set-cdr! (cdr (rear-ptr deque)) new-triple)

           ;; Next set the ptr-prev of the new item to the item which was at the end already
           (set-car! (cdr new-triple) (rear-ptr deque))

           ;; Finally set the rear-ptr of the deque to the new item
           (set-rear-ptr! deque new-triple)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error ("Trying to delete an empty Deque" deque)))
        (else
         ;; I use cddr because the ptr-next is the 3rd item in my rep
         (set-front-ptr! deque (cddr (front-ptr deque)))
         (or (empty-deque? deque) ;; Do nothing
             (set-car! (cdr (front-ptr deque)) '())))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error ("Trying to delete an empty Deque" deque)))
        (else
         (set-rear-ptr! deque (cadr (rear-ptr deque)))
         (if (null? (rear-ptr deque)) ;; Unlike front-delete, now we don't know
                                      ;; directly if the queue is empty
             (set-front-ptr! deque '()) ;; If nothing left on rear-ptr, then
                                        ;; return an empty deque. Weirdly -
                                        ;; (set! deque (make-deque)) doesn't
                                        ;; work
             (set-cdr! (cdr (rear-ptr deque)) '())))))

;; Print-deque has to be an O(n) task.
(define (print-deque deque)
  (define (iter-print front result)
    (if (null? front)
        result
        (iter-print (cddr front)
                    (append result (list (car front))))))
  (iter-print (front-ptr deque) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;
;; Tables ;;

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-dimensional tables ;;

(define (lookup2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert2! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-2d-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;;;;;;;;;;;;
;; Ex 3.24 ;;

(define (sample-same-key? k1 k2)
  (let ((tolerance 0.1))
    (> tolerance
       (/ (abs (- k2 k1))
          k1))))

(define (make-2d-table-with-tolerance same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;;;;;;;;;;;;;
;; Ex 3.25 ;;

(define (gen-nested-record list-of-keys value)
  (if (null? (cdr list-of-keys))
      (cons (car list-of-keys) value)
      (list (car list-of-keys) (gen-nested-record (cdr list-of-keys) value))))

(define (make-n-table)
  (let ((local-table (list '*table*)))
    (define (lookup list-of-keys)
      (define (sub-lookup keys-left current-table)
        (if (null? (cdr keys-left))
            (let ((record (assoc (car keys-left) (cdr current-table))))
              (if record
                  (cdr record)
                  #f))
            (let ((new-subtable (assoc (car keys-left) (cdr current-table))))
              (if new-subtable
                  (sub-lookup (cdr keys-left) new-subtable)
                  #f))))
      (sub-lookup list-of-keys local-table))
    (define (insert! list-of-keys value)
      (define (sub-insert! keys-left current-table)
        (if (null? (cdr keys-left))
            (let ((record (assoc (car keys-left) (cdr current-table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! current-table
                            (cons (cons (car keys-left) value)
                                  (cdr current-table)))))
            (let ((new-subtable (assoc (car keys-left) (cdr current-table))))
              (if new-subtable
                  (if (pair? (cdr new-subtable))
                      (sub-insert! (cdr keys-left) new-subtable)
                      (set-cdr! new-subtable
                                (list (gen-nested-record (cdr keys-left) value))))
                  (set-cdr! current-table
                            (cons (gen-nested-record keys-left value)
                                  (cdr current-table)))))))
      (sub-insert! list-of-keys local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

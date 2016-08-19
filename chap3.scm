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

;;;;;;;;;;;;;
;; Ex 3.26 ;;

(define tab-bin '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree key-val-pair left right)
  (list key-val-pair left right))

(define (adjoin-set key-val-pair existing-records)
  (cond ((null? existing-records) (make-tree key-val-pair '()'()))
        ((= (car key-val-pair) (car (entry existing-records)))
         (set-cdr! (entry existing-records) (cdr key-val-pair)))
        ((< (car key-val-pair) (car (entry existing-records)))
         (make-tree (entry existing-records)
                    (adjoin-set key-val-pair (left-branch existing-records))
                    (right-branch existing-records)))
        ((> (car key-val-pair) (car (entry existing-records)))
         (make-tree (entry existing-records)
                    (left-branch existing-records)
                    (adjoin-set key-val-pair (right-branch existing-records))))))

(define (assoc-bin key records)
  (cond ((null? records) #f)
        ((= key (car (entry records))) (entry records))
        ((< key (car (entry records))) (assoc-bin key (left-branch records)))
        ((> key (car (entry records))) (assoc-bin key (right-branch records)))))

(define (make-table-bin)
  (let ((tab-bin '()))
    (define (lookup-bin key)
      (let ((record (assoc-bin key tab-bin)))
        (if record
            (cdr record)
            #f)))
    (define (insert-bin! key val)
      (let ((record (assoc-bin key tab-bin)))
        (if record
            (set-cdr! record val)
            (set! tab-bin (adjoin-set (cons key val)
                                      tab-bin)))))
    (define (print)
      (display tab-bin) (newline))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup-bin)
            ((eq? m 'insert!) insert-bin!)
            ((eq? m 'print) (print))
            (else (error "Unknown operation - BINARY TABLE" m))))
    dispatch))

;;;;;;;;;;;;;
;; Ex 3.27 ;;

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x  result table)
              result))))))

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electrical circuits ;;;;;;;;;;;;;;;;;;;;;;;

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-proc)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc)
  'ok)

(define (valid-signal? s)
  (and (number? s)
       (or (= s 0)
           (= s 1))))

(define (check-signals . sigs)
  (cond ((null? sigs) #t)
        ((valid-signal? (car sigs))
         (apply check-signals (cdr sigs)))
        (else #f)))

(define (logical-and s1 s2)
  (if (check-signals s1 s2)
      (if (and (= s1 1) (= s2 1))
          1
          0)
      (error "Invalid signal" s1 s2)))

;;;;;;;;;;;;;
;; Ex 3.28 ;;

(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

(define (logical-or s1 s2)
  (if (check-signals s1 s2)
      (if (or (= s1 1) (= s2 1))
          1
          0)
      (error "Invalid signal s1 s2")))

;;;;;;;;;;;;;
;; Ex 3.29 ;;

(define (_or-gate a1 a2 output)
  (let ((n1 (make-wire))
        (n2 (make-wire))
        (s (make-wire)))
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 s)
    (inverter s output)
    'ok))

;; Delay time should be additive -> so 2x inverter (the first 2 are
;; parallel - so count only once). + 1x and-gate

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procs)
  (if (null? procs)
      'done
      (begin ((car procs))
             (call-each (cdr procs)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define (make-agenda) (list 0))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              rest))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint propagation ;;;;;

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradicttion" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception proc list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (proc (car items))
                (loop (cdr items)))))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1)
                          (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum)
                          (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum)
                          (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

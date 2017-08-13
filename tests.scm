;; -*-scheme-*-

(define-macro (check expr arrow expect)
  (list 'begin
        (list 'pretty-print (list 'quote expr))
        (list 'display (list 'quote arrow))
        '(newline)
        (let ((tmp (gensym))
              (tmp2 (gensym)))
          (list 'let* (list (list tmp expr)
                            (list tmp2 expect))
                (list 'pretty-print tmp)
                (list 'if (list 'not (list 'equal? tmp tmp2))
                      (list 'begin
                            (list 'display "Wrong result! Expected:\n")
                            (list 'pretty-print tmp2)))
                '(newline)))))

(print "Some conscheme tests running...")
(print "command line: " (command-line))
(print "The fixnum range is " (least-fixnum) " - " (greatest-fixnum))
(print "Various checks:")
(check (string-length "a") => 1)
(check (make-string 4 #\x) => "xxxx")
(check (string-length (make-string 4)) => 4)
(check (+ 1 2) => 3)
(check (+ 1 -2) => -1)
(check (+ (greatest-fixnum) (least-fixnum)) => -1)
;; (check (/ 1 3) => 1/3)
(check (/ -4 2) => -2)
(check (- 1 3) => -2)
(check (list (< 1 2) (= 1 2) (> 1 2)) => '(#t #f #f))
;; (check (list? (list 42.42 1.3+1.3i -1.0i +1.0i 2+3i 1/3+2/3i))
;;        => #t)
(check (bytevector? #vu8(1 2 3)) => #t)

(print "current input port: " (current-input-port))
(print "current output port: " (current-output-port))
(check (input-port? (current-input-port)) => #t)
(check (input-port? (current-output-port)) => #f)
(check (output-port? (current-input-port)) => #f)
(check (output-port? (current-output-port)) => #t)
(check (list? '(1 2 3 4)) => #t)
(check (let ((x (cons 1 '())))
         (set-cdr! x x)
         (list? x)) => #f)
(check (list? '(1 2 3 . 4) ) => #f)
(check (apply list 1 2 '(3 4)) => '(1 2 3 4))
(check (apply + '(1 2)) => 3)
(check (apply symbol? '(primitive-procedure-test)) => #t)
(check (apply (lambda (x y . z) (vector x y z)) 1 2 '(3 4)) => '#(1 2 (3 4)))
(check (apply (lambda (x y . z) (vector x y z)) '(1 2)) => '#(1 2 ()))
(check (apply apply (list list (list 'apply 'list))) => '(apply list))
(check (let* ((SYMBOL? symbol?) (x (lambda (y) (SYMBOL? y)))) (x 'y)) => #t)

(check (let* ((x 'x)
              (xs (symbol->string x)))
         (string-set! xs 0 #\g)
         (symbol->string x))
       => "x")

;; Some tests from the R6RS
(check `(list ,(+ 1 2) 4) => '(list 3 4))

(check (let ((name 'a)) `(list ,name ',name))
       => '(list a (quote a)))

(check `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
       => '(a 3 4 5 6 b))

(check `(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
       => '((foo 7) . cons))

(define (sqrt x) x)                     ;FIXME
(check `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
       => '#(10 5 2 4 3 8))

;; These are also from R6RS, but are not working. This is because
;; (unquote <expression>) and (unquote-splicing <expression>) are
;; changed (unquote <expression> ...) and (unquote-splicing
;; <expression> ...) in the R6RS.

;; (check (let ((name 'foo))
;;          `((unquote name name name)))
;;        => '(foo foo foo))

;; (check (let ((name '(foo)))
;;          `((unquote-splicing name name name)))
;;        => '(foo foo foo))

;; (check (let ((q '((append x y) (sqrt 9))))
;;           ``(foo ,,@q))
;;        => '`(foo
;;              (unquote (append x y) (sqrt 9))))

;; (check (let ((x '(2 3))
;;               (y '(4 5)))
;;           `(foo (unquote (append x y) (sqrt 9))))
;;        => '(foo (2 3 4 5) 3))

;; threading

(define (thread-test)
  (let ((t ($make-thread
            (lambda ()
              (print "THREAD 2"))
            "thread")))
    (print "THREAD 1")
    (thread-start! t)))

;; FIXME: this should run on multiple cores, but for some reason our
;; goroutines only use one core
(parallel-map (lambda (x)
                (do ((i 0 (+ i 1)))
                    ((= i 300000)))
                (cons 'something x))
              '(1 2 3 4 5 6 7 8 9 10))

;; call/cc

(check (call/cc (lambda (k) 1)) => 1)
(check (call/cc (lambda (k) (k 1))) => 1)
(check (call/cc (lambda (k) (k 1) 2)) => 1)

(check (call-with-current-continuation  ;r6rs
         (lambda (exit)
           (for-each (lambda (x)
                       (if (negative? x)
                           (exit x)))
                     '(54 0 37 -3 245 19))
           #t)) => -3)

(define list-length                     ;r6rs
  (lambda (obj)
    (call-with-current-continuation
      (lambda (return)
         (letrec ((r
                   (lambda (obj)
                     (cond ((null? obj) 0)
                           ((pair? obj)
                            (+ (r (cdr obj)) 1))
                           (else (return #f))))))
           (r obj))))))

(check (list-length '(1 2 3 4)) => 4)

(check (let ((log '()))
         (call/cc
           (lambda (k)
             (dynamic-wind
               (lambda () (set! log (cons 'before log)))
               (lambda () (set! log (cons 'thunk log)))
               (lambda () (set! log (cons 'after log))))))
         (reverse log))
       => '(before thunk after))

(check (let ((log '()))
         (call/cc
           (lambda (k)
             (dynamic-wind
               (lambda () (set! log (cons 'before log)))
               (lambda () (k 0) (set! log (cons 'thunk log)))
               (lambda () (set! log (cons 'after log))))))
         (reverse log))
       => '(before after))

(check (let ((path '())                     ;r6rs
             (c #f))
         (let ((add (lambda (s)
                      (set! path (cons s path)))))
           (dynamic-wind
             (lambda () (add 'connect))
             (lambda ()
               (add (call-with-current-continuation
                      (lambda (c0)
                        (set! c c0)
                        'talk1))))
             (lambda () (add 'disconnect)))
           (if (< (length path) 4)
               (c 'talk2)
               (reverse path))))
       => '(connect talk1 disconnect
                    connect talk2 disconnect))

(check (let ((n 0))                     ;r6rs
         (call-with-current-continuation
           (lambda (k)
             (dynamic-wind
               (lambda  ()
                 (set!  n (+ n 1))
                 (k))
               (lambda  ()
                 (set!  n (+ n 2)))
               (lambda  ()
                 (set!  n (+ n 4))))))
         n)  => 1)

(check (let ((n 0))                     ;r6s
         (call-with-current-continuation
           (lambda (k)
             (dynamic-wind
               values
               (lambda ()
                 (dynamic-wind
                   values
                   (lambda ()
                     (set! n (+ n 1))
                     (k))
                   (lambda ()
                     (set! n (+ n 2))
                     (k))))
               (lambda ()
                 (set! n (+ n 4))))))
         n) => 7)

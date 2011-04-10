;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (C) 2011 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; Standard library for conscheme.

;;; Equivalence predicates

(define (eqv? x y)
  (or (eq? x y)
      (if (and (number? x) (number? y))
          (= x y)
          #f)))

(define (equal? x y)
  (cond ((and (pair? x) (pair? y))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((and (string? x) (string? y))
         (string=? x y))
        ((and (vector? x) (vector? y)
              (= (vector-length x) (vector-length y)))
         (let lp ((i (- (vector-length x) 1)))
           (cond ((= i -1) #t)
                 ((not (equal? (vector-ref x i) (vector-ref y i)))
                  #f)
                 (else (lp (- i 1))))))
        (else (eqv? x y))))

;;; Numbers

(define (exact? z)
  (if (number? z)
      #t
      (error 'exact? "Bad type" z)))

(define (inexact? z) #f)

;; XXX: handle more arguments
(define (< x y) (eq? ($cmp x y) -1))
(define (> x y) (eq? ($cmp x y) 1))

;; <= >=

(define (zero? x) (= x 0))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))

(define (odd? x) (not (even? x)))

;; TODO: handle non-integers
(define (even? x)
  (zero? (modulo x 2)))

;; max min

(define (+ x y) ($+ x y))
;; *
(define (- x y) ($- x y))
(define (/ x y) ($/ x y))

;; (define (+ . rest)
;;   ;; wrapper around $+
;;   (let lp ((rest rest)
;;            (ret 0))
;;     (cond ((null? rest)
;;            ret)
;;           (else
;;            (lp (cdr rest)
;;                ($+ ret (car rest)))))))

(define (abs x)
  (if (negative? x)
      (- x)
      x))

;; quotient remainder modulo
;; gcd lcm
;; numerator denominator
;; floor ceiling truncate round
;; rationalize
;; exp log sin cos tan asin acos atan sqrt expt
;; make-rectangular make-polar
;; real-part imag-part magnitude angle
;; exact->inexact inexact->exact

(define (number->string num . rest)
  (cond ((null? rest)
         ($number->string num 10))
        ((null? (cdr rest))
         (if (memv (car rest) '(2 8 10 16))
             ($number->string num (car rest))
             (error 'number->string "Unknown radix" (car rest))))
        (else
         (error 'number->string "Too many arguments" num rest))))

;; string->number

;;; Pairs

;; set-car! set-cdr!
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (caar (car x)))
(define (caadr x) (caar (cdr x)))
(define (cadar x) (cadr (car x)))
(define (caddr x) (cadr (cdr x)))
(define (cdaar x) (cdar (car x)))
(define (cdadr x) (cdar (cdr x)))
(define (cddar x) (cddr (car x)))
(define (cdddr x) (cddr (cdr x)))
(define (caaaar x) (caaar (car x)))
(define (caaadr x) (caaar (cdr x)))
(define (caadar x) (caadr (car x)))
(define (caaddr x) (caadr (cdr x)))
(define (cadaar x) (cadar (car x)))
(define (cadadr x) (cadar (cdr x)))
(define (caddar x) (caddr (car x)))
(define (cadddr x) (caddr (cdr x)))
(define (cdaaar x) (cdaar (car x)))
(define (cdaadr x) (cdaar (cdr x)))
(define (cdadar x) (cdadr (car x)))
(define (cdaddr x) (cdadr (cdr x)))
(define (cddaar x) (cddar (car x)))
(define (cddadr x) (cddar (cdr x)))
(define (cdddar x) (cdddr (car x)))
(define (cddddr x) (cdddr (cdr x)))

(define (null? x) (eq? x '()))

(define (list? x) (and (floyd x) #t))

(define (list . x) x)

;(define (length x) (or (floyd x) (error 'length "not a list")))

(define (append x y) 
  (let lp ((x (reverse x)) (y y))
	(if (null? y)
	  (reverse x)
	  (lp (cons (car y) x) (cdr y)))))

(define (reverse l)
  (let lp ((l l) (ret '()))
    (if (null? l)
        ret
        (lp (cdr l) (cons (car l) ret)))))

(define (list-tail x k)
  (if (zero? k)
      x
      (list-tail (cdr x) (- k 1))))

;; list-ref

(define (memq el list)
  (cond ((null? list) #f)
        ((eq? el (car list)) #t)
        (else (memq el (cdr list)))))

(define (memv el list)
  (cond ((null? list) #f)
        ((eqv? el (car list)) #t)
        (else (memv el (cdr list)))))

(define (member el list)
  (cond ((null? list) #f)
        ((equal? el (car list)) #t)
        (else (member el (cdr list)))))

(define (assq el list)
  (cond ((null? list) #f)
        ((eq? el (caar list)) (car list))
        (else (assq el (cdr list)))))

(define (assv el list)
  (cond ((null? list) #f)
        ((eqv? el (caar list)) (car list))
        (else (assv el (cdr list)))))

(define (assoc el list)
  (cond ((null? list) #f)
        ((equal? el (caar list)) (car list))
        (else (assoc el (cdr list)))))

;;; Symbols

;; symbol->string
;; string->symbol

;;; Characters

(define (char=? x y) (= (char->integer x) (char->integer y)))

;; char<? char>? char<=? char>=?
;; char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
;; char-alphabetic? char-numeric? char-whitespace?
;; char-upper-case? char-lower-case?
;; char->integer integer->char
;; char-upcase char-downcase

;;; Strings

;; string string-set!

(define (string=? x y)
  (and (= (string-length x) (string-length y))
       (let lp ((i (- (string-length x) 1)))
         (cond ((= i -1) #t)
               ((not (char=? (string-ref x i) (string-ref y i)))
                #f)
               (else (lp (- i 1)))))))

;; string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
;; string-ci<=? string-ci>=?
;; substring

(define (string-append . xs)
  (let lp ((strings xs) (len 0))
    (if (null? strings)
        (let ((ret (make-string len)))
          (let lp ((strings xs) (reti 0))
            (if (null? strings)
                ret
                (let* ((str (car strings))
                       (strlen (string-length str)))
                  (let lp* ((stri 0)
                            (reti reti))
                    (cond ((= stri strlen)
                           (lp (cdr strings) reti))
                          (else
                           (string-set! ret reti (string-ref str stri))
                           (lp* (+ stri 1) (+ reti 1)))))))))
        (lp (cdr strings) (+ len (string-length (car strings)))))))


(define (string->list str) 
  (let lp ((l '()) (i (- (string-length str) 1)))
	(if (= i 0)
	  l
	  (lp (cons (string-ref str k) l) (- i 1)))))

(define (list->string x)
  (let ((str (make-string (length x))))
    (let lp ((ref (- (length x) 1)) (x x))
	  (cond ((null? x) str) 
            ((not (char? (car x))) (error 'list->string "not a list of chars"))
            (else 
              (string-set! str ref (car x))
              (lp (- ref 1) (cdr x)))))))

(define (string-copy x)
  (let ((str (make-string (length x))))
    (let lp ((ref (- (string-length x) 1)))
      (cond ((< ref 0) str)
        (else 
          (string-set! str ref (string-ref x ref))
          (lp (- ref 1)))))))
;; string-copy string-fill!

(define (string-fill! str char)
  (let lp ((ref (- 1 (length str))))
    (cond ((< ref 0) str)
          (else
            (string-set! str ref char)
            (lp (- ref 1))))))
;;; Vectors

;; make-vector vector vector-set! vector->list list->vector
;; vector-fill!

;;; Control features

;; apply
;; map

;; FIXME: takes n>=1 lists
(define (for-each f l)
  (cond ((not (null? l))
         (f (car l))
         (for-each f (cdr l)))))

;; call-with-current-continuation call/cc
;; values call-with-values
;; dynamic-wind
;; eval scheme-report-environment null-environment
;; interaction-environment
;;

;;; Input and output

;; call-with-input-file
;; call-with-output-file

;; with-input-from-file wwith-output-to-file
;; open-input-file open-output-file
;; close-input-file close-output-file
;; read

(define (read-char . rest)
  (if (null? rest)
      ($read-char (current-input-port))
      ($read-char (car rest))))

;; peek-char

(define (eof-object? x) (eq? x (eof-object)))

;; char-ready?

;; TODO: write these in scheme and handle the port argument
(define (write obj . x)
  (if (null? x)
      ($write obj (current-output-port))
      ($write obj (car x))))

(define (display obj . x)
  (if (null? x)
      ($display obj (current-output-port))
      ($display obj (car x))))


(define (newline . x)
  (if (null? x)
      (write-char #\newline)
      (write-char #\newline (car x))))

(define (write-char c . rest)
  (if (null? rest)
      ($write-char c (current-output-port))
      ($write-char c (car rest))))

;; write-char

;; load
;; transcript-on transcript-off

;;; Misc

(define (error who why . irritants)
  (display "Error from ")
  (display who)
  (display ": ")
  (display why)
  (newline)
  (display "List of irrantants: ")
  (display irritants)
  (newline)
  ;; XXX: should tie in with some exception handling stuff
  (exit 1))

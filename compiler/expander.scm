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

;; Simple defmacro-like expander

;; The output from expand should be simplified Scheme code:
;; (define <variable> <expression>
;; (lambda <formals> (begin <expression> ...))
;; (if <test> <consequence> <alternate>)
;; (begin <expression> ...)
;; (<procedure> <argument> ...)
;; (quote <datum>)
;; (set! <variable> <expression>)
;; <variable>

;; It would be nice to write this code with quasiquote, etc, but then
;; we'd be forced to implement them, which I'm not sure we will have
;; time to do.

;;;
(define (lambda-formals x) (cadr x))

(define (lambda-body* x) (cddr x))      ;multi-expression lambda

(define (lambda-body x) (caddr x))      ;simplified lambda

(define (set!-name x) (cadr x))

(define (set!-expression x) (caddr x))

(define (if-test x) (cadr x))

(define (if-consequence x) (caddr x))

(define (if-alternative? x) (pair? (cdddr x)))

(define (if-alternative x) (cadddr x))

(define (define-macro-name x) (caadr x))

(define (define-macro-formals x) (cdadr x))

;;;

(define-macro (define-macro name/args . body)
   (list 'add-macro! (list 'quote (car name/args))
         (append (list 'lambda (cdr name/args))
                 body)))

(define-macro (cond-expand . tests)     ;SRFI-0
  (define (fullfilled? f)
    ;; XXX: does not support and, or and not
    (or (eq? f 'else)
        (memq f '(conscheme))))
  (let lp ((i tests))
    (cond ((null? i)
           (error 'cond-expand "No matching clause" tests))
          ((fullfilled? (caar i))
           (cons 'begin (cdar i)))
          (else
           (lp (cdr i))))))

(define-macro (let bindings . body)
  (if (symbol? bindings)
      ;; Named let
      (let ((name bindings)
            (bindings (car body))
            (body (cdr body)))
        (append (list (list 'letrec
                            (list (list name
                                        (append (list 'lambda (map car bindings))
                                                body)))
                            name))
                (map cadr bindings)))
      ;; Regular let
      (append (list (append (list 'lambda (map car bindings))
                            body))
              (map cadr bindings))))

(define-macro (let* bindings . body)
  (if (null? body)
      (error 'let* "Empty body" bindings body))
  (let lp ((b bindings))
    (if (null? b)
        (cons 'begin body)
        (let ((b1 (car b))
              (b (cdr b)))
          (list 'let (list b1)
                (lp b))))))

(define-macro (letrec bindings . body)
  (let ((temps (map (lambda (_) (gensym)) bindings)))
    (list 'let (map (lambda (binding) (list (car binding) '(if #f #f)))
                    bindings)
          (append (list 'let (map (lambda (temp binding) (list temp (cadr binding)))
                                  temps bindings))
                  (append (map (lambda (temp binding)
                                 (list 'set! (car binding) temp))
                               temps bindings)
                          body)))))

(define-macro (include filename)
  (call-with-input-file filename
    (lambda (p)
      (let lp ((datums '()))
        (let ((datum (read p)))
          (if (eof-object? datum)
              (cons 'begin (reverse datums))
              (lp (cons datum datums))))))))

(define-macro (case value . cases)
  (let ((tmp (gensym)))
    (list 'let (list (list tmp value))
          (cons 'cond
                (map (lambda (c)
                       (cond ((eq? (car c) 'else)
                              c)
                             (else
                              (list (list 'memv tmp (list 'quote (car c)))
                                    (cons 'begin (cdr c))))))
                     cases)))))

(define-macro (cond . tests)
  ;; TODO: does not support =>
  ;; TODO: does not support (cond (expr))
  (let lp ((tests tests))
    (if (null? tests)
        (list 'unspecified)
        (let ((test (car tests))
              (tests (cdr tests)))
          (if (eq? (car test) 'else)
              (if (null? tests)
                  (cons 'begin (cdr test))
                  (error 'cond "else must be the last test" tests))
              (list 'if
                    (car test)
                    (cons 'begin (cdr test))
                    (lp tests)))))))

(define-macro (or . arms)
  (let lp ((arms arms))
    (cond ((null? arms) #f)
          ((null? (cdr arms))
           (car arms))
          (else
           (let ((tmp (gensym)))
             (list 'let (list (list tmp (car arms)))
                   (list 'if tmp tmp (lp (cdr arms)))))))))

(define-macro (and . arms)
  (let lp ((arms arms))
    (cond ((null? arms) #t)
          ((null? (cdr arms))
           (car arms))
          (else
           (list 'if (car arms) (lp (cdr arms)) #f)))))

;; TODO: do
;; TODO: quasiquote

(define (formals-to-list x)
  (cond ((null? x) x)
        ((symbol? x) (list x))
        (else (cons (car x) (formals-to-list (cdr x))))))

(define (exp-new-env) '())

(define (exp-extend-env formals env)
  (append (formals-to-list formals) env))

(define (begin-wrap body)
  ;; For wrapping a body in begin, unless it was already wrapped in begin
  (if (eq? (car body) 'begin)
      body
      (cons 'begin body)))

(define (expand* x env)
  (define (expand** x env)
    (let ((expander (assq (car x) *macros*)))
      (if expander
          (expand* (apply (cdr expander) (cdr x)) env)
          (case (car x)
            ((lambda)
             ;; TODO: transform define into letrec
             (if (null? (lambda-body* x))
                 (error 'expand "Missing lambda body" x))
             (list 'lambda (lambda-formals x)
                   (let ((newenv (exp-extend-env (lambda-formals x) env)))
                     (expand* (begin-wrap (lambda-body* x)) newenv))))
            ((if)
             (list 'if
                   (expand* (if-test x) env)
                   (expand* (if-consequence x) env)
                   (expand* (if (if-alternative? x)
                                (if-alternative x)
                                '(unspecified))
                            env)))
            ((quote) x)
            ((define)
             (cond ((pair? (cadr x))
                    ;; (define (name . formals) body)
                    (expand* (list 'define (caadr x)
                                   (append (list 'lambda (cdadr x)) (cddr x)))
                             env))
                   (else
                    ;; (define name expr)
                    (if (not (symbol? (cadr x)))
                        (error 'expand "Syntax error" x))
                    (list 'define (cadr x) (expand* (caddr x) env)))))
            ((begin)
             (cons 'begin (map-in-order (lambda (x) (expand* x env)) (cdr x))))
            ((set!)
             (list 'set! (set!-name x) (expand* (set!-expression x) env)))
            ((define-macro)
             (add-macro! (define-macro-name x)
                         (eval (append (list 'lambda (define-macro-formals x))
                                       (cddr x))
                               (interaction-environment)))
             '(unspecified))
            (else
             ;; Probably a procedure call
             (if (list? x)
                 (map (lambda (x) (expand* x env)) x)
                 (error 'expand "Syntax error" x)))))))
  (cond ((symbol? x) x)
        ((or (char? x) (number? x) (boolean? x) (string? x) (bytevector? x))
         (list 'quote x))
        ((vector? x)
         (error 'expand "Invalid expression: vectors must be quoted" x))
        ((pair? x)
         (expand** x env))
        (else
         (error 'expand "Invalid syntax" x))))

(define (expand x)
  (expand* x (exp-new-env)))

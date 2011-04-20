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

;; Free variable analysis. Also restores let expressions and assigns
;; names to lambdas. This analysis comes in handy when analysing
;; closures.

;; The input is from the primops pass. The output adds $ann-lambda and
;; let forms.

(define (new-name-ctx define-name)
  ;; This is perhaps not the best approach to assigning names to
  ;; lambdas, but it should be ok for us.
  (let ((c 0))
    (lambda ()
      (set! c (+ c 1))
      (if (= c 1)
          define-name
          (string->symbol
           (string-append (symbol->string define-name) ":" (number->string c)))))))

(define (freevar-find x to-find)
  (if (symbol? x)
      (if (memq x to-find) (list x) '()) ;found a free variable
      (case (car x)
        ((lambda)
         (freevar-find (lambda-body x) to-find))
        ((if begin $primcall $funcall)
         (append-map (lambda (x) (freevar-find x to-find)) (cdr x)))
        ((quote $primitive) '())
        ((define)
         (freevar-find (caddr x) to-find))
        ((set!)
         (if (memq (set!-name x) to-find)
             (cons (set!-name x)
                   (freevar-find (set!-expression x) to-find))
             (freevar-find (set!-expression x) to-find)))
        (else
         (error 'freevar-find "Bad expression" x)))))

(define (freevar* x env name-ctx)
  (if (symbol? x)
      x
      (case (car x)
        ((lambda)
         (let* ((new-env (append (formals-to-list (lambda-formals x)) env))
                (name (name-ctx))
                (body (freevar* (lambda-body x) new-env name-ctx))
                (free-vars (delete-duplicates (freevar-find (lambda-body x) env))))
           (list '$ann-lambda (lambda-formals x)
                 name free-vars
                 body)))
        ((quote $primitive) x)
        ((define)
         (list 'define (cadr x) (freevar* (caddr x) env
                                          (new-name-ctx (cadr x)))))
        ((set!)
         (list 'set! (set!-name x) (freevar* (set!-expression x) env
                                             (new-name-ctx (set!-name x)))))
        ((if begin $primcall)
         (cons (car x) (map (lambda (x) (freevar* x env name-ctx))
                            (cdr x))))
        (($funcall)
         (let ((op (cadr x)))
           ;; Tries to restore let expressions
           (if (and (pair? op) (eq? (car op) 'lambda)
                    (list? (lambda-formals op))
                    (= (length (lambda-formals op))
                       (length (cddr x))))
               (let ((new-env (append (formals-to-list (lambda-formals op)) env)))
                 (list 'let (map list (lambda-formals op)
                                 (map (lambda (x) (freevar* x env name-ctx))
                                      (cddr x)))
                       (freevar* (lambda-body op) new-env name-ctx)))
               (cons '$funcall (map (lambda (x) (freevar* x env name-ctx))
                                    (cdr x))))))
        (else
         (error 'freevar* "Bad expression" x)))))

(define (freevar x)
  (freevar* x '() (new-name-ctx 'top-level)))

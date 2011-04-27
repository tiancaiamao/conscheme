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

;; Generates byte code. The input comes from the closure analysis pass.

(define (cg-reg-gen start)
  (let ((r 0))
    (lambda ()
      (let ((ret r))
        (set! r (+ r 1))
        (list 'reg ret)))))

(define (cg-new-state)
  (let ((env '())
        (reg (cg-reg-gen 0))
        (label (let ((r 0))
                 (lambda ()
                   (let ((ret r))
                     (set! r (+ r 1))
                     (string->symbol (string-append "L" (number->string ret)))))))
        (constpool (let ((consts '())
                         (i 0))
                     (lambda x
                       (if (null? x)
                           (map car consts)
                           (let ((old (assoc (car x) consts)))
                             (if old
                                 (cdr old)
                                 (let ((idx i))
                                   ;; (print "#;const " i " = " (car x))
                                   (set! consts (cons (cons (car x) i) consts))
                                   (set! i (+ idx 1))
                                   i)))))))
        (frame #f))
    (vector env reg label constpool frame)))

(define (cg-env s)
  (vector-ref s 0))

(define (cg-reg s)
  ((vector-ref s 1)))

(define (cg-label s)
  ((vector-ref s 2)))

(define (cg-extend-env s env*)
  (vector (append env* (vector-ref s 0))
          (vector-ref s 1)
          (vector-ref s 2)
          (vector-ref s 3)
          (vector-ref s 4)))

(define (cg-code-env s env* reg-start)
  (vector env*
          (cg-reg-gen reg-start)
          (vector-ref s 2)
          (vector-ref s 3)
          #f))

(define (cg-lookup name emit s)
  ;; (print "#;cg-lookup " name " in " (cg-env s))
  (let ((v (assq name (cg-env s))))
    (if v
        (case (cadr v)
          ((closure)
           (let ((reg (cg-reg s)))
             (emit (list 'closure-ref reg (cddr v)))
             reg))
          ((local)
           ;; (print "#;local" v)
           (cddr v))
          (else
           (error 'cg-lookup "Unknown variable" v)))
        (let ()
          ;; XXX: convert to primitive before this pass
          (emit (list 'globalref 'fixme name))
          'ERROR))))

(define (cg-mutate name reg emit s)
  ;; (print "#;cg-mutate " name " in " (cg-env s))
  (let ((r (cg-reg s)))
    (let ((v (assq name (cg-env s))))
      (if v
          (case (cadr v)
            ((closure)
             (emit (list 'closure-set! reg (cddr v) reg)))
            ((local)
             (emit (list 'move (cddr v) reg)))
            (else
             (error 'cg-mutate "Unknown variable" v)))
          (let ()
            ;; XXX: convert to primitive before this pass
            (emit (list 'globalset 'fixme name)))))
    (emit (list 'make-void r))
    r))

(define (cg-const value emit s)
  (let ((reg (cg-reg s)))
    ;; TODO: load small immediate constants directly from the
    ;; instruction
    (emit (list 'const-ref reg ((vector-ref s 3) value)))
    reg))

(define (cg-const-pool s)
  (reverse ((vector-ref s 3))))

(define (cg-primcall name ai emit s)
  ;; TODO: include primitives as instructions. they can be
  ;; included in the bytecode vm main loop.
  ;; TODO: assign numbers to all primitives
  (let ((idx (case name
               ((cons) 0)
               (else -1)
               #;(else (error 'cg-primitive "Unknown primitive" name)))))
    (let ((r (cg-reg s)))
      (emit (list 'primcall r idx ai))
      r)))

(define (cg-primitive name emit s)
  ;; TODO: does not work.
  (let ((idx (case name
               ((cons) 0)
               (else -1)
               #;(else (error 'cg-primitive "Unknown primitive" name)))))
    (let ((r (cg-reg s)))
      (emit (list 'primref r idx))
      r)))

(define (cg-frame-start emit s)
  ;; emit a instruction that will be modified by cg-frame-end
  (let ((frame (list 'frame 'n)))
    (vector-set! s 4 frame)
    (emit frame)))

(define (cg-frame-end s)
  ;; record the number of the highest used register
  (set-car! (cdr (vector-ref s 4))
            (cg-reg s)))

(define (codegen* x emit s tail?)
  ;; (display (list 'codegen* x)) (newline)
  (if (symbol? x)
      (cg-lookup x emit s)
      (case (car x)
        (($labels)
         ;; generate codes and body
         ;; extend env with both closure and argument variables
         (cg-frame-start emit s)
         (emit (list 'return (codegen* (caddr x) emit s tail?)))
         (cg-frame-end s)
         (for-each (lambda (x)
                     #; (print "#;label " x)
                     (let ((label (car x))
                           (code (cadr x)))
                       (emit (list 'label label))
                       (let ((formals (cadr code))
                             (closure-vars (caddr code))
                             (body (cadddr code)))
                         (let lp ((formals formals)
                                  (env '())
                                  (fi 0))
                           (cond ((null? formals)
                                  (let lp ((closure-vars closure-vars)
                                           (env env)
                                           (ci 0))
                                    (if (null? closure-vars)
                                        (let ((s* (cg-code-env s env fi)))
                                          (cg-frame-start emit s*)
                                          (let ((ret (codegen* body emit s* #t)))
                                            (emit (list 'return ret))
                                            (cg-frame-end s*)
                                            'unused))
                                        (lp (cdr closure-vars)
                                            (cons (cons (car closure-vars)
                                                        (cons 'closure ci))
                                                  env)
                                            (+ ci 1)))))
                                 ((pair? formals)
                                  (lp (cdr formals)
                                      (cons (cons (car formals)
                                                  (cons 'local (list 'reg fi)))
                                            env)
                                      (+ fi 1)))
                                 (else
                                  (emit (list 'consargs fi))
                                  (lp '()
                                      (cons (cons formals
                                                  (cons 'local (list 'reg fi)))
                                            env)
                                      (+ fi 1))))))))
                   (cadr x)))
        (($closure)
         (let ((r (cg-reg s))
               (name (cadr x))
               (label (caddr x))
               (closure-vars (cdddr x)))
           #; (print "#;closure " (cdr x))
           (emit (list 'closure r (length closure-vars)))
           (emit (list 'closure.name r (cg-const name emit s)))
           (emit (list 'closure.label r label))
           (let lp ((vars closure-vars)
                    (ci 0))
             (cond ((null? vars) r)
                   (else
                    (emit (list 'closure.var r ci (codegen* (car vars) emit s #f)))
                    (lp (cdr vars) (+ ci 1)))))))
        ((let)
         ;; Extend the environment with new bindings
         (let ((regs (map (lambda (bind)
                            (let ((lhs (car bind))
                                  (rhs (cadr bind)))
                              (cons lhs (cons 'local (codegen* rhs emit s #f)))))
                          (cadr x))))
           #; (print "#;regs " regs)
           (codegen* (caddr x) emit
                     (cg-extend-env s regs)
                     tail?)))
        ((quote)
         (cg-const (cadr x) emit s))
        (($primitive)
         (cg-primitive (cadr x) emit s))
        ((set! define)
         ;; FIXME: define should be global-set! instead
         (cg-mutate (cadr x)
                    (codegen* (caddr x) emit s #f)
                    emit s))
        ((if)
         (let ((test (cadr x))
               (consequence (caddr x))
               (alternative (cadddr x)))
           (let* ((alt (cg-label s))
                  (exit (cg-label s)))
             (let ((vreg (codegen* test emit s #f))
                   (rreg (cg-reg s)))
               (emit (list 'bf vreg alt))
               (emit (list 'move rreg (codegen* consequence emit s tail?)))
               (emit (list 'jump exit))
               (emit (list 'label alt))
               (emit (list 'move rreg (codegen* alternative emit s tail?)))
               (emit (list 'label exit))
               rreg))))
        ((begin)
         (let lp ((body (cdr x))
                  (reg #f))
           (cond ((null? body) reg)
                 (else
                  (lp (cdr body)
                      (codegen* (car body) emit s
                                (and tail? (null? (cdr body)))))))))
        (($funcall $primcall)
         ;; Call to a primitive, a tail-call or a normal call.
         (let lp ((args (cddr x))
                  (ai 0))
           (cond ((null? args)
                  (if (eq? (car x) '$primcall)
                      (cg-primcall (cadr x) ai emit s)
                      (let ((f (codegen* (cadr x) emit s #f)))
                        (cond (tail?
                               (emit (list 'tailcall f ai))
                               'unused)
                              (else
                               (let ((r (cg-reg s)))
                                 (emit (list 'funcall r f ai))
                                 r))))))
                 (else
                  (emit (list 'push (codegen* (car args) emit s #f)))
                  (lp (cdr args)
                      (+ ai 1))))))
        (else
         (error 'codegen* "Bad expression" x)))))

(define (codegen x)
  (let* ((code '())
         (emit (lambda (expr)
                 ;; (display "#;d ")
                 ;; (display expr)
                 ;; (newline)
                 (set! code (cons expr code))))
         (s (cg-new-state)))
    (codegen* x emit s #f)
    (vector (reverse code)
            (cg-const-pool s))))

;; (pretty-print
;;  (codegen
;;   (closures
;;    (compile-expression
;;     '(lambda (x) (if x #f #t))
;;     #;'(include "main.scm")))))

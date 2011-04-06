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

(include "defmacro.scm")
(cond-expand
 (conscheme (include "library.scm"))
 (else #f))
(include "primitives.scm")
(include "aconv.scm")
(include "serialize.scm")
(include "expander.scm")
;; (include "codegen.scm")

;; See what happens...
(cond-expand
 (guile (use-modules (ice-9 pretty-print)))
 (else (define (pretty-print x)
         (write x)
         (newline))))

(define (compile)
  (let ((output-file "main.cso")
        (input-file "main.scm"))
    (let ((code (primops (aconv (expand (list 'include input-file))))))
      (if (member "print" (command-line))
          (pretty-print code))
      (if (file-exists? output-file)
          (delete-file output-file))
      (call-with-port (open-file-output-port output-file)
        (lambda (p)
          (serialize-object code p))))))

(define (print . x) (for-each (lambda (x) (display x)) x) (newline))

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

(cond-expand
 (conscheme
  (print "conscheme running")
  (print "Some tests:")
  (print "command line: " (command-line))
  (print "The fixnum range is " (least-fixnum) " - " (greatest-fixnum))

  (check (string-length "a") => 1)
  ;;(check (make-string 4 #\x) => "xxxx")
  (check (string-length (make-string 4)) => 4)
  (check (+ 1 2) => 3)
  (check (+ 1 -2) => -1)
  (check (+ (greatest-fixnum) (least-fixnum)) => -1)
  (check (/ 1 3) => 1/3)
  (check (- 1 3) => -2)
  )
 (else #F))

(cond ((member "compile" (command-line))
       (compile))
      ((member "genprim" (command-line))
       (print-operations (current-output-port)))
      (else
       (display "Usage: main compile|genprim.\n")
       (exit 1)))

;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright (C) 2011 Göran Weinholt <goran@weinholt.se>
;; Copyright (C) 2011 Per Odlund <per.odlund@gmail.com>

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

;; Definitions for primitive operations

(define *primitives* '())
(define *operations* '())

(define (formals-required formals)
  ;; Takes a list of formal arguments from a define-primitive and
  ;; returns how many arguments are required in order to run the
  ;; primitive.
  (let lp ((formals formals) (req 0))
    (cond ((or (null? formals) (symbol? formals)) req)
          (else
           (lp (cdr formals) (+ req 1))))))

(define (formals-satisfied formals args)
  (let lp ((formals formals)
           (args args))
    (cond ((and (null? formals) (null? args))
           #t)
          ((symbol? formals) #t)
          ((and (pair? formals) (null? args))
           #f)
          (else
           (lp (cdr formals) (cdr args))))))

;; Primitives check the argument list of the primitive and then return
;; a operation name.
(define (add-primitive! name proc)
  (set! *primitives* (cons (cons name proc)
                           *primitives*)))

(define-macro (define-primitive name/args)
  (list 'add-primitive! (list 'quote (car name/args))
        (list 'quote (cdr name/args))))

;; Operations are written in Go and are used to implement the primitives.
(define (add-operation! name proc)
  (set! *operations* (cons (cons name proc)
                           *operations*)))

(define-macro (define-operation name . body)
  (list 'add-operation! (list 'quote name)
        (append (list 'lambda '())
                body)))

(define-macro (define-call opname funcname args)
   (list 'begin
         (list 'define-operation opname (list 'normal-call funcname args))
         (list 'define-primitive (cons opname (make-list args 'arg)))))

(define (shift-args) "args = args[1:]")
(define (argn n)
  (string-append "args[" (number->string n) "]"))
(define (all-args)
  "args")
(define (arglen)
  "len(args)")
(define (ct)
  ;; We have no thread-local variables, so current-thread is passed
  ;; around like this.
  "ct")

(define (normal-call funcname args)
  (let lp ((i 0) (formals ""))
    (if (= i args)
        (list (string-append "return " funcname "(" formals ")"))
        (lp (+ i 1)
            (string-append formals (if (positive? i) ", " "") (argn i))))))

(define (print-operations p)
  (display "// This file is part of conscheme\n" p)
  (display "// Automatically generated by compiler/primitives.scm\n" p)
  (display "package conscheme\n" p)
  (display "import \"fmt\"\n" p)
  (display "import \"os\"\n" p)
  (display "var primitives map[string]Obj = make(map[string]Obj)\n" p)
  (display "func init() {\n" p)
  (for-each (lambda (prim)
              (let ((required (formals-required (cdr prim))))
                (display (string-append
                          "\tprimitives[\"" (symbol->string (car prim)) "\"] = "
                          "wrap(Procedure{name:\"" (symbol->string (car prim)) "\","
                          "required:" (number->string required) ","
                          "apply:apprim"
                          "})\n"))))
            *primitives*)
  (display "}\n\n" p)
  (display "func evprim(primop string, args []Obj, ct Obj) Obj {\n" p)
  (display "\tswitch primop {\n" p)
  (for-each (lambda (op)
              (display (string-append "\tcase \"" (symbol->string (car op)) "\":\n") p)
              (for-each (lambda (line)
                          (display (string-append "\t\t" line "\n") p))
                        ((cdr op))))
            *operations*)
  (display "\tdefault:\n" p)
  (display "\t\tfmt.Fprintf(os.Stderr, \"Please regenerate primitives.go\\n\")\n" p)
  (display "\t\tpanic(fmt.Sprintf(\"Unimplemented primitive: %s\",primop))\n" p)
  (display "\t}\n" p)
  (display "\tpanic(fmt.Sprintf(\"Fell off the edge in evprim(): %s\",primop))\n" p)
  (display "}\n" p))

;; Booleans

(define-call boolean? "boolean_p" 1)
(define-call not "not" 1)

;; Pairs

(define-call pair? "pair_p" 1)
(define-call cons "Cons" 2)
(define-call car "car" 1)
(define-call cdr "cdr" 1)
(define-call floyd "Floyd" 1)
(define-call length "Length" 1)
(define-call set-car! "set_car_ex" 2)
(define-call set-cdr! "set_cdr_ex" 2)

(define-operation null?
  (list (string-append "if " (argn 0) " ==  Eol {")
        "\treturn True"
        "} else {"
        "\treturn False"
        "}"))
(define-primitive (null? x))

;; Symbols

(define-call symbol? "symbol_p" 1)
(define-call symbol->string "Symbol_to_string" 1)
(define-call string->symbol "String_to_symbol" 1)

;; Characters

(define-call char? "char_p" 1)
(define-call char->integer "char_to_integer" 1)
(define-call integer->char "integer_to_char" 1)
(define-call char-whitespace? "char_whitespace_p" 1)
(define-call char-upcase "char_upcase" 1)
(define-call char-downcase "char_downcase" 1)

;; Vectors

(define-call vector? "vector_p" 1)
(define-call make-vector "Make_vector" 2) ;TODO: 2nd arg is optional
(define-call vector-length "vector_length" 1)
(define-call vector-ref "Vector_ref" 2)
(define-call vector-set! "Vector_set_ex" 3)

(define-operation vector
  (list (string-append "return wrap(" (all-args) ")")))
(define-primitive (vector . el))

;; Numbers

(define-call number? "number_p" 1)
(define-call integer? "integer_p" 1)
(define-call $number->string "_number_to_string" 2)
(define-call $string->number "_string_to_number" 2)
(define-call $+ "number_add" 2)
(define-call $/ "number_divide" 2)
(define-call $- "number_subtract" 2)
(define-call $cmp "number_cmp" 2)
(define-call denominator "denominator" 1)
(define-call $bitwise-ior "bitwise_ior" 2)
(define-call $bitwise-and "bitwise_and" 2)
(define-call bitwise-arithmetic-shift-right "bitwise_arithmetic_shift_right" 2)

(define-operation least-fixnum (list "return Make_fixnum(fixnum_min)"))
(define-primitive (least-fixnum))

(define-operation greatest-fixnum (list "return Make_fixnum(fixnum_max)"))
(define-primitive (greatest-fixnum))

;; Strings

(define-call string? "string_p" 1)
(define-call string-length "String_length" 1)
(define-call string-ref "String_ref" 2)
(define-call string-set! "String_set_ex" 3)

(define-operation make-string
  (list
   (string-append "switch " (arglen) " {")
   (string-append "default: return Make_string(" (argn 0) "," (argn 1) ")")
   (string-append "case 1: return Make_string(" (argn 0)
                  ",Make_char(" (number->string (char->integer #\space)) "))")
   "}"))
(define-primitive (make-string len . opt)) ;TODO: 1 optional argument

;; Misc

(define-operation apply
  (list (string-append "return apply(" (all-args) "," (ct) ")")))
(define-primitive (apply fun . args))

(define-call procedure? "procedure_p" 1)

(define-operation unspecified (list "return Void"))
(define-primitive (unspecified))

(define-operation eof-object (list "return Eof"))
(define-primitive (eof-object))

(define-operation eq?
  (list (string-append "if " (argn 0) " == " (argn 1) " {")
        "\treturn True"
        "} else {"
        "\treturn False"
        "}"))
(define-primitive (eq? x y))

(define-operation exit
  (list (string-append "os.Exit(number_to_int(" (argn 0) "))")))
(define-primitive (exit status))

(define-call command-line "Command_line" 0)

(define-call $eval "Eval" 1)

;; Cells, internal to the compiler, used for mutation. TODO: should
;; not be callable by the user.

(define-operation $make-cell
  (list "var v [1]Obj"
        (string-append "v[0] = " (argn 0))
        "var vv interface{} = &v"
        "return Obj(&vv)"))
(define-primitive ($make-cell init))

(define-operation $cell-ref
  (list (string-append "v := " (argn 0))
        "vv := (*v).(*[1]Obj)"
        "return vv[0]"))
(define-primitive ($cell-ref cell))

(define-operation $cell-set!
  (list (string-append "v := " (argn 0))
        "vv := (*v).(*[1]Obj)"
        (string-append "vv[0] = " (argn 1))
        "return Void"))
(define-primitive ($cell-set! cell value))

;; Bytevectors

(define-call bytevector? "bytevector_p" 1)
(define-call bytevector-length "bytevector_length" 1)
(define-call u8-list->bytevector "u8_list_to_bytevector" 1)
(define-call string->utf8 "string_to_utf8" 1)

;; I/O

(define-call port? "port_p" 1)
(define-call input-port? "input_port_p" 1)
(define-call output-port? "output_port_p" 1)
(define-call current-input-port "current_input_port" 0)
(define-call current-output-port "current_output_port" 0)

(define-call file-exists? "file_exists_p" 1)
(define-call delete-file "delete_file" 1)
(define-call open-input-file "open_input_file" 1)
(define-call open-file-output-port "open_file_output_port" 1) ;TODO: three more arguments
(define-call close-input-port "close_input_port" 1)
(define-call close-output-port "close_output_port" 1)
(define-call close-port "close_port" 1)

(define-call $read-char "_read_char" 1)
(define-call $peek-char "_peek_char" 1)
(define-call $write-char "_write_char" 2)

(define-call get-u8 "get_u8" 1)
(define-call put-u8 "put_u8" 2)
(define-call put-bytevector "put_bytevector" 2)
(define-call lookahead-u8 "lookahead_u8" 1)

(define-call $display "display" 2)
(define-call $write "write" 2)

;; Threading

(define-call $make-thread "_make_thread" 2)
(define-call thread? "thread_p" 1)
(define-call thread-name "thread_name" 1)
(define-call thread-specific "thread_specific" 1)
(define-call thread-specific-set! "thread_specific_set_ex" 2)
(define-call thread-yield! "thread_yield_ex" 0)
(define-call thread-start! "thread_start_ex" 1)

(define-operation current-thread
  (list (string-append "return " (ct))))
(define-primitive (current-thread))

;;; A compiler pass

(define (lookup-primop x)
  (let ((x (assq x *primitives*)))
    (if x (cdr x) #f)))

(define (primcall primop name args)
  (cond ((not (formals-satisfied primop args))
         (display "Warning: wrong number of arguments to built-in procedure:\n"
                  (current-error-port))
         (pretty-print (cons name args) (current-error-port))
         (newline (current-error-port))
         (primops (list 'begin (cons 'begin args)
                        (list 'error (list 'quote name)
                              (list 'quote "Wrong number of arguments")))))
        (else
         (cons '$primcall (cons name args)))))

;; The input is from aconv. The output language differentiates between
;; calls to known primitives and calls to closures.
(define (primops x)
  (if (symbol? x)
      (if (lookup-primop x)
          (list '$primitive x)
          x)
      (case (car x)
        ((lambda)
         (list 'lambda (lambda-formals x) (primops (lambda-body x))))
        ((if)
         (cons 'if (map (lambda (x) (primops x)) (cdr x))))
        ((quote) x)
        ((define)
         (list 'define (cadr x) (primops (caddr x))))
        ((begin)
         (cons 'begin (map (lambda (x) (primops x)) (cdr x))))
        ((set!)
         (list 'set! (set!-name x) (primops (set!-expression x))))
        (else
         (let ((primop (and (pair? x) (lookup-primop (car x)))))
           (if primop
               (primcall primop (car x) (map (lambda (x) (primops x)) (cdr x)))
               (cons '$funcall (map (lambda (x) (primops x)) x))))))))

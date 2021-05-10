#lang racket

(require racket/format)
(require racket/control)

(require "lisp-CPS-transform.rkt")
(require "lisp-alpha-transform.rkt")
(require "whitespace_converter.rkt")
(require "whitespace_assembler.rkt")

(define (yield x) (shift k (cons x (k (void)))))

(define (transform program)
  (T-c (alpha-transform program) '(lambda (x) x)))

(define (prim->instruction primitive)
  (match primitive
    ['+ "add"]
    ['- "sub"]
    ['* "mul"]
    ['/ "div"]
    ['modulus "mod"]))

(define (compile-program program)
  (transform program))

(define (load-program in-file)
  (define in (open-input-file in-file))
  (define program (read in))
  (close-input-port in)
  program)

(define (save-assembly assembly out-file)
  (define out (open-output-file out-file #:exists 'truncate))
  (display assembly out)
  (close-output-port out))

(define (compile-from-file in-file out-file)
  (define program (load-program in-file))
  (define assembly (string-join (compile-program program) (string #\newline)))
  (save-assembly assembly out-file))

(define (compile-and-assemble in-file out-file)
  (define asm-file (string-append in-file ".asm"))
  (compile-from-file in-file asm-file)
  (assembly->whitespace asm-file))

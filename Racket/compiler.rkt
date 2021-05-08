#lang racket

(require racket/match)
(require racket/format)

(require "whitespace_converter.rkt")
(require "whitespace_assembler.rkt")

(define (expr->assembly expr)
  (if (number? expr)
      `(,(format "psh ~a" expr))
      (subracket->assembly-step expr)))

(define (op->assembly op)
  (match op
    ['+ '("add")]
    ['- '("sub")]
    ['* '("mul")]
    ['/ '("div")]
    ['modulus '("mod")]))

; Takes in only well-formed LISP-style code with math operators (+, -, *, /, modulus)
; and two arguments per operator call
; e.g. '(+ (* 2 3) (- 4 5))
(define (subracket->assembly-step subracket)
  (match subracket
    [`(,op ,lhs ,rhs) (append (expr->assembly lhs)
                              (expr->assembly rhs)
                              (op->assembly op))]))

(define (subracket->assembly subracket)
  (append (subracket->assembly-step subracket) '("pnm") '("end")))

(define (subracket->assembly-str subracket)
  (string-join (subracket->assembly subracket) (string #\newline)))

(define (read-subracket filename)
  (define in (open-input-file filename))
  (let ((input (read in)))
    (close-input-port in)
    input))

(define (save-assembly filename assembly)
  (define out (open-output-file filename #:exists 'truncate))
  (display assembly out)
  (close-output-port out))

(define (compile-subracket in-file out-file)
  (save-assembly out-file (subracket->assembly-str (read-subracket in-file))))

(define (compile-and-assemble in-file)
  (define assembly-file (string-append in-file ".out"))
  (compile-subracket in-file assembly-file)
  (assembly->whitespace assembly-file))
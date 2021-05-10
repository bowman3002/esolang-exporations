#lang racket

(require racket/format)
(require racket/control)

(require "lisp-CPS-transform.rkt")
(require "lisp-alpha-transform.rkt")
(require "whitespace_converter.rkt")
(require "whitespace_assembler.rkt")

(define (yield x) (shift k (cons x (k (void)))))

(define (prim->instruction primitive)
  (match primitive
    ['+ "add"]
    ['- "sub"]
    ['* "mul"]
    ['/ "div"]
    ['modulus "mod"]))

(define (CPS-transform expr)
  (T-c expr '(lambda (x) print-end)))

(define (transform-pipeline program)
  (define out-program (alpha-transform (CPS-transform program)))
  (find-function-bindings out-program)
  out-program)

(define (compile expr)
  (compile-impl expr))

(define (compile-args-list args-list)
  (match args-list
    [`(,first ,rest ...)
     ; =>
     (define arg-heap-index (hash-ref bindings-hash-table first))
     (yield (format "psh ~a" arg-heap-index))
     (yield "swp")
     (yield "str")
     (compile-args-list rest)]
    [`() '()]))

(define (compile-impl expr)
    (match expr
      [`(lambda (,args ... ,cont-sym) ,body)
       ; =>
       (compile-args-list args)
       (compile-impl body)
       (match (hash-ref bindings-hash-table cont-sym)
         [`(func-call ,func-index)
          ; =>
          (yield (format "jmp ~a" func-index))]
         [ _ '()])]
      [`((cps ,sym) ,args ... ,cont-sym)
       ; =>
       (compile-impl args)
       (define instruction (prim->instruction sym))
       (for ([i (build-list (- (length args) 1) (lambda (x) 0))])
         (yield instruction))]
      [`(,first)
       ; =>
       `(,(compile-impl first))]
      [`(,first ,rest ...)
       ; =>
       (compile-impl first)
       (compile-impl rest)]
      ['print-end
       ; =>
       (yield "pnm")
       (yield (format "jmp ~a" (+ 1 function-label-index)))]
      [ (? symbol?)
       ; =>
       (define heap-index (hash-ref bindings-hash-table expr))
       (match heap-index
         [`(func-call ,func-index)
          ; =>
          (yield (format "jmp ~a" func-index))]
         [ heap-index
           ; =>
           (yield (format "psh ~a" heap-index))
           (yield "rtr")])]
      [_ expr]))

(define (compile-main main-program)
  (yield (format "lbl ~a" function-label-index))
  (match main-program
    [`((func-call ,index) ,args ... ,cont)
     ; =>
     (for ([arg args])
       (yield (format "psh ~a" arg)))
     (yield (format "jmp ~a" index))]
    [`((func-call ,index))
     (yield (format "jmp ~a" index))]))

(define (compile-program program)
  (define transformed-program (transform-pipeline program))
  (define instructions (reset
                        (begin
                          (compile-functions functions-hash-table)
                          (compile-main transformed-program)
                          (yield (format "lbl ~a" (+ 1 function-label-index)))
                          (yield "end")
                          '())))
  `(,(format "jmp ~a" function-label-index) . ,instructions))

(define (compile-functions functions-table)
  (map compile-function-pair (hash->list functions-table)))

(define (compile-function-pair function-pair)
  (match function-pair
    [`(,function-index . ,function)
     ; =>
     (yield (format "lbl ~a" function-index))
     (compile-impl function)]))

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

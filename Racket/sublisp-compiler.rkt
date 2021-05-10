#lang racket

(require racket/format)
(require racket/control)

(require "lisp-CPS-transform.rkt")
(require "lisp-alpha-transform.rkt")
(require "whitespace_converter.rkt")
(require "whitespace_assembler.rkt")

(define transformed-program '())

(define (yield x) (shift k (cons x (k (void)))))

(define (prim->instruction primitive)
  (match primitive
    ['+ "add"]
    ['- "sub"]
    ['* "mul"]
    ['/ "div"]
    ['modulus "mod"]))

(define (CPS-transform expr)
  (T-c expr '(lambda (x) (print-end x))))

(define (transform-pipeline program)
  (define out-program (alpha-transform (CPS-transform program)))
  ;(find-function-bindings out-program)
  (set! transformed-program out-program)
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

(define (call-fn cont-sym)
  (match cont-sym
    [`(func-call ,func-index)
     ; =>
     (yield (format "psh ~a" func-index))
     (yield "psh 0")
     (yield "swp")
     (yield "str")
     (yield "jmp 0")]
    [_
     ; =>
     (define fn-index (hash-ref bindings-hash-table cont-sym))
     (yield (format "psh ~a" fn-index)) ; BEGIN - set FN-ind reg (heap:0) to cont-fn
     (yield "rtr")
     (yield "psh 0")
     (yield "swp")
     (yield "str")
     (yield "jmp 0")])) ; END

(define (compile-impl expr)
    (match expr
      [`(lambda (,args ...) ,body)
       ; =>
       (compile-args-list args)
       (compile-impl body)]
      [`(func-call ,func-index)
       (yield (format "psh ~a" func-index))]
      [`((cps ,sym) ,args ... ,cont-sym)
       ; =>
       (for ([arg (reverse args)])
         (compile-impl arg))
       (define instruction (prim->instruction sym))
       (for ([i (build-list (- (length args) 1) (lambda (x) 0))])
         (yield instruction))
       (call-fn cont-sym)]
      [`(print-end ,expr)
       ; =>
       (yield (format "psh ~a" (hash-ref bindings-hash-table expr)))
       (yield "rtr")
       (yield "pnm")
       (yield (format "jmp ~a" (+ 1 function-label-index)))]
      [`(,first ,rest ...)
       ; =>
       (for ([i (reverse rest)])
         (compile-impl i))
       (call-fn first)]
      [ (? symbol?)
       ; =>
       (define heap-index (hash-ref bindings-hash-table expr))
       (match heap-index
         [`(func-call ,func-index)
          ; =>
          (yield (format "psh ~a" func-index))]
         [ heap-index
           ; =>
           (yield (format "psh ~a" heap-index))
           (yield "rtr")])]
      [_
       ; =>
       (yield (format "psh ~a" expr))]))

(define (compile-main main-program)
  (compile-impl main-program))

(define (compile-program program)
  (define transformed-program (transform-pipeline program))
  (define instructions (reset
                        (begin
                          (compile-main transformed-program)
                          (compile-function-lookup functions-hash-table)
                          (compile-functions functions-hash-table)
                          (yield (format "lbl ~a" (+ 1 function-label-index)))
                          (yield "end")
                          '())))
  instructions)

(define (compile-function-lookup functions-table)
  (yield "lbl 0")
  (yield "psh 0")
  (yield "rtr")
  (yield "psh 1")
  (yield "sub")
  (map compile-lookup (hash->list functions-table))
  (yield (format "jmp ~a" (+ 1 function-label-index))))


(define (compile-lookup function-pair)
  (match function-pair
    [`(,function-index . ,_)
     ; =>
     (yield "dup")
     (yield (format "jez ~a" function-index))
     (yield "psh 1")
     (yield "sub")]))

(define (compile-functions functions-table)
  (map compile-function-pair (hash->list functions-table)))

(define (compile-function-pair function-pair)
  (match function-pair
    [`(,function-index . ,function)
     ; =>
     (yield (format "lbl ~a" function-index))
     (yield "dis")
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

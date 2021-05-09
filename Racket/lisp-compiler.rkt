#lang racket


(require racket/match)

; Converts atomic expressions (symbols, integers, or lambdas) into atomic CPS expressions
; When converting a lambda body, uses simple T-c transform to convert directly to
; this lambda's continuation without wrapping in another lambda
(define (M expr)
  (match expr
    [`(lambda (,var) ,expr)
     ; =>
     (define $k (gensym '$k))
     `(lambda (,var ,$k) ,(T-c expr $k))]

    [(? symbol?) expr]
    [(? integer?) expr]))

; Converts expression and continuation into application of expr into the continuation
; Used by M when we are transforming a piece of a simple atomic expression
; Recurses into T-k to handle CPS transform of a more complex nested CPS application
(define (T-c expr cont)
  (match expr
    [`(lambda . ,_)    `(,cont ,(M expr))]
    [ (? symbol?)      `(,cont ,(M expr))]
    [ (? integer?)     `(,cont ,(M expr))]
    [`(,f ,e)
      ; =>
      (define $f (gensym '$f))
      (define $e (gensym '$e))
      (T-k f (lambda ($f)
               (T-k e (lambda ($e)
                        `(,$f ,$e ,cont)))))]))

; Converts expression and (atomic-expr -> CPS-expr) function into application of the expr into
; the continuation
; Allows calling function to create wrapped continuation holding more complex expressions?
(define (T-k expr k)
  (match expr
    [`(lambda . ,_)     (k (M expr))]
    [ (? symbol?)       (k (M expr))]
    [ (? integer?)      (k (M expr))]
    [`(,f ,e)
     ; =>
     (define $rv (gensym '$rv))
     (define cont `(lambda (,$rv) ,(k $rv)))
     (T-k f (lambda ($f)
            (T-k e (lambda ($e)
                   `(,$f ,$e ,cont)))))]))
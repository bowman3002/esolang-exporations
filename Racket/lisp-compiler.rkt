#lang racket


(require racket/match)

; Converts atomic expressions (symbols, integers, or lambdas) into atomic CPS expressions
(define (M expr)
  (match expr
    [`(lambda (,var) ,expr)
     ; =>
     (define $k (gensym '$k))
     `(lambda (,var ,$k) ,(T-k expr (lambda (rv) `(,$k ,rv))))]

    [(? symbol?) expr]
    [(? integer?) expr]))

; Converts expression and (atomic-expr -> CPS-expr) function into application of the expr into
; the continuation
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

; Converts expression and continuation into application of expr into the continuation
(define (T expr cont)
  (match expr
    [`(lambda . ,_)    `(,cont ,(M expr))]
    [ (? symbol?)      `(,cont ,(M expr))]
    [ (? integer?)     `(,cont ,(M expr))]
    [`(,f ,e)
      ; =>
      (define $f (gensym '$f))
      (define $e (gensym '$e))
      (T f `(lambda (,$f)
              ,(T e `(lambda (,$e)
                       (,$f ,$e ,cont)))))]
      ))
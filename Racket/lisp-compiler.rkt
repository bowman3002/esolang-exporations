#lang racket


(require racket/match)

; Converts atomic expressions (symbols, integers, or lambdas) into atomic CPS expressions
(define (M expr)
  (match expr
    [`(lambda (,var) ,expr)
     ; =>
     (define $k (gensym '$k))
     `(lambda (,var ,$k) ,(T expr $k))]

    [(? symbol?) expr]
    [(? integer?) expr]))

; Converts expression and continuation into application of expr into the continuation
(define (T expr cont)
  (match expr
    [`(lambda . ,_)    `(,cont ,(M expr))]
    [ (? symbol?)      `(,cont ,(M expr))]
    [ (? integer?)     `(,cont ,(M expr))]
    [ `(,f ,e)
      ; =>
      (define $f (gensym '$f))
      (define $e (gensym '$e))
      (T f `(lambda (,$f)
              ,(T e `(lambda (,$e)
                       (,$f ,$e ,cont)))))]
      ))
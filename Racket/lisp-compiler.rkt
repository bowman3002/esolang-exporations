#lang racket


(require racket/match)

; Atomic expressions are those that cannot be split up further into recursable-pieces
(define (aexpr? expr)
  (match expr
    [`(lambda (,_) ,_) #t]
    [ (? symbol?)     #t]
    [ (? integer?)    #t]
    [ else #f]))

; Primitive functions that allow any number of arguments
(define prims (apply set '(+ - / * modulus)))

(define (prim? term)
  (set-member? prims term))

; Converts atomic expressions into atomic CPS expressions
; When converting a lambda body, uses simple T-c transform to convert directly to
; this lambda's continuation without wrapping in another lambda
(define (M aexpr)
  (match aexpr
    [`(lambda (,vars ...) ,body)
     ; =>
     (define $k (gensym '$k))
     `(lambda (,@vars ,$k)
        ,(T-c body $k))]

    [(or (? symbol?)
         (? integer?))
     ; =>
     aexpr]
    [else
     (error "Not an aexpr!")]))

; Converts expression and continuation into application of expr into the continuation
; Used by M when we are transforming a piece of a simple atomic expression
; Recurses into T-k to handle CPS transform of a more complex nested CPS application
(define (T-c expr cont)
  (match expr
    [ (? aexpr?)       `(,cont ,(M expr))]
    [`(if-ltz ,expr-num ,expr-t ,expr-f)
     ; =>
     (define $k (gensym '$k))
     `((lambda (,$k)
         ,(T-k expr-num (lambda (aexp)
                          `(if-ltz ,aexp
                                   ,(T-c expr-t $k)
                                   ,(T-c expr-f $k)))))
       ,cont)]
    [`(if-eqz ,expr-num ,expr-t ,expr-f)
     ; =>
     (define $k (gensym '$k))
     `((lambda (,$k)
         ,(T-k expr-num (lambda (aexp)
                          `(if-eqz ,aexp
                                   ,(T-c expr-t $k)
                                   ,(T-c expr-f $k)))))
       ,cont)]
    [`(,(and p (? prim?)) ,es ...)
     ; =>
     (T*-k es (lambda ($es)
                `((cps ,p) ,@$es ,cont)))]
    [`(,f ,es ...)
     ; =>
     (T-k f (lambda ($f)
              (T*-k es (lambda ($es)
                         `(,$f ,@$es ,cont)))))]))

; Converts expression and (atomic-expr -> CPS-expr) function into application of the expr into
; the continuation
; Allows calling function to create wrapped continuation holding more complex expressions?
(define (T-k expr k)
  (match expr
    [ (? aexpr?)    (k (M expr))]
    [`(if-ltz ,expr-num ,expr-t ,expr-f)
     ; =>
     (define $rv (gensym '$rv))
     (define cont `(lambda (,$rv) ,(k $rv)))
     (T-k expr-num (lambda (aexp)
                     `(if-ltz ,aexp
                              ,(T-c expr-t cont)
                              ,(T-c expr-f cont))))]
    [`(if-eqz ,expr-num ,expr-t ,expr-f)
     ; =>
     (define $rv (gensym '$rv))
     (define cont `(lambda (,$rv) ,(k $rv)))
     (T-k expr-num (lambda (aexp)
                     `(if-eqz ,aexp
                              ,(T-c expr-t cont)
                              ,(T-c expr-f cont))))]
    [`(,_ ,_ ...)
     ; =>
     (define $rv (gensym '$rv))
     (define cont `(lambda (,$rv) ,(k $rv)))
     (T-c expr cont)]))

; Recursive list-handling version of T-k
(define (T*-k exprs k)
  (cond
    [(null? exprs) (k '())]
    [(pair? exprs) (T-k (car exprs) (lambda (hd)
                                      (T*-k (cdr exprs) (lambda (tl)
                                                          (k (cons hd tl))))))]))


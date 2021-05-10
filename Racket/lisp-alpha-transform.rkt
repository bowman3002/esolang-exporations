#lang racket

(require racket/match)

(provide alpha-transform)

(define (alpha-transform expr)
  (match expr
    [`(lambda (,args ...) ,body)
     ; =>
     (define alpha-body (alpha-transform body))
     (match (parse-bindings args alpha-body)
       [`(func-call ,func-index)
        ; =>
        17]
       [`(,new-args ,new-body)
        ; =>
        (define new-lambda `(lambda (,@new-args) ,new-body))
        new-lambda])]
    [`(,first ,rest ...)
     ; =>
     (define alpha-first (alpha-transform first))
     (define alpha-rest (alpha-transform rest))
     `(,alpha-first . ,alpha-rest)]
    [ else expr]))

(define (parse-bindings args-list body)
  (match args-list
    [`(,first ,rest ...)
     ; =>
     (define first-sym (gensym '$s))
     (define new-body (replace-symbol body first first-sym))
     (match (parse-bindings rest new-body)
       [`(,rest-args . (,final-body))
        ; =>
        `(,(cons first-sym rest-args) . (,final-body))])]
    [`()
     ; =>
     `(() . (,body))]))

(define (replace-symbol expr old new)
  (match expr
    [`(lambda (,args ...) ,body)
     ; =>
     `(lambda (,@args) ,(replace-symbol body old new))]
    [`(,first ,rest ...)
     ; =>
     `(,(replace-symbol first old new) . ,(replace-symbol rest old new))]
    [ (? symbol?) (if (equal? expr old) new expr)]
    [ _ expr]))

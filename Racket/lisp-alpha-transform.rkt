#lang racket

(require racket/match)

(provide bindings-hash-table)
(provide functions-hash-table)
(provide alpha-transform)

(define binding-index 2)
(define bindings-hash-table (make-hash))
(define function-label-index 0)
(define functions-hash-table (make-hash))

(define (add-binding symbol)
  (hash-set! bindings-hash-table symbol binding-index)
  (set! binding-index (+ 1 binding-index)))

(define (add-function function)
  (hash-set! functions-hash-table function-label-index function)
  (define current-function-label function-label-index)
  (set! function-label-index (+ 1 function-label-index))
  current-function-label)

(define (alpha-transform expr)
  (match expr
    [`(lambda (,args ...) ,body)
     ; =>
     (define alpha-body (alpha-transform body))
     (match (parse-bindings args body)
       [`(,new-args ,new-body)
        ; =>
        (define new-lambda `(lambda (,@new-args) ,new-body))
        `(func-call ,(add-function new-lambda))])]
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
     (add-binding first-sym)
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
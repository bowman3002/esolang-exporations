#lang racket

(require racket/match)

(provide bindings-hash-table)
(provide functions-hash-table)
(provide alpha-transform)
(provide find-function-bindings)
(provide function-label-index)
(provide bindings-function-table)

(define binding-index 2)
(define bindings-hash-table (make-hash))
(define function-label-index 0)
(define functions-hash-table (make-hash))
(define bindings-function-table (make-hash))

(define (add-binding symbol)
  (hash-set! bindings-hash-table symbol binding-index)
  (set! binding-index (+ 1 binding-index)))

(define (add-function function)
  (hash-set! functions-hash-table function-label-index function)
  (define current-function-label function-label-index)
  (set! function-label-index (+ 1 function-label-index))
  current-function-label)

(define (add-function-binding function-index binding-symbol)
  (hash-set! binding-symbol function-index))

(define (bind-for-function function-label args-list)
  (define function (hash-ref functions-hash-table function-label))
  (match function
    [`(lambda (,bindings ...) ,_)
     ; =>
     (bind-function bindings args-list)]))

(define (bind-function bindings-list args-list)
  (match `(,bindings-list ,args-list)
    [`((,binding ,bind-rest ...) ((func-call ,func-label) ,arg-rest ...))
     ; =>
     (hash-set! bindings-hash-table binding `(func-call ,func-label))
     (bind-function bind-rest arg-rest)]
    [`((,_ ,bind-rest ...) (,_ ,arg-rest ...))
     ; =>
     (bind-function bind-rest arg-rest)]
    [`((,bind-last) ((func-call ,func-label)))
     ; =>
     (hash-set! bindings-hash-table bind-last `(func-call ,func-label))]
    [ _
     ; =>
      '()]))

(define (find-function-bindings expr)
  (match expr
    [`((func-call ,function-label) ,args ...)
     ; =>
     (define current-function (hash-ref functions-hash-table function-label))
     (match current-function
       [`(lambda (,bindings ...) ,body)
        ; =>
        (bind-function bindings args)
        (find-function-bindings body)])
     (for ([arg args])
       (find-function-bindings arg))]
    [`(,elts ...)
     ; =>
     (for ([elt elts])
       (find-function-bindings elt))]
    [`(func-call ,function-label)
     ; =>
     (define function (hash-ref functions-hash-table function-label))
     (match function
       [`(lambda (,_ ...) ,body)
        ; =>
        (find-function-bindings body)])]
    [ else '()]))

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
        (define function-label (add-function new-lambda))
        `(func-call ,function-label)])]
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
    [`(func-call ,func-index)
     ; =>
     (define old-lambda (hash-ref functions-hash-table func-index))
     (match old-lambda
       [`(lambda (,args ...) ,body)
        ; =>
        (define new-lambda `(lambda (,@args) ,(replace-symbol body old new)))
        (hash-set! functions-hash-table func-index new-lambda)
        `(func-call ,func-index)])]
    [`(,first ,rest ...)
     ; =>
     `(,(replace-symbol first old new) . ,(replace-symbol rest old new))]
    [ (? symbol?) (if (equal? expr old) new expr)]
    [ _ expr]))

(define full-transform
  (compose find-function-bindings alpha-transform))
#lang racket

(define (closure-transform expr)
  (match expr
    [`(lambda (,args ...) ,body)
     ; =>
     (define free-vars (set->list (free expr)))
     (list->vector `((lambda (self ,@args) ,body) . ,free-vars))]))

(define (free expr)
  (match expr
    [`(lambda (,args ...) ,body)
     (set-subtract (free body) (apply set args))]
    [(? symbol?)
     (set expr)]
    [`(,f ,args ...)
     (apply set-union (map free `(,f . ,args)))]))
#lang racket

(require "whitespace_converter.rkt")

(define (number->whitespacechar number)
  (define (binary->whitespacechar binary)
    (if (= binary 0)
        #\s
        #\t))
  (define (number->whitespacechar-rec number char-list)
    (if (= number 0)
        char-list
        (number->whitespacechar-rec (quotient number 2) (cons (binary->whitespacechar (remainder number 2)) char-list))))
  (if (< number 0)
      (cons #\t (number->whitespacechar-rec (abs number) '()))
      (cons #\s (number->whitespacechar-rec number '()))))

(define (assembly->whitespacechars command)
  (cond ((= command "dup")
         '(#\s #\l #\s))))
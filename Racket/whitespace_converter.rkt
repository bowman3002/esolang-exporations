#lang racket


(define (convert-literal literal)
  (cond ((eq? literal #\s)
         #\space)
        ((eq? literal #\n)
         #\newline)
        ((eq? literal #\t)
         #\tab)))


(define (literal->whitespace literal-list)
  (define (literal->whitespace-rec literal-list whitespace-list)
    (if (empty? literal-list)
        (reverse whitespace-list)
        (literal->whitespace-rec (rest literal-list) (cons (convert-literal (first literal-list)) whitespace-list))))
  (literal->whitespace-rec literal-list '()))


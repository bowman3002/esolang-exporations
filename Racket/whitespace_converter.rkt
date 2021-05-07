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


(define (print-list list)
  (define out (open-output-file "data" #:exists 'truncate))
  (display list out)
  (close-output-port out)
  )

(define (read-conversion)
  (define in (open-input-file "input"))
  (let ((input (read-line in)))
    (close-input-port in)
    input)
  )

(define run-conversion
  (compose print-list list->string literal->whitespace string->list read-conversion)
  )

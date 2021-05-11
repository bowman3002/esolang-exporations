((lambda (gcd get-user-input) ((get-user-input) gcd))
    ((lambda (x) (x x))
        (lambda (f)
            (lambda (big small)
                ((lambda (remainder)
                    (if-eqz remainder
                            small
                            ((f f) small remainder))) (modulo big small)))))
    (lambda ()
        ((lambda (first)
            ((lambda (second)
                (if-ltz (- first second)
                        (lambda (func) (func second first))
                        (lambda (func) (func first second)))) (input))) (input))))

#lang multiscope
(scopes [rkt racket/base]
        [dsl (submod "dsl.rkt" base)])
(require "dsl.rkt" chk)

(chk (cek (dsl 5)) 5)
(chk (cek (dsl (+ 5 5))) 10)
(chk (cek (dsl ((λ (x) x) 5))) 5)

(define (pow2 i)
  (cond
    [(zero? i) (dsl 1)]
    [else
     (dsl (let ([x (rkt (pow2 (sub1 i)))])
            (+ x x)))]))

(chk (cek (pow2 5)) (expt 2 5))

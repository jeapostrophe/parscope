#lang racket
(require multiscope
         "dsl.rkt")
#;#; ;; IMAGINE "dsl.rkt" contains:
(define-scope dsl rkt (submod "dsl.rkt" base))
(provide dsl)

;; This means that it provides a dsl form, with an escape form rkt,
;; which is bootstrapped to the scope requiring (submod "dsk.rkt"
;; base), and maybe I actually just write those definitions inside the
;; `define-scope` form.

(module+ test
  (require chk))

(module+ test
  (chk (cek (dsl 5)) 5)
  (chk (cek (dsl (+ 5 5))) 10)
  (chk (cek (dsl ((λ (x) x) 5))) 5))

(define (pow2 i)
  (cond
    [(zero? i) (dsl 1)]
    [else
     (dsl (let ([x (rkt (pow2 (sub1 i)))])
            (+ x x)))]))

(module+ test
  (chk (cek (pow2 5)) (expt 2 5)))

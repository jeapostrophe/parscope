#lang racket/base
(require racket/match)

(struct :con (b))
(struct :var (x))
(struct :lam (x m))
(struct :app (m n))
(struct :prm (o ms))

(struct :clo (l e))

(struct k:ret ())
(struct k:arg (n e k))
(struct k:fn (v k))
(struct k:prm (o vs e ms k))

(define (cek c [e (hasheq)] [k (k:ret)])
  (match c
    [(:var x) (cek (hash-ref e x) #f k)]
    [(:lam x m) (cek (:clo c e) #f k)]
    [(:app m n) (cek m e (k:arg n e k))]
    [(:prm o '()) (cek (:con (o)) #f k)]
    [(:prm o (cons m ms)) (cek m e (k:prm o '() e ms k))]
    [v
     (match k
       [(k:ret)
        (match v
          [(:con b) b]
          [(:clo _ _) 'lambda])]
       [(k:arg n e k)
        (cek n e (k:fn v k))]
       [(k:fn (:clo (:lam x m) e) k)
        (cek m (hash-set e x v) k)]
       [(k:prm o vs _ '() k)
        (cek (:con (apply o (map :con-b (reverse (cons v vs))))) #f k)]
       [(k:prm o vs e (cons m ms) k)
        (cek m e (k:prm o (cons v vs) e ms k))])]))

(module+ test
  (require chk)
  (chk (cek (:con 5)) 5)
  (chk (cek (:prm + (list (:con 5) (:con 5)))) 10)
  (chk (cek (:app (:lam 'x (:var 'x)) (:con 5))) 5)

  (define (pow2 i)
    (cond
      [(zero? i) (:con 1)]
      [else
       (define im1 (pow2 (sub1 i)))
       (:app (:lam 'x (:prm + (list (:var 'x) (:var 'x))))
             im1)]))

  (chk (cek (pow2 6)) (expt 2 6)))

(provide cek)
(module+ base
  (define-syntax-rule (-#%app m n) (:app m n))
  (define-syntax-rule (-+ x y) (:prm + (list x y)))
  (define-syntax-rule (-#%datum . d) (:con (#%datum . d)))
  (define-syntax-rule (-lambda (x) m)
    (let ([x-id (gensym)])
      (:lam x-id (let ([x (:var x-id)]) m))))
  (define-syntax-rule (-let ([x xe]) b)
    (-#%app (-lambda (x) b) xe))
  (provide
   (rename-out [-+ +] [-#%app #%app] [-#%datum #%datum]
               [-lambda lambda] [-lambda λ] [-let let])))

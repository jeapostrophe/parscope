#lang racket/base
(require racket/match)

;; Expressions
(struct :var (x))
(struct :lam (xs m))
(struct :app (m ns))
(struct :callcc (m))

;; External Values
(struct :con (b))
(struct :prm (o))

;; Internal Values
(struct :clo (l e))
(struct :kont (k))

;; Continuations
(struct k:ret ())
(struct k:fun (e ns k))
(define (k:fun* e ns k) (k:fun (and (not (null? ns)) e) ns k))
(struct k:app (fv vs e ns k))

(define (hash-set-all h ks vs)
  (for/fold ([h h]) ([k (in-list ks)] [v (in-list vs)])
    (hash-set h k v)))

(define (cek-apply fv vs k)
  (match fv
    [(:kont k)
     (match-define (list v) vs)
     (cek v #f k)]
    [(:clo (:lam xs m) e)
     (cek m (hash-set-all e xs vs) k)]
    [(:prm o)
     (cek (:con (apply o (map :con-b vs))) #f k)]))

(define (cek c [e (hasheq)] [k (k:ret)])
  (match c
    [(:var x)    (cek (hash-ref e x) #f k)]
    [(:lam _ _)  (cek (:clo c e) #f k)]
    [(:app m ns) (cek m e (k:fun* e ns k))]
    [(:callcc m) (cek (:app m (list (:kont k))) e k)]
    [v
     (match k
       [(k:fun e '() k)
        (cek-apply v '() k)]
       [(k:fun e (cons n ns) k)
        (cek n e (k:app v '() e ns k))]
       [(k:app fv vs e (cons n ns) k)
        (cek n e (k:app fv (cons v vs) e ns k))]
       [(k:app fv vs e '() k)
        (cek-apply fv (reverse (cons v vs)) k)]
       [(k:ret)
        (match v
          [(:con b) b]
          [(:clo _ _) 'lambda]
          [(:kont _) 'continuation]
          [(:prm _) 'primitive])])]))

(module+ test
  (require chk)
  (chk (cek (:con 5)) 5)
  (chk (cek (:app (:prm +) (list (:con 5) (:con 5)))) 10)
  (chk (cek (:app (:lam '(x) (:var 'x)) (list (:con 5)))) 5)

  (define (pow2 i)
    (cond
      [(zero? i) (:con 1)]
      [else
       (define im1 (pow2 (sub1 i)))
       (:app (:lam '(x) (:app (:prm +) (list (:var 'x) (:var 'x))))
             (list im1))]))

  (chk (cek (pow2 6)) (expt 2 6)))

(provide cek)
(module+ base
  (require (for-syntax racket/base))
  (define-syntax-rule (-#%app m n ...) (:app m (list n ...)))
  (define -+ (:prm +))
  (define-syntax-rule (-#%datum . d) (:con (#%datum . d)))
  (define-syntax (-lambda stx)
    (syntax-case stx ()
      [(_ (x ...) m)
       ;; XXX check xs are unique
       (with-syntax ([(x-id ...) (generate-temporaries #'(x ...))])
         (syntax/loc stx
           (let ([x-id (gensym)] ...)
             (:lam (list x-id ...)
                   (let ([x (:var x-id)] ...)
                     m)))))]))
  (define-syntax-rule (-let ([x xe] ...) b)
    (-#%app (-lambda (x ...) b) xe ...))
  (define-syntax-rule (-call/cc m) (:callcc m))
  (define-syntax-rule (-let/cc k m)
    (-call/cc (-lambda (k) m)))
  (provide
   (rename-out [-+ +] [-#%app #%app] [-#%datum #%datum]
               [-lambda lambda] [-lambda λ] [-let let]
               [-call/cc call/cc] [-let/cc let/cc])))

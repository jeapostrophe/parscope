#lang racket/base
(require (for-syntax racket/base))

(begin-for-syntax
  (struct parscope (si)))

(define-syntax (define-scope stx)
  (syntax-case stx ()
    [(_ sc . initial)
     (identifier? #'sc)
     (syntax/loc stx
       (begin
         (define-for-syntax sc-scope (make-syntax-introducer #t))
         (define-syntax (sc stx)
           (syntax-case stx ()
             [(_ in)
              (sc-scope (datum->syntax #f (syntax->datum #'in)) 'add)]))
         (define-syntax (sc-just-add stx)
           (syntax-case stx ()
             [(_ . in)
              (sc-scope #'(begin . in) 'add)]))
         (sc-just-add . initial)))]))

(provide define-scope)

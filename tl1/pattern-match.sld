(define-library (tl1 pattern-match)
  (export pattern-match)
  (import (scheme base))
  (begin

    (define-syntax identifier
      (syntax-rules ()
        ((_ condition seq alt)
         (let-syntax ((foo (syntax-rules () ((_) seq))))
           (let-syntax ((test (syntax-rules ()
                                ((_ condition) (foo))
                                ((_ foo) alt))))
             (test foo))))))

    (define-syntax literal
      (syntax-rules ()
        ((_ p (literals ...) seq alt)
         (let-syntax ((bar (syntax-rules () ((_) seq))))
           (let-syntax ((foo (syntax-rules (literals ...)
                               ((_ literals) (bar)) ...
                               ((_ bar) alt))))
             (foo p))))))

    (define-syntax placeholder
      (syntax-rules (_)
        ((_ _ seq alt) seq)
        ((_ p seq alt) alt)))

    (define-syntax %match
      (syntax-rules ()
        ((_ (literals ...) (p . r) e seq alt)
         (let ((temp e))
           (if (pair? temp)
               (%match (literals ...) p (car temp)
                       (%match (literals ...) r (cdr temp) seq alt)
                       alt)
               (alt))))
        ((_ (literals ...) () e seq alt)
         (if (null? e) seq (alt)))
        ((_ (literals ...) p e seq alt)
         (identifier p
                     (literal p (literals ...)
                              (if (equal? 'p e) seq (alt))
                              (placeholder p
                                           seq
                                           (let ((p e)) seq)))
                     (if (equal? p e) seq (alt))))))

    (define-syntax %%match
      (syntax-rules ()
        ((_ (literals ...) pattern lst seq alt)
         (let ((alt-thunk (lambda() alt)))
           (%match (literals ...) pattern lst seq alt-thunk)))))

    (define-syntax %pattern-match
      (syntax-rules (else)
        ((_ (literals ...) lst) (if #f #t))
        ((_ (literals ...) lst (else expr))
         expr)
        ((_ (literals ...) lst
            (pattern expr ...) rest ...)
         (%%match (literals ...) pattern lst
                  (begin expr ...)
                  (%pattern-match (literals ...) lst
                                  rest ...)))
        ))

    (define-syntax pattern-match
      (syntax-rules ()
        ((_ lst (literals ...) clause  ...)
         (%pattern-match (literals ...) lst clause ...))))
    ))

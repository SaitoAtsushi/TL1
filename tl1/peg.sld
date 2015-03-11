
(define-library (tl1 peg)
  (export $return $fail $while $one-of $eqv $debug $parameterize
          $order $select $split-by $optional $let* $lazy $expect
          $between $binary-operator-left $null $error $not)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (scheme lazy))

  (begin
    (define $return
      (case-lambda
       ((v) (lambda(s) (values 'success v s)))
       ((v state) (lambda(s) (values state v s)))
       ((v state s) (lambda(_) (values state v s)))))

    (define-syntax $parameterize
      (syntax-rules ()
        ((_ ((p e) ...) body)
         (lambda(s)(parameterize ((p e) ...) (body s))))))

    (define ($fail m) ($return m 'fail))

    (define ($error m) ($return m 'error))

    (define ($expect parser msg)
      (lambda(s)
        (let-values (((r v ss) (parser s)))
          (case r
            ((success) (values r v ss))
            ((fail error)
             (values 'expect msg s))))))

    (define ($peek parser)
      (lambda(s)
        (let-values (((r v ss) (parser s)))
          (case r
            ((expect) (values 'expect v ss))
            (else (values r v ss))))))

    (define ($not parser)
      (lambda(s)
        (let-values (((r v s1)(parser s)))
          (case r
            ((success)(values 'fail v s))
            ((fail)(values 'success v s))
            (else (values r v s1))))))

    (define $while
      (case-lambda
       ((parser end)
        (lambda(s)
          (let loop ((lst s)
                     (result '()))
            (if (pair? lst)
                (let-values (((r v s1)(parser lst)))
                  (case r
                    ((success) (loop s1 (cons v result)))
                    ((fail)
                     (let-values (((r2 v2 s2)(end lst)))
                       (case r2
                         ((success) (values 'success (reverse result) s2))
                         (else (values r2 v2 s2)))))
                    ((error expect) (values r v s1))))
                (let-values (((r v s1) (end lst)))
                  (case r
                    ((success) (values 'success (reverse result) s1))
                    (else (values 'error "block is not closed" s1))))))))
       ((parser)
        (lambda(s)
          (let loop ((lst s)
                     (result '()))
            (if (pair? lst)
                (let-values (((r v s1)(parser lst)))
                  (case r
                    ((success) (loop s1 (cons v result)))
                    ((fail) (values 'success (reverse result) s1))
                    ((error expect) (values r v s1))))
                (values 'success (reverse result) lst)))))))

    (define ($one-of pred)
      (lambda(s)
        (if (pair? s)
            (let ((v (car s)))
              (if (pred v)
                  (values 'success v (cdr s))
                  (values 'fail #f s)))
            (values 'fail #f s))))

    (define ($null parser)
      (lambda(s)
        (let-values (((r v s1)(parser s)))
          (case r
            ((success)
             (if (null? s1)
                 (values r v s1)
                 (values 'fail "unknown pattern" s1)))
            (else (values r v s1))))))

    (define ($eqv ch)
      ($one-of (lambda(x)(eqv? x ch))))

    (define-syntax $let*
      (syntax-rules ()
        ((_ label ((var expr)) body)
         (lambda(label)
           (let-values (((r var s1) (expr label)))
             (case r
               ((success) (body s1))
               ((expect error) (values r var s1))
               (else (values r var label))))))
        ((_ ((var expr) rest ...) body)
         ($let* _ ((var expr) rest ...) body))
        ((_ label ((var expr) rest ...) body)
         (lambda(label)
           (let-values (((r var s1) (expr label)))
             (case r
               ((success) (($let* (rest ...) body) s1))
               ((expect error) (values r var s1))
               (else (values r var label))))))))

    (define ($binary-operator-left parser op)
      (lambda(s)
        (let-values (((r v s) (parser s)))
          (if (eqv? r 'success)
              (let loop ((r1 r) (v1 v) (s1 s))
                (let-values (((r2 v2 s2)
                              (($let* ((proc op)
                                       (v parser))
                                 ($return (list proc v1 v)))
                               s1)))
                  (case r2
                    ((success) (loop r2 v2 s2))
                    ((fail) (values r1 v1 s1))
                    ((error expect) (values r2 v2 s)))))
              (values r v s)))))

    (define ($debug parser)
      (lambda(s)
        (let-values (((r v s) (parser s)))
          (display "status : ") (write r) (newline)
          (display "result : ") (write v) (newline)
          (display "rest   : ") (write s) (newline)
          (values r v s))))

    (define ($between left body right)
      ($let* ((_ left)
              (b body)
              (_ right))
        ($return b)))

    (define-syntax $lazy
      (syntax-rules ()
        ((_ parser)
         (let ((p (delay parser)))
           (lambda (s) ((force p) s))))))

    (define ($order p . ps)
      (if (null? ps)
          p
          ($let* ((x p)
                  (xs (apply $order ps)))
            ($return xs))))

    (define ($select p . ps)
      (if (null? ps)
          p
          (lambda(s)
            (let-values (((r v s1)(p s)))
              (case r
                ((success) (values 'success v s1))
                ((fail) ((apply $select ps) s))
                ((error expect) (values r v s1)))))))

    (define ($split-by p sp)
      (lambda(s)
        (let loop ((s1 s)
                   (result '()))
          (let-values (((r v s2) (p s1)))
            (case r
              ((success)
               (let-values (((r2 v2 s3) (sp s2)))
                 (case r2
                   ((success) (loop s3 (cons v result)))
                   ((fail) (values 'success (reverse (cons v result)) s2))
                   ((error expect) (values r2 v2 s3)))))
              ((error expect) (values r v s2))
              ((fail) (values 'success '() s))
              (else (values r v s)))))))

    (define $optional
      (case-lambda
       ((parser fallback)
        (lambda(s)
          (let-values (((r v s2)(parser s)))
            (case r
              ((success) (values r v s2))
              ((fail) (values 'success fallback s))
              ((error expect) (values r v s2))))))
       ((p) ($optional p #f))))
    ))

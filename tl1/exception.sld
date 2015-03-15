
(define-library (tl1 exception)
  (export raise-tl1-error
          tl1-error?
          print-tl1-error)
  (import (scheme base)
          (scheme write))

  (begin
    (define-record-type <tl1-error>
      (make-tl1-error message irritants)
      tl1-error?
      (message tl1-error-message)
      (irritants tl1-error-irritants))

    (define (raise-tl1-error message . objs)
      (raise (make-tl1-error message objs)))

    (define (print-tl1-error err)
      (display "*error*\n" (current-error-port))
      (display (tl1-error-message err) (current-error-port))
      (display " " (current-error-port))
      (for-each
       (lambda(irr)
         (display irr (current-error-port))
         (display " " (current-error-port)))
       (tl1-error-irritants err))
      (display "\n" (current-error-port)))
    ))

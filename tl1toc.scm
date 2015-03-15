;;; TL/1 Parser Driver

(import (scheme base)
        (scheme write)
        (tl1 parser)
        (tl1 c-gen)
        (tl1 exception))

(guard (e ((tl1-error? e)
           (print-tl1-error e)
           (exit #f)))
  (call-with-port (open-output-string)
    (lambda(port)
      (parameterize ((current-output-port port))
        (let ((ast (tl1-parse (current-input-port))))
          (ast->c ast)))
      (display (get-output-string port)))))

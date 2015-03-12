;;; TL/1 Parser Driver

(import (scheme base)
        (scheme write)
        (tl1 parser)
        (tl1 exception))

(guard (e ((tl1-error? e)
           (print-tl1-error e)))
  (write (tl1-parse (current-input-port))))

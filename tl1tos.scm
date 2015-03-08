;;; TL/1 Parser Driver

(import (scheme base)
        (scheme write)
        (tl1 parser))

(write (tl1-parse (current-input-port)))

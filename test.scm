(import (rnrs)
        (prefix (boson globals) globals:)
        (prefix (boson http) http:)
        (sistim wrap64))

(test-begin "globals")
(test-assert (char? globals:*sess-id-sep*))
(test-end "globals")

; Make sure (boson http) exports all the bindings shown in the Spark manual.
(test-begin "http")
(test-assert http:web-server)
(test-assert http:web-server-start)
(test-assert http:web-server-stop)
(test-end "globals")

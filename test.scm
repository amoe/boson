(import (rnrs)
        (prefix (boson globals) globals:)
        (prefix (boson http) http:)
        (prefix (boson compat) compat:)
        (prefix (boson session-util) session-util:)
        (sistim wrap64))

(test-begin "globals")
(test-assert (char? globals:*sess-id-sep*))
(test-end "globals")

; Make sure (boson http) exports all the bindings shown in the Spark manual.
; We don't test these functions here as the http library is just re-exporting
; from other modules.
(test-begin "http")
(test-assert http:web-server)
(test-assert http:web-server-start)
(test-assert http:web-server-stop)
(test-end "http")

(test-begin "compat")
(test-equal "txt" (compat:filename-extension "readme.txt"))
(test-equal "gz" (compat:filename-extension "package.tar.gz"))
(test-assert (not (compat:filename-extension "README")))

; We don't test the mosh-specific functions as they will go away.
(test-end "compat")

(test-begin "session-util")
(let ((state (session-util:make-default-session-state 'nonesuch))
      (key "page") (value "index"))
  (test-assert (hashtable? state))   ; this type is specified, implementation is
                                     ; not abstract.  user can use hash procedures.
  (session-util:http-value! state key value)
  (test-equal value (session-util:http-value state key "default"))
  (test-assert (not (session-util:http-value state "not-present")))
  (test-assert (not (session-util:http-keep-alive? state)))
  (session-util:http-keep-alive! state #t)
  (test-assert (session-util:http-keep-alive? state))
  (test-assert (session-util:http-share-state? state))
  (session-util:http-share-state! state #f)
  (test-assert (not (session-util:http-share-state? state))))
  

; http-call is used to throw procedures around with exceptions.
(let ((proc (lambda (x) x)))   ; never actually called, just type-checked
  (test-error procedure? (session-util:http-call proc))
  (test-error string?    (session-util:http-call 1234)))   ; error case.
  
;; (test-assert session-util:make-default-session-state)
(test-end "session-util")

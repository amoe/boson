(import (rnrs)
        (prefix (boson globals) globals:)
        (prefix (boson http) http:)
        (prefix (boson compat) compat:)
        (prefix (boson session-util) session-util:)
        (prefix (boson url-encode) url-encode:)
        (prefix (boson util) util:)
        (prefix (boson sml-parser) sml-parser:)
        (prefix (boson request-parser) request-parser:)
        (prefix (boson response) response:)
        (sistim wrap64))

(define-syntax test-not-error
  (syntax-rules ()
    ((test-no-error expr)
     (test-assert (begin expr #t)))))
      

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
(test-end "session-util")
  
(test-begin "url-encode")
(test-equal "hello" (url-encode:url-decode "hello"))
(test-equal "hello world" (url-encode:url-decode "hello%20world"))
(test-equal "~tilde~" (url-encode:url-decode "%7Etilde%7E"))
(test-equal "~tilde~" (url-encode:url-decode "%7etilde%7e"))   ; should be case
                                                               ; insensitive
(test-end "url-encode")


(test-begin "util")
(test-equal "fry bender leela"
            (call-with-string-output-port
             (lambda (p)
               (util:fprintf p "~a ~a ~a" "fry" "bender" "leela"))))

(test-equal "fry"
            (call-with-string-output-port
             (lambda (p)
               (util:fprintf p "f~cy" #\r))))

(test-equal "new\nlines\nrule"
            (call-with-string-output-port
             (lambda (p)
               (util:fprintf p "~a~%~a~%~a" "new" "lines" "rule"))))

(test-assert (integer? (util:current-seconds)))
; great scott!
(test-assert (not (zero? (util:current-seconds))))

(let ((counter 0)
      (ht (make-eq-hashtable)))
  (hashtable-set! ht 'foo 'bar)
  (hashtable-set! ht 'baz 'quux)
  (hashtable-set! ht 'fry 'bender)
  (util:hashtable-for-each ht
   (lambda (key val)
     (set! counter (+ counter 1))))
  (test-eqv 3 counter))

(test-end "util")


(test-begin "sml-parser")
(let ((document "foo")
      (data  (make-hashtable equal-hash equal?))
      (header "<html><head><title>Add two numbers</title></head><body>")
      (footer "</body></html>")
      (dynamic
"<?spark
  \"Number1=$num1<br>\"
  \"Number2=$num2<br>\"
  \"<b>Result=\"
  (+ $num1 $num2)
  \"</b>\"
?>"))
         

  (test-assert data)

  ; Identity transform.
  (test-equal document
               (sml-parser:execute-embedded-script document data))

  (hashtable-set! data "foo" "bar")

  (test-equal "bar"
               (sml-parser:execute-embedded-script "<?spark \"$foo\" ?>"
                                                   data))

  (hashtable-set! data "num1" "2")
  (hashtable-set! data "num2" "3")

  (test-equal (string-append header
                             "Number1=2<br>Number2=3<br><b>Result=5</b>"
                             footer)
               (sml-parser:execute-embedded-script (string-append header
                                                                  dynamic
                                                                  footer)
                                                   data)))


(test-end "sml-parser")

(test-begin "request-parser")
(let ((req (request-parser:http-request)))
  ; As a predicate isn't exported, the data structure is totally opaque.
  (test-assert req)

  (test-assert request-parser:http-request)

  (test-not-error
   (request-parser:http-request-request! req "GET /some/path HTTP/1.0"))
  (test-equal 'GET (request-parser:http-request-method req))
  (test-equal "/some/path" (request-parser:http-request-uri req))
  (test-equal "HTTP/1.0" (request-parser:http-request-version req))

  ; invalid method
  (test-error
   (request-parser:http-request-request! req "TRACE / HTTP/1.0"))

  (test-not-error
   (request-parser:http-request-header! req "Content-Type: application/xml"))
  (test-equal "application/xml"
              (request-parser:http-request-header req "content-type" 'nonesuch))
  (test-eq 'nonesuch
           (request-parser:http-request-header req "content-length" 'nonesuch))

  (test-not-error
   (request-parser:http-request-data! req "foo=bar&baz=quux&fry=leela"))

  (let ((query-string (request-parser:http-request-data req)))
    (test-assert (hashtable? query-string))
    (test-equal "bar" (hashtable-ref query-string "foo"))
    (test-equal "quux" (hashtable-ref query-string "baz"))
    (test-equal "leela" (hashtable-ref query-string "fry")))
  
  (test-assert (hashtable? (request-parser:http-request-headers req)))

  ; No ability to instantitate these is actually exported!
  ; So just check for really obvious things
  (test-assert (procedure? request-parser:http-parser-error?))
  (test-error (request-parser:http-parser-error-message 2))
)

(test-end)


(test-begin "response")
(test-assert response:make-response)

(let* ((content "Hello, world!")
       (len (string-length content))
       (time (util:current-seconds)))
  (let ((res (response:make-response "HTTP/1.0"
                                     content
                                     len
                                     "text/plain"
                                     time)))
    (test-assert res)
    (test-equal "HTTP/1.0 200 OK" (response:response-status-line res))
    (test-not-error
     (response:response-status-line! res "HTTP/1.1 200 OK"))
    (test-equal "HTTP/1.1 200 OK" (response:response-status-line res))
    (let ((new-headers (make-hashtable equal-hash equal?))
          (old-headers (response:response-headers res)))
      (test-assert (hashtable? old-headers))
      (hashtable-set! new-headers "foo" "bar")
      (test-not-error (response:response-headers! res new-headers))
      (test-equal "bar" (hashtable-ref (response:response-headers res) "foo"))
      (test-not-error (response:response-headers! res old-headers)))

    (test-equal content (response:response-body res))
    (let ((new-content (string-append content "  Hello, universe!")))
      (test-not-error
       (response:response-body! res new-content))
      (test-equal new-content (response:response-body res)))

    (test-equal "text/plain" (response:response-header-value
                 (response:response-headers res) "Content-Type"))
    (let ((new-value "application/xml"))
     (test-not-error (response:response-header-value!
                      (response:response-headers res)
                      "Content-Type" new-value))
     (test-equal new-value
                 (response:response-header-value
                  (response:response-headers res) "Content-Type")))

    (let ((string-version (response:response->string res))
          (full-version (response:response->string res #t)))
      (test-assert (string? string-version))
      (test-assert (> (string-length string-version) len))
      (test-assert (> (string-length full-version)
                      (string-length string-version))))))

(let ((res (response:make-error-response "FAIL"
                                         404
                                         "HTTP/1.0")))
  (test-assert res)
  (test-equal "HTTP/1.0 404 FAIL" (response:response-status-line res)))


(test-assert response:response->string)
(test-end)

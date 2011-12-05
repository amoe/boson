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
        (prefix (boson session) session:)
        (prefix (boson resource-loader) resource-loader:)
        (prefix (boson web-server) web-server:)
        (prefix (boson mime-types) mime-types:)
        (only (srfi :1) make-list)
        (srfi :48)
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
(test-end)


(test-begin "session")

; Sessions must be an eqv hashtable.

; functionalities of SESSION-EXECUTE-PROCEDURE:
;  test the state is added correctly
;  test the session is created if it doesn't exist when passed in
;  test the correct procedure is executed
;  check the session is destroyed if the proc is out of range
;  check that http-keep-alive! works to prevent session destruction
;  check a different procedure is executed if the procedure throws
;    an exception
;  check that the single-procedure call form for PROCS is working

; Some procedures to allow us to easily check the right procedure was called.
(define (constant-session-procedure x)
  (lambda (session-url state) x))

(let ((sessions (make-eqv-hashtable))
      (url "http://www.google.com/")
      (empty-state (make-hashtable equal-hash equal?))
      (non-final-session (make-list 2 (constant-session-procedure #t))))
  (let ((procs (map constant-session-procedure (list 1 2 3)))
        (sess-id "nonexistent-1"))
    (test-eqv 2 (session:session-execute-procedure url
                                                   procs
                                                   sess-id
                                                   1   ; idx?
                                                   empty-state
                                                   sessions)))
  ; A new key has now been added to sessions.
  (test-eqv 1 (hashtable-size sessions))

  (let ((sess-id (vector-ref (hashtable-keys sessions) 0)))
    ; We can only access the state through the medium of dance^Wsession
    ; procedures.
    ; We use two procedures in the list because the session is destroyed if
    ; you execute the last procedure in the list.
    (let ((state (session:session-execute-procedure
                  url
                  (list
                   (lambda (session-url s) s)
                   (lambda (session-url s) s))
                  sess-id
                  0    ; first procedure in list
                  empty-state
                  sessions))
          (additional-state (make-hashtable equal-hash equal?)))
        ; Test adding state in the call.
      (test-false (session-util:http-value state "thisone"))
      (hashtable-set! additional-state "thisone" 42)
      (session:session-execute-procedure url
                                         non-final-session
                                         sess-id
                                         0
                                         additional-state
                                         sessions)
      (test-eqv 42 (session-util:http-value state "thisone")))


    ; Test if this session is destroyed when the final procedure is called.

    (session:session-execute-procedure
     url
     (list (constant-session-procedure #t))
     sess-id
     0
     empty-state
     sessions)
    (test-eqv 0 (hashtable-size sessions)))

    ; Test if HTTP-KEEP-ALIVE! works to prevent session destruction.
  (let ((state (session:session-execute-procedure
                url
                (list
                 (lambda (session-url s) s)
                 (lambda (session-url s) s))
                "nonexistent-2"
                0    ; first procedure in list
                empty-state
                sessions)))
    (let ((sess-id (vector-ref (hashtable-keys sessions) 0)))
      (test-eqv 1 (hashtable-size sessions))
      (session-util:http-keep-alive! state #t)
      (session:session-execute-procedure url
                                         (list (constant-session-procedure #t))
                                         sess-id
                                         0
                                         empty-state
                                         sessions)
      (test-eqv 1 (hashtable-size sessions))))

  ; Test if we can throw an exception to jump to an arbitrary procedure
  (let ((procs
         (list
          (lambda (u s) (raise (session:make-session-jump-condition 2)))
          (lambda (u s) "I will never be reached")
          (lambda (u s) 'some-return-value))))
    (test-eq 'some-return-value
             (session:session-execute-procedure url
                                                procs
                                                "nonexistent-3"
                                                0
                                                empty-state
                                                sessions)))

  ; Make sure the old method errors.
  (letrec ((proc1 (lambda (u s) (raise proc3)))
           (proc2 (lambda (u s) "I will never be reached"))
           (proc3 (lambda (u s) 'should-also-not-be-reached)))
    (test-error error?
      (session:session-execute-procedure url
                                         (list proc1 proc2 proc3)
                                         "nonexistent-3"
                                         0
                                         empty-state
                                         sessions)))

  ; Check the single non-session form works.
  (let ((val 'return-value))
    (let ((ret (session:session-execute-procedure url
                                                    (lambda (s) (cons val s))
                                                    "nonexistent-4"
                                                    #f
                                                    empty-state
                                                    #f)))
      (test-eq val (car ret))
      (test-assert (hashtable? (cdr ret)))))

  ; Test for SESSION-DESTROY
  (test-not-error
    (for-each
     (lambda (sess-id) (session:session-destroy sess-id sessions))
     (vector->list (hashtable-keys sessions))))
  (test-eqv 0 (hashtable-size sessions))
  

  (session:session-execute-procedure
   url
   non-final-session
   "nonexistent-5"
   0
   empty-state
   sessions)

  (let ((sess-id (vector-ref (hashtable-keys sessions) 0)))
    (let ((sess (hashtable-ref sessions sess-id)))
      (test-assert (integer? (session:session-last-access sess))))))
   
(test-end)


(test-begin "resource-loader")
(let ((self (resource-loader:resource-loader))
      (web-server-conf (make-eq-hashtable))
      (http-request (request-parser:http-request))
      (sessions #f))

  (hashtable-set! web-server-conf 'script-ext ".ss")
  (hashtable-set! web-server-conf 'embedded-script-ext ".sml")
  (hashtable-set! web-server-conf 'max-response-size (expt 2 20))

  (request-parser:http-request-request! http-request "GET /test.scm HTTP/1.0")
  (test-assert (record? self))
  (let ((res (resource-loader:resource-loader-load self
                                                   web-server-conf
                                                   http-request
                                                   sessions)))
    (test-assert (record? res))
    (test-assert (bytevector? (resource-loader:resource-content res)))
    (test-assert (string? (resource-loader:resource-content-type res)))
    (test-assert (integer? (resource-loader:resource-content-length res)))
    (test-assert (integer? (resource-loader:resource-content-last-modified res)))))
(test-end "resource-loader")


(test-begin "mime-types")
(test-assert mime-types:find-mime-type)
(let ((res (mime-types:find-mime-type "myfile.html")))
  (test-assert (pair? res))
  (test-assert (string? (car res)))
  (test-assert (string? (cdr res))))
(test-end "mime-types")


(test-begin "web-server")

; Test 3 call forms

(define (delete-file/uncaring filename)
  (guard (ex ((i/o-filename-error? ex)  #t))
    (delete-file filename)))

(define *log-file-name* "test-output.log")
(delete-file/uncaring *log-file-name*)    ; XXX: CAN DELETE USER FILE

(let ((conf '(port 8080))
      (log-port (open-file-output-port *log-file-name*)))
  (let ((ws (web-server:web-server conf log-port)))
    (test-assert (record? ws))
    (close-port log-port)
    (delete-file/uncaring *log-file-name*)))


(test-assert web-server:web-server-start)
(test-assert web-server:web-server-stop)
(test-assert web-server:web-server-socket)
(test-assert web-server:web-server-configuration)
(test-assert web-server:web-server-configuration!)
(test-assert web-server:web-server-hook!)
(test-assert web-server:write-log)
(test-assert web-server:on-client-connect)
(test-end "web-server")

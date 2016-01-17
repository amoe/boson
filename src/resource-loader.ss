; resource-loader.ss - loads resources to be sent to http clients
; Copyright (C) 2007-2010 Vijay Mathew Pandyalakal
; copyright 2011 David Banks <amoebae@gmail.com>
; license: GPL-3+

(library (boson resource-loader)
  (export resource-loader 
          resource-loader-load
          resource-content
          resource-content-type
          resource-content-length
          resource-content-last-modified)
  (import (rnrs)
          (rnrs eval)
          (boson util)
          (boson compat)
          (boson session) 
          (boson request-parser)
          (boson globals)
          (boson sml-parser)
          (boson mime-types)
          (spells pathname)
          (only (spells filesys) working-directory)
          (only (srfi :1) take)
          (only (srfi :13) string-index))

  (define-record-type resource-loader-s
    (fields script-cache
            sml-cache))
  (define-record-type resource-s
    (fields content 
            content-type
            content-length
            content-last-modified))

  (define *debug* #t)


  (define (debug msg)
    (when *debug*
      (display msg)
      (newline)
      (flush-output-port (current-output-port))))


  (define (resource-loader)
    (make-resource-loader-s (make-hashtable equal-hash equal?)
                            (make-hashtable equal-hash equal?)))

  ;; Returns an instance of resource-s
  (define (resource-loader-load self web-server-conf
                                http-request sessions)
    (debug "Inside RESOURCE-LOADER-LOAD")
    (let* ((uri (normalize-uri (http-request-uri http-request)))
           (uri-data (parse-uri uri))
           (root-uri (list-ref uri-data 0))
           (sess-info (list-ref uri-data 1))
           (type (find-res-type root-uri web-server-conf)))
      (let-values (((sz res) 
                    (load-resource self root-uri type web-server-conf)))
        (cond
         ((eq? type 'embedded-script)
          (let ((content (execute-embedded-script 
                          res
                          (http-request-data http-request))))
            (make-resource-s content
                             (string-length content)
                             "text/html"
                             (current-seconds))))
         ((eq? type 'script)
          (let* ((ids (parse-session-info sess-info))
                 (content (execute-resource res root-uri 
                                            (list-ref ids 0)
                                            (list-ref ids 1)
                                            (http-request-data http-request)
                                            sessions)))
            (make-resource-s (string->utf8 content)
                             "text/html"
                             (string-length content)
                             (current-seconds))))
         (else (make-resource-s res
                                (content-type uri)
                                sz
                                (file-or-directory-modify-seconds uri)))))))

  (define (resource-content r)
    (resource-s-content r))
  (define (resource-content-type r)
    (resource-s-content-type r))
  (define (resource-content-length r)
    (resource-s-content-length r))
  (define (resource-content-last-modified r)
    (resource-s-content-last-modified r))

  (define (content-type uri) 
    (let ((mt (find-mime-type uri)))
      (if (not mt)
          "text/html"
          (cdr mt))))
  
  (define (load-resource self uri 
                         type web-server-conf)
    (debug "Inside LOAD-RESOURCE")
    (case type
      ((embedded-script) 
       (read-sml self uri web-server-conf))
      ((file) 
       (read-fresh-file uri web-server-conf))
      ((script) 
       (let ((cache 
              (hashtable-ref (resource-loader-s-script-cache self)
                             uri #f)))
         (if cache
             (values 0 cache)
             (values 0 (read-fresh-script self uri)))))))

  (define (read-sml self uri web-server-conf)
    (let ((cache (hashtable-ref (resource-loader-s-sml-cache self)
                                uri #f)))
      (if cache
          (values (car cache) (cdr cache))
          (let-values (((sz content)
                        (read-fresh-file uri web-server-conf)))
            (hashtable-set! (resource-loader-s-sml-cache self)
                            uri (cons sz content))
            (values sz content)))))

  (define (read-fresh-file uri web-server-conf)
    (debug "About to call FILE-SIZE-IN-BYTES")
    (let ((sz (file-size-in-bytes uri)))
      (debug "Called FILE-SIZE-IN-BYTES")
      (when (> sz (hashtable-ref web-server-conf 'max-response-size #f))
            (raise "Response will exceed maximum limit."))
      (values sz
              (get-bytevector-all
               (open-file-input-port uri)))))
  
  (define (read-fresh-script self uri)
    (let ((ret (load-servlet uri)))
      (hashtable-set! (resource-loader-s-script-cache self)
                      uri ret)
      ret))

  (define (uri->library uri)
    (list
     (string->symbol
      (file-name (pathname-file (->pathname uri))))))

  (define (load-servlet uri)
    (eval 'start (environment (uri->library uri))))

  (define (normalize-uri uri)
    (if (char=? (string-ref uri 0) #\/)
        (set! uri (string-append "." uri)))
    (if (string=? uri "./")
        (set! uri "./index.html"))

    (when (not (safe-uri? uri))
      (error 'normalize-uri "unsafe URI" uri))
    
    uri)

  (define (safe-uri? uri)
    (let* ((base (pathname-as-directory (working-directory)))
           (new-base (pathname-join base uri)))
      (let* ((prefix (pathname-directory base))
             (prefix-len (length prefix))
             (new-prefix (pathname-directory new-base)))
        (cond
          ((< (length new-prefix) prefix-len) #f)
          (else
           (equal? prefix (take new-prefix prefix-len)))))))
          

  (define (find-res-type uri conf)
    (let ((ext (filename-extension uri)))
      (cond
       ((string=? ext (script-ext conf)) 'script)
       ((string=? ext (embedded-script-ext conf)) 'embedded-script)
       (else 'file))))

  (define *ss-script-ext* #f)
  (define *embedded-script-ext* #f)

  ; Handle case where the conf doesn't contain a script-ext
  (define (script-ext conf)
    (if (not *ss-script-ext*)
        (set! *ss-script-ext* (hashtable-ref conf 'script-ext #f)))
    *ss-script-ext*)

  (define (embedded-script-ext conf)
    (if (not *embedded-script-ext*)
        (set! *embedded-script-ext* (hashtable-ref conf 'embedded-script-ext #f)))
    *embedded-script-ext*)

  (define (parse-uri uri)
    (let ((idx (string-index uri *sess-id-sep*)))
      (if (not idx)
          (let ((idx (string-index uri #\?)))
            (if (not idx)
                (list uri #f)
                (list (substring uri 0 idx) #f)))
          (list (substring uri 0 idx)
                (find-session-info uri (+ idx 1))))))

  (define (find-session-info uri start-idx)
    (let ((idx (string-index uri *sess-id-sep* start-idx)))
      (if (not idx) #f
          
          (substring uri start-idx idx))))

  (define (parse-session-info sess-info)
    (if (not sess-info)
        (list -1 0)
        (let ((idx (string-index sess-info #\.)))
          (if (not idx)
              (list -1 0)
              (let ((num1 (string->number (substring sess-info 0 idx)))
                    (num2 (string->number (substring sess-info
                                                     (+ idx 1)
                                                     (string-length
                                                      sess-info)))))
                (list num1 num2))))))

  (define (execute-resource res uri 
                            sess-id proc-count 
                            http-data sessions)
    (let ((content 
           (session-execute-procedure 
            uri res sess-id 
            proc-count http-data sessions)))
      content))

)

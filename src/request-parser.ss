(library (boson request-parser)
  (export http-request
          http-request-request!
          http-request-header!
          http-request-data!
          http-request-method
          http-request-uri
          http-request-version
          http-request-headers
          http-request-header
          http-request-data

          ;; Exception
          http-parser-error?
          http-parser-error-message)

  (import (rnrs)
          (boson compat)
          (boson util)
          (boson url-encode)
          (spells string-utils)
          (only (srfi :13) string-index
                string-trim-both))

  (define-record-type http-request-s
    (fields
     (mutable method 
              http-request-s-method set-http-request-s-method!)
     (mutable uri
              http-request-s-uri set-http-request-s-uri!)
     (mutable version
              http-request-s-version set-http-request-s-version!)
     headers
     (mutable data
              http-request-s-data set-http-request-s-data!)))

  (define-record-type http-parser-error (fields message))

  (define (http-request)
    (make-http-request-s 'GET 
                         "/"
                         "HTTP/1.1"
                         (make-hashtable equal-hash equal?)
                         '()))

  (define (http-request-method self) (http-request-s-method self))
  (define (http-request-uri self) (http-request-s-uri self))
  (define (http-request-version self) (http-request-s-version self))
  (define (http-request-headers self) (http-request-s-headers self))
  (define (http-request-data self) (http-request-s-data self))
  (define (http-request-header self key default-value)
    (hashtable-ref (http-request-s-headers self) key default-value))

  ;; "method uri http-version" -> self!
  (define (http-request-request! self line)
    (let ((tokens (string-split line #\space)))
      (if (not (= (length tokens) 3))
          (raise (make-http-parser-error "Invalid HTTP request.")))
      (let-values (((t1 t2 t3) (apply values tokens)))
        (set-http-request-s-method! self
                                    (assert-method (string->symbol t1)))
        (let ((uri (string-trim-both t2)))
          (set-http-request-s-uri! self uri)
          (set-http-request-s-data! self
                                    (parse-request-data
                                     (extract-request-data uri))))
        (set-http-request-s-version! self t3))))

  ;; "key: value" -> self!
  (define (http-request-header! self line)
    (let ((idx (string-index line #\:)))
      (if idx
          (let ((key (substring line 0 idx))
                (value (string-trim-both
                        (substring line
                                   (+ idx 1)
                                   (string-length line)))))
            (hashtable-set! (http-request-s-headers self) 
                            (string-downcase key) value))
          (raise (make-http-parser-error "Invalid header.")))))

  ;; parse-body input-stream -> text
  (define (http-request-data! self body)
    (set-http-request-s-data! 
     self (parse-request-data body)))

  (define (extract-request-data url)
    (let ((idx (string-index url #\?)))
      (if idx
          (substring url (+ idx 1) (string-length url))
          #f)))

  (define (parse-request-data req)
    (if req
        (let ((key-values (string-split req #\&))
              (ret (make-hashtable equal-hash equal?)))
          (let loop ((kw key-values))
            (if (not (null? kw))
                (let* ((split (string-split (car kw) #\=))
                       (key (url-decode (car split)))
                       (value (url-decode (car (cdr split)))))
                  (hashtable-set! ret key value)
                  (loop (cdr kw)))))
          ret)
        (make-hashtable equal-hash equal?)))

  (define (assert-input-line in)
    (let ((line (get-line in)))
      (if (eof-object? line) 
          #f
          line)))

  ;; As of now, we support only GET and POST.
  (define (assert-method method)
    (if (not (or (eq? method 'GET)
                 (eq? method 'POST)))
        (raise "Method not supported.")
        method))
)

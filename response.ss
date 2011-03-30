(library (response)
  (export make-response 
          make-error-response
          response-status-line
          response-status-line!
          response-headers
          response-headers!
          response-body
          response-body!
          response-header-value
          response-header-value!
          response->string)
  (import (rnrs)
          (compat)
          (util)
          (srfi :19))
  
  (define-record-type response-s
    (fields
     (mutable status-line
              response-s-status-line set-response-s-status-line!)
     (mutable headers
              response-s-headers set-response-s-headers!)
     (mutable body
              response-s-body set-response-s-body!)))
  
  (define crlf "\r\n")

  (define (make-response version 
                         content
                         content-length 
                         content-type
                         content-mod)
    (let ((headers (make-hashtable equal-hash equal?)))
      (hashtable-set! headers "Date"
                      (date-str (gmt-date (current-seconds))))
      (hashtable-set! headers "Content-Type" content-type)
      (hashtable-set! headers "Content-Length"
                      content-length)
      (hashtable-set! headers "Last-Modified"
                      (date-str (gmt-date content-mod)))
      (make-response-s 
       (call-with-string-output-port
        (lambda (status-line)
          (fprintf status-line "~a 200 OK" version)))
       headers
       content)))

  (define (make-error-response error-message 
                               error-code 
                               version)
    (let ((headers (make-hashtable equal-hash equal?)))
      (hashtable-set! headers "Date"
                      (date-str (gmt-date (current-seconds))))
      (hashtable-set! headers "Content-Type" "text/html")
      (hashtable-set! headers "Content-Length"
                      (string-length error-message))
      (make-response-s
       (call-with-string-output-port
        (lambda (status-line)
          (fprintf status-line "~a ~a ~a" 
                   version 
                   error-code
                   error-message)))
       headers
       error-message)) )

  (define (response-status-line r) 
    (response-s-status-line r))
  (define (response-status-line! r s)
    (set-response-s-status-line! r s))
  (define (response-headers r) 
    (response-s-headers r))
  (define (response-headers! r h)
    (set-response-s-headers! r h))
  (define (response-body r)
    (response-s-body r))
  (define (response-body! r b)
    (set-response-s-body! r b))
  (define (response-header-value headers key)
    (hashtable-ref headers key))
  (define (response-header-value! headers key value)
    (hashtable-set! headers key value))

  (define response->string
    (case-lambda
     ((resp)
      (response->string resp #f))
     ((resp with-body)
      (call-with-string-output-port
       (lambda (out)
         (fprintf out "~a~a"
                  (response-s-status-line resp)
                  crlf)
         (hashtable-for-each 
          (response-s-headers resp)
          (lambda (k v)
            (fprintf out "~a: ~a~a" k v crlf)))
         (fprintf out "~a" crlf)
         (if with-body
             (fprintf out "~a" (response-s-body resp))))))))

  (define (date-str d) 
    (call-with-string-output-port
     (lambda (out)
       (fprintf out "~a, ~a ~a ~a ~a:~a:~a GMT"
                (week-day->string (date-week-day d))
                (date-day d)
                (month->string (date-month d))
                (date-year d)
                (date-hour d)
                (date-minute d)
                (date-second d)))))

  (define (gmt-date secs)
    (time-monotonic->date (make-time time-monotonic 0 secs)))

  (define (week-day->string wd)
    (case wd
      ((0) "Sun")
      ((1) "Mon")
      ((2) "Tue")
      ((3) "Wed")
      ((4) "Thu")
      ((5) "Fri")
      (else "Sat")))

  (define (month->string mon)
    (case mon
      ((1) "Jan")
      ((2) "Feb")
      ((3) "Mar")
      ((4) "Apr")
      ((5) "May")
      ((6) "Jun")
      ((7) "Jul")
      ((8) "Aug")
      ((9) "Sep")
      ((10) "Oct")
      ((11) "Nov")
      (else "Dec")))

  (define (error->string error-code)
    (case error-code
      ((200) "OK")
      ((401) "Not Found")
      ((301) "Moved Permanently")
      ((302) "Moved Temporarily")
      ((303) "See Other")
      (else "Server Error"))))

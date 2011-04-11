(library (session-util)
  (export http-value
          http-value! 
          http-call
          http-keep-alive!
          http-keep-alive?
          http-share-state!
          http-share-state?
          make-default-session-state)
  (import (rnrs))

  (define *keep-alive* "*keep-alive*")
  (define *session-id* "*sesssion-id*")
  (define *share-state* "*share-state*")

  (define http-value
    (case-lambda
     ((state name) (http-value state name #f))
     ((state name def-value) (hashtable-ref state name def-value))))

  (define (http-value! state name value)
    (hashtable-set! state name value))	 

  (define (http-call proc)
    (if (not (procedure? proc))
        (raise "http-call failed. Not a procedure.")
        (raise proc)))

  (define (http-keep-alive! state flag)
    (hashtable-set! state *keep-alive* flag))

  (define (http-share-state! state flag)
    (hashtable-set! state *share-state* flag))

  (define (http-keep-alive? state)
    (hashtable-ref state *keep-alive* #f))

  (define (http-share-state? state)
    (hashtable-ref state *share-state* #t))
  
  (define (make-default-session-state id)
    (let ((state (make-hashtable equal-hash equal?)))
      (hashtable-set! state *session-id* id)
      (hashtable-set! state *keep-alive* #f)
      (hashtable-set! state *share-state* #t)
      state)))

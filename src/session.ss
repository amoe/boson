; session.ss - http session
; Copyright (C) 2007-2010 Vijay Mathew Pandyalakal
; copyright 2011 David Banks <amoebae@gmail.com>
; license: GPL-3+

(library (boson session)
  (export session-execute-procedure
          session-destroy
          session-last-access
          make-session-jump-condition)
  (import (rnrs)
          (boson util)
          (boson globals)
          (boson session-util)
          (boson compat)
          (only (srfi :1) list-index)
          (only (srfi :13) string-index string-index-right))

  (define-record-type session-s
    (fields id
            url
            (mutable state
                     session-s-state set-session-s-state!)
            (mutable last-access
                     session-s-last-access set-session-s-last-access!)))

  ; Used to implement a non-local control transfer for session procedures, as
  ; the previous system relied on procedure equality which is not portable under
  ; R6RS.
  (define-condition-type &session-jump &condition
    make-session-jump-condition session-jump-condition?
    (index condition-index))

  (define *session-id* 0)
  (define *vars-sep* #\?)

  (define (session-last-access session)
    (session-s-last-access session))

  (define (session-create script-url sessions) 
    (let* ((sess-id (next-session-id))
           (sess (make-session-s sess-id
                                 (make-session-url script-url sess-id 0)
                                 (make-default-session-state sess-id)
                                 (current-seconds)))
           (state (make-default-session-state sess-id))) ; FIXME redundant
      (hashtable-set! sessions sess-id sess)
      sess))

  (define (session-destroy id sessions)
    (hashtable-delete! sessions id))
  
  (define (session-remap sess sessions)
    (let ((new-sess (find-session -1 (session-s-url sess)
                                  sessions)))
      (set-session-s-state! new-sess (session-s-state sess))
      (session-destroy (session-s-id sess) sessions)
      new-sess))

  (define (session-execute-procedure url procs 
                                     sess-id
                                     p-count
                                     state-to-add
                                     sessions)
    (if (procedure? procs) ;; execute-procedure-without-session
        (procs state-to-add)
        (let* ((sess (find-session sess-id url sessions))
               (id (session-s-id sess))
               (procs-len (length procs)))
          (let ((proc-count (+ p-count 1)))
            (let ((state (session-s-state sess)) 
                  (res-html #f))
              (hashtable-for-each
               state-to-add 
               (lambda (k v)
                 (hashtable-set! state k v)))
              (guard (ex ((session-jump-condition? ex)
                          (set! proc-count (condition-index ex))
                          (set! res-html
                                (session-execute-procedure
                                 url procs sess-id proc-count state-to-add
                                 sessions)))
                         ((procedure? ex)
                          (error 'session-execute-procedure
                                 "do not raise procedures for jumps, see doc")))
                     (when (not (http-share-state? state))
                           (set! sess (session-remap sess sessions))
                           (set! id (session-s-id sess)))
		     ; NB: This is where the servlet procedure is actually
		     ; (finally) invoked.
		     (let ((servlet-proc (list-ref procs (- proc-count 1)))
			   (session-url (make-session-url url id proc-count)))
		       (set! res-html (servlet-proc session-url state))))
              (when (>= proc-count procs-len)
                  (when (not (http-keep-alive? state))
                      (session-destroy id sessions)))
              res-html))))

  ; Using eq? with procedures is totally not portable.
  ; What should we do?
  (define (find-proc-index proc procs-list)
    (or 
     (list-index (lambda (p) (eq? p proc)) procs-list)
     0))

  (define (next-session-id)
    (let ((new (+ *session-id* 1)))
      (set! *session-id* new)
      new))

  (define (find-session id url sessions)
    (let ((sess (hashtable-ref sessions id #f)))
      (when (not sess)
            (set! sess (session-create url sessions)))
      (if (not sess)
          (raise "Invalid session")
          (set-session-s-last-access! sess (current-seconds)))
      sess))

  (define (make-session-url url sess-id proc-count)
    (let ((norm (normalize-url url)))
      (string-append
       norm
       (call-with-string-output-port
        (lambda (out)
          (fprintf out "~a~a.~a~a.ss" 
                   *sess-id-sep* sess-id proc-count *sess-id-sep*))))))

  (define (find-session-id url)
    (let ((idx (string-index url *sess-id-sep*)))
      (if (not idx)
          -1
          (let ((end-idx (string-index url *sess-id-sep* (+ idx 1))))
            (if (and end-idx (> end-idx 0))
                (substring url (+ idx 1) end-idx)
                -1)))))

  (define (normalize-url url)
    (let ((idx (string-index-right url #\/)))    ; find last forward slash
      (if idx
          (substring url (+ idx 1) (string-length url))
          url)))

  (define (remove-vars url)
    (let ((idx (string-index url *vars-sep*)))
      (if (and idx (>= idx 0))
          (substring url 0 idx)
          url))))

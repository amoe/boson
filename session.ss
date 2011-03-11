;; HTTP session.
;; Copyright (C) 2007-2010 Vijay Mathew Pandyalakal
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
  
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
  
;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.
  
;; Please contact Vijay Mathew Pandyalakal if you need additional 
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

(library (session)
	 
	 (export session-execute-procedure
		 session-destroy
		 session-last-access)

	 (import (rnrs)
                 (util)
                 (globals)
                 (session-util))

	 (define-record-type session-s
           (fields id
                   url
                   (mutable state
                            session-s-state set-session-s-state!)
                   (mutable last-access
                            session-s-last-access set-session-s-last-access!)))

	 (define *session-id* 0)
	 (define *vars-sep* "?")

	 (define (session-last-access session)
	   (session-s-last-access session))

	 (define (session-create script-url sessions) 
	   (let* ((sess-id (next-session-id))
		  (sess (make-session-s sess-id
					(make-session-url script-url sess-id 0)
					(make-default-session-state sess-id)
					(current-seconds)))
		  (state (make-default-session-state sess-id)))
	     (hash-table-put! sessions sess-id sess)
	     sess))

	 (define (session-destroy id sessions)
	   (hash-table-remove! sessions id))
	 
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
		 (let ((proc-count (add1 p-count)))
		   (let ((state (session-s-state sess)) 
			 (res-html null))
		     (hash-table-map state-to-add 
				     (lambda (k v) (hash-table-put! state k v)))
		     (try
		      (cond ((not (http-share-state? state))
			     (set! sess (session-remap sess sessions))
			     (set! id (session-s-id sess))))
		      (set! res-html ((list-ref procs (sub1 proc-count))
				      (make-session-url url id proc-count) 
				      state))
		      (catch (lambda (ex)
			       (cond ((procedure? ex)
				      (set! proc-count (find-proc-index ex procs))
				      (set! res-html 
					    (session-execute-procedure url procs
								       sess-id 
								       proc-count
								       state-to-add
								       sessions)))
				     (else (raise ex))))))
		     (if (>= proc-count procs-len)
			 (if (not (http-keep-alive? state))
			     (session-destroy id sessions)))
		     res-html)))))

	 (define (find-proc-index proc procs-list)
	   (let ((ret 0))
	     (let loop ((plist procs-list) (i 0))
	       (cond ((not (null? plist))
		      (cond ((eq? proc (car plist))
			     (set! ret i)
			     (loop null i))
			    (else (loop (cdr plist) (add1 i)))))))
	     ret))

	 (define (next-session-id)
	   (let ((id null))
	     (set! *session-id* (+ *session-id* 1))
	     (set! id *session-id*)
	     id))

	 (define (find-session id url sessions)
	   (let ((sess (hash-table-get sessions id null)))
	     (if (null? sess) (set! sess (session-create url sessions)))
	     (if (null? sess) (raise "Invalid session")
		 (set-session-s-last-access! sess (current-seconds)))
	     sess))		       

	 (define (make-session-url url sess-id proc-count)
	   (let ((out (open-output-string)))
	     ;; TODO: need to find a way to recycle old sessions-ids.
	     (set! url (normalize-url url))
	     (fprintf out "~a~a.~a~a.ss" 
		      *sess-id-sep* sess-id proc-count *sess-id-sep*)
	     (string-append url (get-output-string out))))

	 (define (find-session-id url)
	   (let ((idx (string-find url *sess-id-sep*)))
	     (if (= idx -1) -1
		 (let ((end-idx (string-find url (+ idx 1) *sess-id-sep*)))
		   (if (> idx 0)
		       (string->number (substring url (+ idx 1) end-idx))
		       -1)))))

	 (define (normalize-url url)
	   (let ((idx (string-rfind url "/")))
	     (if (= idx -1) url
		 (substring url (add1 idx)))))

	 (define (remove-vars url)
	   (let ((idx (string-find url *vars-sep*)))
	     (if (>= idx 0) (substring url 0 idx) url))))

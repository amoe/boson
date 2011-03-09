;; Some utilities that makes working with sessions easier.
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

(library (session-util)

	 (export http-value http-value! 
		 http-call http-keep-alive!
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
	   (let ((state (make-hash-table 'equal)))
	     (hashtable-set! state *session-id* id)
	     (hashtable-set! state *keep-alive* #f)
	     (hashtable-set! state *share-state* #t)
	     state)))

;; HTTP request parsing.
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

(library (request-parser)
	 (export http-request
		 http-request-request!
		 http-request-header!
		 http-request-data!
		 ;http-parse
		 http-request-method
		 http-request-uri
		 http-request-version
		 http-request-headers
		 http-request-header
		 http-request-data
		 http-request->string
		 ;; Exception
		 http-parser-error?
		 http-parser-error-message)

	 (import (rnrs)
                 (url-encode)
                 (spells string-utils)
                 (only (srfi :13) string-trim-both))

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
	   (let ((idx (string-find line ":")))
	     (if (not (= idx -1))
		 (let ((key (substring line 0 idx))
		       (value (string-trim (substring line (+ idx 1)))))
		   (hash-table-put! (http-request-s-headers self) 
				    (string-downcase key) value))
		 (raise (make-http-parser-error "Invalid header.")))))

	 ;; parse-body input-stream -> text
	 (define (http-request-data! self body)
	   (set-http-request-s-data! 
	    self (parse-request-data body)))

	 (define (extract-request-data url)
	   (let ((idx (string-find url "?")))
	     (if (> idx 0)
		 (substring url (+ idx 1))
		 null)))

	 (define (parse-request-data req)
	   (if (not (null? req))	       
	       (let ((key-values (string-split req #\&))
		     (ret (make-hashtable equal-hash equal?)))
		 (let loop ((kw key-values))
		   (if (not (null? kw))
		       (let* ((split (string-split (car kw) #\=))
			      (key (url-decode (car split)))
			      (value (url-decode (car (cdr split)))))
			 (hash-table-put! ret key value)
			 (loop (cdr kw)))))
		 ret)
	       (make-hashtable equal-hash equal?)))

	 (define (assert-input-line in)
	   (let ((line (read-line in 'return-linefeed)))
	     (if (eof-object? line) 
		 null
		 line)))

	 ;; As of now, we support only GET and POST.
	 (define (assert-method method)
	   (if (not (or (eq? method 'GET)
			(eq? method 'POST)))
	       (raise "Method not supported.")
	       method))

	 (define (http-request->string self)
	   (let ((out (open-output-string)))
	     (fprintf out "~a ~a ~a~n" 
		      (http-request-s-method self)
		      (http-request-s-uri self)	     
		      (http-request-s-version self))
	     (hash-table-map (http-request-s-headers self)
			     (lambda (k v) (fprintf out "~a: ~a~n" k v)))
	     (fprintf out "~n")
	     (if (not (null? (http-request-s-data self)))
		 (fprintf out "~a" (http-request-s-data self)))
	     (get-output-string out))))


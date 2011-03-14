;; Loads resources to be send to HTTP clients.
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

(library (resource-loader)

	 (export resource-loader 
		 resource-loader-load
		 resource-content
		 resource-content-type
		 resource-content-length
		 resource-content-last-modified)

	 (import (rnrs)
                 (util)
                 (compat)
                 (session) 
		 (request-parser)
		 (globals)
		 (sml-parser)
		 (mime-types))

	 (define-record-type resource-loader-s
           (fields script-cache
                   sml-cache))
	 (define-record-type resource-s
           (fields content 
                   content-type
                   content-length
                   content-last-modified))

	 (define (resource-loader)
	   (make-resource-loader-s (make-hashtable equal-hash equal?)
				   (make-hashtable equal-hash equal?)))

	 ;; Returns an instance of resource-s
	 (define (resource-loader-load self web-server-conf
				       http-request sessions)
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
		   (make-resource-s content 
				    (string-length content)
				    "text/html"
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
	     (if (not mt) "text/html"
		 (cdr mt))))
	 
	 (define (load-resource self uri 
				type web-server-conf)
	   (case type
	     ((embedded-script) 
	      (read-sml self uri web-server-conf))
	     ((file) 
	      (read-fresh-file uri web-server-conf 'bytes))
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
                               (read-fresh-script uri web-server-conf 'string)))
                   (hashtable-set! (resource-loader-s-sml-cache self)
                                   uri (cons sz content))
                   (values sz content)))))
                   
	 (define (read-fresh-file uri web-server-conf mode)	   
	   (let ((sz (file-size-in-bytes uri)))		 
	     (if (> sz (hashtable-ref web-server-conf 'max-response-size #f))
		 (raise "Response will exceed maximum limit."))
	     (let ((file null) (err null) (data null))
	       (try
		(set! file (open-input-file uri))
		(case mode
		  ((string) (set! data (read-string sz file)))
		  (else (set! data (read-bytes sz file))))
		(catch (lambda (ex) (set! err ex))))
	       (if (not (null? file)) (close-input-port file))
	       (if (not (null? err)) (raise err))
	       (values sz data))))
	 
	 (define (read-fresh-script self uri)
	   (let ((ret (load uri)))
	     (hashtable-set! (resource-loader-s-script-cache self)
			      uri ret)
	     ret))

	 (define (normalize-uri uri)
	   (if (char=? (string-ref uri 0) #\/)
	       (set! uri (string-append "." uri)))
	   (if (string=? uri "./")
	       (set! uri "./index.html"))
	   uri)

	 (define (find-res-type uri conf)
	   (let ((ext (filename-extension uri)))
	     (if (bytes? ext)
		 (cond ((bytes=? ext (script-ext conf)) 'script)
		       ((bytes=? ext (embedded-script-ext conf)) 'embedded-script)
		       (else 'file)))))

	 (define *ss-script-ext* null)
	 (define *embedded-script-ext* null)

	 (define (script-ext conf)
	   (if (null? *ss-script-ext*)
	       (set! *ss-script-ext* (hashtable-ref conf 'script-ext #f)))
	   *ss-script-ext*)

	 (define (embedded-script-ext conf)
	   (if (null? *embedded-script-ext*)
	       (set! *embedded-script-ext* (hashtable-ref conf 'embedded-script-ext #f)))
	   *embedded-script-ext*)

	 (define (parse-uri uri)
	   (let ((idx (string-find uri *sess-id-sep*)))
	     (if (= idx -1) 
		 (let ((idx (string-find uri "?")))
		   (if (= idx -1)
		       (list uri null)
		       (list (substring uri 0 idx) null)))
		 (list (substring uri 0 idx) 
		       (find-session-info uri (add1 idx))))))

	 (define (find-session-info uri start-idx)
	   (let ((idx (string-find uri start-idx *sess-id-sep*)))
	     (if (= idx -1) null
		 (substring uri start-idx idx))))

	 (define (parse-session-info sess-info)
	   (if (null? sess-info) (list -1 0)	       
	       (let ((idx (string-find sess-info ".")))
		 (if (= idx -1) (list -1 0)
		     (let ((num1 (string->number (substring sess-info 0 idx)))
		       (num2 (string->number (substring sess-info (add1 idx)))))
		       (list num1 num2))))))

	 (define (execute-resource res uri 
				   sess-id proc-count 
				   http-data sessions)
	   (let ((content 
		  (session-execute-procedure 
		   uri res sess-id 
		   proc-count http-data sessions)))
	     content)))

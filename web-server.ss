;; An HTTP server.
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

;; TODO:
;;  * Test hooks.

(library (web-server)
	 (export web-server
		 web-server-start
		 web-server-stop
		 web-server-socket
		 web-server-configuration
		 web-server-configuration!
		 web-server-hook!
		 write-log)		 

	 (import (rnrs)
                 (prefix (request-parser) parser::)
                 (prefix (resource-loader) loader::)
                 (prefix (response) response::)
                 (prefix (session) session::))

	 (define-record-type web-server-s
           (fields configuration
                   resource-loader
                   sessions
                   (mutable server-socket
                            web-server-s-server-socket
                            set-web-server-s-server-socket!)
                   (mutable hooks
                            web-server-s-hooks
                            set-web-server-s-hooks!)
                   log-port))

	 (define *HTTP-VERSION* "HTTP/1.0")

	 ;; (list) -> web-server
	 ;; Creates a new web-server object. The list argument
	 ;; contains key-values that make up the web-server configuration.
	 ;; Valid key-values are
	 ;; 'port integer - The port to listen on. Defaults to 80.
	 ;; 'script-ext string - Script file extention. Defaults to "ss".
	 ;; 'embedded-script-ext string - Embedded script file extention. 
	 ;;                               Defaults to "sml".
	 ;; 'session-timeout integer - Session timeout in seconds. 
         ;;                            Defaults to 5 minutes.
	 ;; 'max-header-length - Maximum number of bytes that the request
	 ;;                      header can contain. Defaults to 512 Kb
	 ;; 'max-body-length - Maximum number of bytes the body can contain.
	 ;;                    Defaults to 5Mb.
	 ;; 'max-response-size - Maximum size of response. Defaults to 5Mb.
	 ;; E.g.: (web-server (list 'port 8080 'session-timeout 10))
	 (define web-server
	   (case-lambda
	    (()
	     (web-server (list 'port 80) (current-output-port)))
	    ((conf)
	     (web-server conf (current-output-port)))
	    ((conf log-port)
	     (let ((self (make-web-server-s (make-default-conf)
					    (loader::resource-loader)
					    (make-hashtable equal-hash equal?)
					    null null
					    log-port)))		  
               (let loop ((conf conf))
                 (when (not (null? conf))
                   (web-server-configuration! self
                                              (car conf)
                                              (cadr conf))
                   (loop (cddr conf))))

	       (let ((server-socket (socket))
		     (addr (address)))
		 (address-port! addr (web-server-configuration self 'port))
		 (socket-open server-socket)
		 (socket-bind server-socket addr #t)
		 (set-web-server-s-server-socket! self server-socket))
	       self))))

	 ;; (web-server procedure) -> bool
	 ;; Starts the web-server and enters a listen loop.
	 ;; The loop will continue to execute as long as
	 ;; condition-check-proc returns #t.
	 (define web-server-start
	   (case-lambda
	    ((self)
	     (web-server-start self (lambda () #t)))
	    ((self condition-check-proc)
	     (let* ((server-socket (web-server-s-server-socket self))
		    (last-check-secs (current-seconds))
		    (session-timeout-secs (web-server-configuration 
					   self 'session-timeout))
		    (sess-check-proc
		     (lambda ()
		       (let ((cs (current-seconds)))
			 (cond 
			  ((> (- cs last-check-secs)
			      session-timeout-secs)			   
			   (thread (lambda ()
				     (sessions-gc-check self cs 
							session-timeout-secs)))
			   (set! last-check-secs cs)
			   (sleep 0)))))))
	       (socket-listen server-socket)
	       (while (condition-check-proc)
		      (let ((conn (socket-accept server-socket)))
			(thread (lambda () (on-client-connect self conn)))
			(sess-check-proc)
			;; Just to context switch.
			(sleep 0)))))))

	 (define (web-server-stop self)
	   (socket-close (web-server-s-server-socket self)))

	 ;; Hooks could be:
	 ;; 'before-handle-request - (hook-proc web-server-obj 
	 ;;                                     client-connection http-request)
	 ;; If the before-handle-request procedure returns #f, no reponse
	 ;; will be generated by the web-server's handle-request.
	 ;; 'before-send-response - (hook-proc web-server-obj client-connection
	 ;;                                    response-contents)
	 ;; If the before-send-response procedure returns #f, no reponse
	 ;; will be generated by the web-server's handle-request.
	 (define (web-server-hook! self hook-name
				   hook-proc)
	   (let ((hooks (web-server-s-hooks self)))
	     (cond ((null? hooks) 
		    (set! hooks (make-eq-hashtable))
		    (set-web-server-s-hooks! self hooks)))
	     (hashtable-set! hooks hook-name hook-proc)))

	 (define (web-server-socket self)
	   (web-server-s-server-socket self))

	 (define (web-server-configuration self conf-key)
	   (hashtable-ref (web-server-s-configuration self) 
			   conf-key null))

	 (define (web-server-configuration! self 
					    conf-key
					    conf-value)
	   (let ((conf (web-server-s-configuration self)))
	     (hashtable-set! conf conf-key conf-value)))
	 
	 (define (make-default-conf)
	   (let ((conf (make-eq-hashtable)))
	     (hashtable-set! conf 'port 80)
	     (hashtable-set! conf 'script-ext "ss")
	     (hashtable-set! conf 'embedded-script-ext "sml")
	     (hashtable-set! conf 'session-timeout (* 5 60)) ;; 5 minutes
	     (hashtable-set! conf 'max-header-length (* 1024 512)) ;; 512Kb
	     (hashtable-set! conf 'max-body-length (* 1024 5120)) ;; 5Mb
	     (hashtable-set! conf 'max-response-size (* 1024 5120)) ;; 5Mb
	     conf))

	 ;; Called when a new client connection is established.
	 (define (on-client-connect self client-conn)
	   (let ((client-socket (connection-socket client-conn))
		 (conf (web-server-s-configuration self)))
	     (try
	      (let* ((http-request (read-header conf client-socket))
		     (body-str (read-body conf client-socket http-request)))
		(if (> (string-length (string-trim body-str)) 0)
		    (parser::http-request-data! http-request body-str))
		(handle-request self 
				client-conn
				http-request))
	      (catch (lambda (error)
 		       (write-log self
 				  (list "Error: ~a in connection ~a."
 					error
 					(address->string 
 					 (connection-address client-conn))))
		       (let ((str null))
			 (cond
			   ((string? error) (set! str error))
			   ((parser::http-parser-error? error)
			    (set! str (parser::http-parser-error-message error)))
			   (else (set! str (exn-message error))))
			 (return-error self
				       client-conn
				       str
				       conf)))))
	     (try
	      (socket-close client-socket)
	      (catch (lambda (error)
		       (write-log self
				  '("Error: (socket-close): ~a."
				    error)))))))

	 (define (read-header conf client-socket)
	   (let ((max-header-length (hashtable-ref conf 'max-header-length #f))
		 (http-request (parser::http-request))
		 (request-parsed #f))
	     (let loop ((line (socket-recv-line client-socket max-header-length))
			(running-header-length 0))
	       (cond 
		((not (null? line))
		 (cond 
		  ((> running-header-length max-header-length)
		   (raise "Header content is too long."))
		  (else
		   (let ((line-length (string-length line)))
		     (cond 
		      ((> line-length 0)
		       (if (not request-parsed)
			   (begin
			     (parser::http-request-request! http-request line)
			     (set! request-parsed #t))
			   (parser::http-request-header! http-request line))
		       (loop (socket-recv-line client-socket max-header-length)
			     (+ running-header-length line-length))))))))))
	     http-request))

	 (define (read-body conf client-socket http-request)
	   (let ((max-body-length (hashtable-ref conf 'max-body-length #f))
		 (content-length (string->number (parser::http-request-header 
						  http-request "content-length" "0")))
		 (content ""))
	     (if (number? content-length)
		 (if (> content-length 0)
		     (cond ((> content-length max-body-length)
			    (raise "Content is too long."))
			   (else
			    (set! content (socket-recv client-socket content-length))))))
	     content))

	 (define (handle-request self client-conn http-request)
	   (if (invoke-hook self 'before-handle-request
			    (list client-conn http-request))
	       (let* ((content (loader::resource-loader-load
				(web-server-s-resource-loader self)
				(web-server-s-configuration self)
				http-request
				(web-server-s-sessions self)))
		      (response (response::make-response
				 *HTTP-VERSION*
				 (loader::resource-content content)
				 (loader::resource-content-length content)
				 (loader::resource-content-type content)
				 (loader::resource-content-last-modified content))))
		 (send-response self client-conn response))))

	 (define (return-error self
			       client-conn
			       error-message
			       conf)
	   (try
	    (send-response self client-conn
			   (response::make-error-response
			    error-message
			    500 
			    *HTTP-VERSION*))
	    (catch (lambda (error)
		     (write-log self
				'("(return-error): ~a." error))))))
	 
	 (define (send-response self client-conn resp)
	   (cond ((invoke-hook self 'before-send-response
			       (list client-conn resp))
		  (socket-send (connection-socket client-conn) 
			       (response::response->string resp))
		  (socket-send (connection-socket client-conn)
			       (response::response-body resp)))))

	 (define (write-log self entries)
	   (if (not (list? entries))
	       (set! entries (list entries)))
	   (let ((port (web-server-s-log-port self)))
	     (if (not (null? port))
		 (let ((f (list fprintf port)))
		   (for e in entries 
			(set! f (append f (list e))))
		   (eval f)
		   (fprintf port "~n")
		   (flush-output port)))))

	 (define (invoke-hook self hook-name hook-args)
	   (let ((hooks (web-server-s-hooks self))
		 (ret #t))
	     (if (not (null? hooks))
		 (let ((hook-proc (hashtable-ref hooks hook-name null)))
		   (if (not (null? hook-proc))
		       (set! ret (apply hook-proc (cons self hook-args))))))
	     ret))

	 (define (sessions-gc-check self curr-secs
				    session-timeout-secs)
	   (let ((sessions (web-server-s-sessions self))
		 (gc-session-ids (list)))
	     (hashtable-for-each 
	      sessions
	      (lambda (id session)
		(if (> (- curr-secs (session::session-last-access session))
		       session-timeout-secs)		    
		    (set! gc-session-ids (cons id gc-session-ids)))))
	     (for id in gc-session-ids
		  (session::session-destroy id sessions)))))
	       

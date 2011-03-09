;; HTTP response wrapper.
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

(library http-response
	 
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
	 
	 (define-struct response-s (status-line
				    headers
				    body))
	 (define crlf "\r\n")

	 (define (make-response version 
				content
				content-length 
				content-type
				content-mod)
	   (let ((headers (make-hash-table 'equal))
		 (status-line (open-output-string)))
	     (fprintf status-line "~a 200 OK" version)
	     (hash-table-put! headers "Date"
			      (date-str (gmt-date (current-seconds))))
	     (hash-table-put! headers "Content-Type" content-type)
	     (hash-table-put! headers "Content-Length"
			      content-length)
	     (hash-table-put! headers "Last-Modified"
			      (date-str (gmt-date content-mod)))
	     (make-response-s (get-output-string status-line)
			      headers content)))

	 (define (make-error-response error-message 
				      error-code 
				      version)
	   (let ((headers (make-hash-table 'equal))
		 (status-line (open-output-string)))
	     (fprintf status-line "~a ~a ~a" 
		      version 
		      error-code
		      error-message)
	     (hash-table-put! headers "Date"
			      (date-str (gmt-date (current-seconds))))
	     (hash-table-put! headers "Content-Type" "text/html")
	     (hash-table-put! headers "Content-Length"
			      (string-length error-message))
	     (make-response-s (get-output-string status-line)
			      headers error-message)))

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
	   (hash-table-get headers key))
	 (define (response-header-value! headers key value)
	   (hash-table-put! headers key value))

	 (define response->string
	   (case-lambda
	    ((resp)
	     (response->string resp #f))
	    ((resp with-body)
	     (let ((out (open-output-string)))
	       (fprintf out "~a~a"
			(response-s-status-line resp)
			crlf)
	       (hash-table-for-each 
		(response-s-headers resp)
		(lambda (k v)
		  (fprintf out "~a: ~a~a" k v crlf)))
	       (fprintf out "~a" crlf)
	       (if with-body
		   (fprintf out "~a" (response-s-body resp)))
	       (get-output-string out)))))

	 (define (date-str d) 
	   (let ((out (open-output-string)))
	     (fprintf out "~a, ~a ~a ~a ~a:~a:~a GMT"
		      (week-day->string (date-week-day d))
		      (date-day d)
		      (month->string (date-month d))
		      (date-year d)
		      (date-hour d)
		      (date-minute d)
		      (date-second d))
	     (get-output-string out)))

	 (define (gmt-date secs)
	   (let ((d (seconds->date secs)))
	     (seconds->date (- secs (date-time-zone-offset d)))))

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

	     
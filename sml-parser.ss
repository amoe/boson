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

(library (sml-parser)
	 

	 (export execute-embedded-script)
         (import (rnrs)
                 (wak irregex))

	 (define (execute-embedded-script sml-doc http-data)
	   (parse-sml sml-doc http-data))

	 (define *token-regex*
           (string->irregex "\\$\\S[^)(\\[\\]<>|\\\\\"' ]+"))
	 (define *tag-regex*
           (string->irregex "<\\?spark(.*?)\\?>"))
	 (define *start-tag-len* (string-length "<?spark"))
	 (define *end-tag-len* (string-length "?>"))

	 (define (get-var-name s)
	   (if (> (string-length s) 1)
	       (substring s 1)
	       ""))
	 
	 (define (get-value varname state)
	   (hash-table-get state varname ""))

	 (define (replace-tokens expr-str state)
	   (let* ((res (pregexp-match *token-regex* expr-str))
		  (ret expr-str))
	     (while (list? res)
		    (set! ret (pregexp-replace *token-regex* ret 
					       (get-value 
						(get-var-name (car res))
						state)))
		    (set! res (pregexp-match *token-regex* ret)))
	     ret))

	 (define (parse-sml sml-doc state)
	   (let* ((res (pregexp-match *tag-regex* sml-doc))
		  (ret sml-doc))
	     (while (list? res)
		    (set! ret (pregexp-replace *tag-regex*  
					       ret
					       (eval-script (car res) state)))
		    (set! res (pregexp-match *tag-regex* ret)))
	     ret))
	 
	 (define (eval-script script state)
	   (let* ((spark-script (replace-tokens
				 (substring script *start-tag-len* 
					    (- (string-length script) *end-tag-len*))
				 state))
		  (in (open-input-string spark-script))
		  (out (open-output-string))
		  (expr (read in)))
	     (while (not (eof-object? expr))
		    (let ((result (eval expr)))
		      (if (not (void? result))
			  (fprintf out "~a" result)))
		    (set! expr (read in)))
	     (get-output-string out))))

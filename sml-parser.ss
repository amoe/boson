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
	       (substring s 1 (string-length s))
	       ""))
	 
	 (define (get-value varname state)
	   (hashtable-ref state varname ""))

	 (define (replace-tokens expr-str state)
           (let ((match (irregex-search *token-regex* expr-str)))
             (if match
                 (let ((var (irregex-match-substring match)))
                   (replace-tokens
                    (irregex-replace *token-regex* expr-str
                                     (get-value (get-var-name var) state))
                    state))
                 expr-str)))

         (define (parse-sml sml-doc state)
           (let ((match (irregex-search *tag-regex* sml-doc)))
             (if match
                 (let ((script (irregex-match-substring match)))
                   (parse-sml
                    (irregex-replace *tag-regex* sml-doc
                                     (eval-script script state))
                    state))
                 sml-doc)))

	 (define (eval-script script state)
           (call-with-string-output-port
            (lambda (out)
	   (let* ((spark-script (replace-tokens
				 (substring script *start-tag-len* 
					    (- (string-length script) *end-tag-len*))
				 state))
		  (in (open-string-input-port spark-script))
		  (expr (read in)))
             (let loop ()
               (let ((expr (read in)))
                  (when (not (eof-object? expr))
                    (fprintf out "~a" (eval expr))
                    (loop)))))))))
                   


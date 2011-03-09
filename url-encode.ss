;; URL encode, decode procedures.
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

(library (url-encode)

	 (export url-encode url-decode)
         (import (rnrs)
                 (compat))

	 (define (url-encode str)
           (call-with-string-output-port
            (lambda (encoded)
	     (map (lambda (c) 
		    (cond 
		     ((char-whitespace? c) 
		      (fprintf encoded "~a" #\+))
		     ((or (char-symbolic? c)
			  (char-punctuation? c))
		      (fprintf encoded "~a" (encode-char c)))
		     (else (fprintf encoded "~a" c))))
		  (string->list str)))))

	 (define (url-decode str)
           (call-with-string-output-port
            (lambda (decoded)
	   (let ((enc ()) (in-enc #f))
	     (map (lambda (c)
		    (cond 
		     (in-enc
		      (set! enc (cons c enc))
		      (if (= (length enc) 2)
			  (begin
			    (fprintf decoded "~c" 
				     (decode-char (list->string (reverse enc))))
			    (set! enc ())
			    (set! in-enc #f))))
		     ((char=? c #\+) (fprintf decoded "~c" #\space))
		     ((char=? c #\%) (set! in-enc #t))
		     (else (fprintf decoded "~c" c))))
		  (string->list str))))))

	 (define (encode-char c)
           (call-with-string-output-port
            (lambda (out)
	     (fprintf out "%~x" (char->integer c)))))

	 (define (decode-char c)
	   (integer->char (string->number c 16))))



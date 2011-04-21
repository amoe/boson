; Utility code that doesn't find anywhere else and has a portable
; implementation.

(library (boson util)
  (export fprintf
          current-seconds
          hashtable-for-each
          load
          get-line-bytevector)
  (import (rnrs)
          (rnrs eval)
          (prefix (srfi :19) srfi-19:)
          (srfi :48))

  (define *line-feed* #x0a)
  (define *carriage-return* #x0d)
  

  (define (fprintf output-port format-string . rest)
    (display (apply format (cons format-string rest))
             output-port))

  ; return current UTC seconds since epoch
  (define (current-seconds)
    (srfi-19:time-second (srfi-19:current-time)))

  ; "applies the procedure proc to each element in hashtable (for the
  ; side-effects of proc) in an unspecified order and returns void. The
  ; procedure proc must take two arguments: a key and its value."
  (define (hashtable-for-each hashtable proc)
    (let-values (((keys values) (hashtable-entries hashtable)))
      (vector-for-each proc keys values)))

  (define (load file . import-spec)
    (eval (cons 'let (cons '() (read-forms file)))
          (apply environment import-spec)))

  ; read CRLF
  (define (get-line-bytevector binary-input-port)
    (u8-list->bytevector
     (let loop ()
       (let ((byte (get-u8 binary-input-port)))
         (cond
          ((or (= byte *line-feed*)
               (eof-object? byte))
           '())
          ((= byte *carriage-return*)
           (get-u8 binary-input-port)    ; skip the next byte, assumed LF
           '())
          (else
           (cons byte (loop))))))))

  (define (read-forms file)
    (with-input-from-file file
      (lambda ()
        (let loop ()
          (let ((form (read)))
            (if (not (eof-object? form))
                (cons form (loop))
                '()))))))
)

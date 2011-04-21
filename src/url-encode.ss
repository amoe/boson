(library (boson url-encode)
  (export url-decode)
  (import (rnrs)
          (boson compat)
          (boson util))

  (define (url-decode str)
    (call-with-string-output-port
     (lambda (decoded)
       (let ((enc '()) (in-enc #f))
         (map (lambda (c)
                (cond 
                 (in-enc
                  (set! enc (cons c enc))
                  (if (= (length enc) 2)
                      (begin
                        (fprintf decoded "~c" 
                                 (decode-char (list->string (reverse enc))))
                        (set! enc '())
                        (set! in-enc #f))))
                 ((char=? c #\+) (fprintf decoded "~c" #\space))
                 ((char=? c #\%) (set! in-enc #t))
                 (else (fprintf decoded "~c" c))))
              (string->list str))))))

  (define (decode-char c)
    (integer->char (string->number c 16))))



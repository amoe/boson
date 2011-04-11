(library (boson url-encode)
  (export url-encode
          url-decode)
  (import (rnrs)
          (boson compat)
          (boson util))

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

  (define (encode-char c)
    (call-with-string-output-port
     (lambda (out)
       (fprintf out "%~x" (char->integer c)))))

  (define (decode-char c)
    (integer->char (string->number c 16))))



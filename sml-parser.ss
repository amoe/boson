(library (sml-parser)
  (export execute-embedded-script)
  (import (rnrs)
          (rnrs eval)
          (compat)
          (util)
          (wak irregex))

  (define (execute-embedded-script sml-doc http-data)
    (parse-sml sml-doc http-data))

  (define *token-regex*
    (string->irregex "\\$\\S[^)(\\[\\]<>|\\\\\"' ]+"))
  (define *tag-regex*
    (string->irregex "<\\?spark(.*?)\\?>" 'single-line))
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
          (let ((script (irregex-match-substring match 1)))
            (parse-sml
             (irregex-replace *tag-regex* sml-doc
                              (eval-script script state))
             state))
          sml-doc)))

  (define (eval-script script state)
    (call-with-string-output-port
     (lambda (out)
       (let* ((spark-script (replace-tokens script state))
              (in (open-string-input-port spark-script)))
         (let loop ()
           (let ((expr (read in)))
             (when (not (eof-object? expr))
                   (fprintf out "~a" (eval expr
                                           (environment '(rnrs))))
                   (loop)))))))))

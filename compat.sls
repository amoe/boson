; Mosh compatibility layer.  All code should eventually be migrated to
; respective R6RS cross-implementation libraries.

(library (compat)
  (export fprintf
          filename-extension)
  (import (rnrs)
          (irregex))

  (define (fprintf . args) (raise 'unimplemented))

  ; trimmed down version of the one from file.ss
  ;; name can be any string; we just look for a dot
  (define (filename-extension name)
    (cond ((and name (irregex-search "[.]([^.]+)$" name))
           => (lambda (match)
                (irregex-match-substring match 1)))
          (else #f))))

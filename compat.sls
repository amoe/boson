; Mosh compatibility layer.  All code should eventually be migrated to
; respective R6RS cross-implementation libraries.

(library (compat)
  (export fprintf
          filename-extension
          file-or-directory-modify-seconds
          file-size-in-bytes
          thread)
  (import (rnrs)
          (irregex)
          (prefix (mosh file) mosh:)
          (prefix (mosh concurrent) mosh:))

  (define (fprintf . args) (raise 'unimplemented))

  ; trimmed down version of the one from file.ss
  ;; name can be any string; we just look for a dot
  (define (filename-extension name)
    (cond ((and name (irregex-search "[.]([^.]+)$" name))
           => (lambda (match)
                (irregex-match-substring match 1)))
          (else #f)))

  ; Long term: replace call site with use of FILE-MODIFICATION-TIME from
  ; (spells filesys).
  (define (file-or-directory-modify-seconds pathname)
    (/ (mosh:file-stat-mtime pathname) (expt 10 9)))
  
  (define (file-size-in-bytes pathname)
    (mosh:file-size-in-bytes pathname))

  ; NB: Inside the first argument of the macro SPAWN we have no access to
  ; variables in the current namespace, so we can't use a closure - we must
  ; pass the thunk in as an argument.
  (define (thread thunk)
    (mosh:spawn (lambda (p) (p)) thunk '((rnrs) (mosh concurrent))))
)

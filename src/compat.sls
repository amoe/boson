; Mosh compatibility layer.  All code should eventually be migrated to
; respective R6RS cross-implementation libraries.

(library (boson compat)
  (export filename-extension
          file-or-directory-modify-seconds
          file-size-in-bytes)
  (import (rnrs)
          (wak irregex)
          (prefix (mosh file) mosh:)
          (prefix (mosh concurrent) mosh:))


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
)

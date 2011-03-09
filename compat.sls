; Mosh compatibility layer.  All code should eventually be migrated to
; respective R6RS cross-implementation libraries.

(library (compat)
  (export fprintf)
  (import (rnrs))

  (define (fprintf . args) args))

; Utility code that doesn't find anywhere else and has a portable
; implementation.

(library (util)
  (export char-symbolic?)
  (import (rnrs))

  ; returns #t if char's Unicode general category is Sm, Sc, Sk, or So, #f
  ; otherwise.
  (define (char-symbolic? . args) (raise 'unimplemented)))

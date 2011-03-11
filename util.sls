; Utility code that doesn't find anywhere else and has a portable
; implementation.

(library (util)
  (export char-symbolic?
          char-punctuation?
          current-seconds)
  (import (rnrs)
          (prefix (srfi :19) srfi-19:))

  ; returns #t if char's Unicode general category is Sm, Sc, Sk, or So, #f
  ; otherwise.
  (define (char-symbolic? . args) (raise 'unimplemented))
  (define (char-punctuation? . args) (raise 'unimplemented))

  ; return current UTC seconds since epoch
  (define (current-seconds)
    (srfi-19:time-second (srfi-19:current-time)))
)

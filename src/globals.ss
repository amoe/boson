; globals.ss - global state for the http server
; Copyright (C) 2007-2010 Vijay Mathew Pandyalakal
; copyright 2011 David Banks <amoebae@gmail.com>
; license: GPL-3+

(library (boson globals)
  (export *sess-id-sep*)
  (import (rnrs))

  (define *sess-id-sep* #\$)
)

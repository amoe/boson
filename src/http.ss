; http.ss - http packages
; Copyright (C) 2007-2010 Vijay Mathew Pandyalakal
; copyright 2011 David Banks <amoebae@gmail.com>
; license: GPL-3+

(library (boson http)
  (export web-server                    ; web-server.ss 
          web-server-start
          web-server-stop
          web-server-socket
          web-server-configuration
          web-server-configuration!
          web-server-hook!
          write-log

          find-mime-type                ; mime-types.ss

          http-value                    ; session-util.ss
          http-value!        
          http-call
          http-keep-alive!
          http-keep-alive?
          http-share-state!
          http-share-state?
          make-default-session-state)

  (import (boson web-server)
          (boson mime-types)
          (boson session-util)))


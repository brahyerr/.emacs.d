(setq rcirc-server-alist
      '(("irc.libera.chat" :channels ("#emacs" "#rcirc") :port 6697 :encryption tls)
	("irc.atl.chat" :user-name "bhreigher" :channels ("#general") :port 6697 :encryption tls))
      )

(provide 'irc_local)

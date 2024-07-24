(setq rcirc-buffer-maximum-lines 1000
      rcirc-default-nick "pb"
      rcirc-default-user-name "breyers"
      rcirc-log-flag t
      rcirc-server-alist '(("irc.libera.chat" :channels ("#emacs" "#rcirc") :port 6697 :encryption tls)
			   ("irc.atl.chat" :user-name "bhreigher" :channels ("#general") :port 6697 :encryption tls))
      )

(setq erc-server "irc.atl.chat"
      erc-nick "pb"    ; Change this!
      erc-user-full-name "breyers"  ; And this!
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.atl.chat" "#general"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

(use-package circe
  :config
  (add-to-list 'circe-network-defaults
	       '("ATL Chat" :host "irc.atl.chat" :port (6667 . 6697)
		 :tls t
		 :nickserv-mask "^NickServ!NickServ@services\\.atl\\.chat$"
		 :nickserv-identify-challenge "This nickname is registered."
		 :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {nick} {password}"
		 :nickserv-identify-confirmation "^You are now identified for \x02.*\x02\\.$"
		 :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
		 :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"))
  (setq circe-format-self-say "<{nick}> {body}"
	circe-network-options
	'(("ATL Chat"
	   :tls t
	   :nick "pb"
           :sasl-username "breyers"
           :channels ("#general")
         )
	("Libera Chat"
	   :tls t
	   :nick "pb"
           :sasl-username "breyers"
           :channels ("#emacs-circe" "#emacs")
           ))))

(use-package circe-notifications
  :ensure t
  :after circe
  :config
  ;; (autoload 'enable-circe-notifications "circe-notifications" nil t)
  (setq circe-notifications-watch-strings
      '("pb:"))
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications)
  )

(provide 'irc_local)

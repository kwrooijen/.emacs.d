(setq gnus-select-method
      '(nnimap "hover"
	       (nnimap-address "mail.hover.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("mail.hover.com" 587 nil nil))
      smtpmail-auth-credentials '(("mail.hover.com" 587
				   "kevin.van.rooijen@attichacker.com" "PASSWORD"))
      smtpmail-default-smtp-server "mail.hover.com"
      smtpmail-smtp-server "mail.hover.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")


(setq send-mail-function         'smtpmail-send-it
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-smtp-server       "mail.hover.com")

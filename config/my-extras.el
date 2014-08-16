(if (boundp 'my-extra-music) (progn
;=================================================================;
;============================= Music =============================;
;=================================================================;

(require 'emms-setup)

;; Don't use mplayer, after suspend it crashes and violates your ears
(setq emms-setup-default-player-list '(emms-player-vlc))

;; Default config for emms
(emms-standard)
(emms-default-players)

;; Use the Music directory as default playlist
(emms-add-directory-tree "~/Music/")

;; Toggle repeat and shuffle Music playlist
(emms-toggle-repeat-playlist)
(emms-shuffle)

;; Adjust volume by 10 on change
(setq emms-volume-change-amount 5)

;; List of commands for emms "C-c C-a C-?"
;; These command can be repeated by pressing the last
;; pressed key thanks to command-repeater
(define-key attic-minor-mode-map (kbd "C-c C-a")
  (lambda () (interactive) (command-repeater '(
    ("a" . emms-pause)
    ("g" . emms-playlist-mode-go)
    ("n" . emms-next)
    ("f" . emms-next)
    ("p" . emms-previous)
    ("b" . emms-previous)
    ("w" . emms-volume-raise)
    ("q" . emms-volume-lower)
    ("s" . helm-swoop-emms)
    ("[" . emms-seek-backward)
    ("]" . emms-seek-forward)))))

)) ;=================== Music ends here ==========================;

;=================================================================;
;============================== Mail =============================;
;=================================================================;
(if (boundp 'my-extra-mail) (progn

(add-to-list 'load-path "~/.emacs.d/plugins/mu4e")
(add-to-list 'load-path "~/.emacs.d/plugins/helm-mu")
(require 'mu4e)
(require 'helm-mu)

(define-key mu4e-main-mode-map (kbd "p") 'helm-mu-contacts)
(define-key mu4e-main-mode-map (kbd "o") 'helm-mu)
(define-key mu4e-main-mode-map (kbd "z") 'helm-buffers-list)
(define-key mu4e-main-mode-map (kbd "u") 'helm-buffers-list)
(define-key mu4e-main-mode-map (kbd "c s a") 'helm-bookmarks)
(define-key mu4e-main-mode-map (kbd "x f") 'helm-find-files)

;; Update every 2 minutes
(setq mu4e-update-interval 120)

;; Use .authinfo.gpg (encryped) instead of .authinfo
(setq smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))

;; Do not use gpg agent when runing in terminal
(when (file-executable-p "/usr/bin/gpg1") (setq epg-gpg-program "/usr/bin/gpg1"))

;; Mail directory for mu4e to use
(setq mu4e-maildir "~/Mail")

;; Email adres for mu4e to use
(setq mu4e-my-email-addresses '("kevin.van.rooijen@gmail.com"))

;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; Allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; smtp configurations
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "kevin.van.rooijen@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Some settings to check how many unread messages there are
;; This is used with my Powerline to display the amount of unread messages
(defun new-messages ()
  "Check our Maildir for 'new' messages and return the count"
  (let ((cmd (concat "find " (expand-file-name "~/Mail")
                     " -type f | grep -i new | wc -l")))
    (string-to-number (replace-regexp-in-string "![0-9]" "" (shell-command-to-string cmd)))))

;; Set the unread variable
(setq total-unread (new-messages))

;; When mu4e updates mail, update the unread messages afterwards
(defadvice mu4e-update-mail-and-index (after mu4e-update-mail-and-index-after activate)
    (setq total-unread (new-messages)))

)) ;=================== Mail ends here ===========================;

(if (boundp 'my-extra-chat) (progn
;=================================================================;
;============================= Chat ==============================;
;=================================================================;

(require 'elim)
(require 'garak)

;; Garak Keys
(define-key garak-mode-map (kbd "\r") nil)
(define-key garak-mode-map (kbd "M-<RET>") 'lui-send-input)

;; Set elim executable
(setq elim-executable "/usr/bin/elim-client")

;; Use master password for twitter instead of authenticating every time
(setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")
(setq twittering-use-master-password t)

(define-key twittering-mode-map (kbd "s") 'twittering-search)

)) ;=================== Chat ends here ===========================;
(provide 'my-extras)

;=================================================================;
(if (boundp 'my-extra-music) (progn ;; Music =====================;
;=================================================================;

;; List of commands for emms "C-c C-a C-?"
;; These command can be repeated by pressing the last
;; pressed key thanks to command-repeater

(define-key attic-minor-mode-map (kbd "C-c C-a")
  (lambda () (interactive) (command-repeater '(
    ("a" . spotify-playpause)
    ("g" . spotify-playlist-mode-go)
    ("n" . spotify-next)
    ("f" . spotify-next)
    ("p" . spotify-playpause)
    ("b" . spotify-previous)
    ("w" . spotify-volume-raise)
    ("q" . spotify-volume-lower)
    ("s" . helm-swoop-emms)
    ("[" . spotify-seek-backward)
    ("]" . spotify-seek-forward)))))

)) ;=================== Music ends here ==========================;

(require 'twittering-mode)
(setq twittering-icon-mode t)    
;; Use master password for twitter instead of authenticating every time
(setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")
(setq twittering-use-master-password t)

(define-key twittering-mode-map (kbd "s") 'twittering-search)
(define-key twittering-mode-map (kbd ";") 'semi-colon-map)

(provide 'my-extras)


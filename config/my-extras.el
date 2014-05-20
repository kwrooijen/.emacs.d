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

(provide 'my-extras)

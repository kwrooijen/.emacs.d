(require 'emms-setup)

(emms-standard)
(emms-default-players)
(emms-toggle-repeat-playlist)
(emms-add-directory-tree "~/Music")
(emms-shuffle)

(setq emms-volume-change-amount 10)

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

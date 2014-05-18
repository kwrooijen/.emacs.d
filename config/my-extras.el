(require 'emms-setup)

(emms-standard)
(emms-default-players)
(emms-toggle-repeat-playlist)
(emms-add-directory-tree "~/Music")
(emms-shuffle)

(setq emms-volume-change-amount 10)

(define-key attic-minor-mode-map (kbd "C-c C-a") 'repeat-command)

(setq repeated-char nil)

(defun repeat-command (_ char)
  (interactive "p\nc ")
  (if (or (equal repeated-char nil) (equal char repeated-char))
      (progn
        (pcase (string char)
          ("a" (call-interactively 'emms-pause))
          ("g" (call-interactively 'emms-playlist-mode-go))
          ("n" (call-interactively 'emms-next))
          ("p" (call-interactively 'emms-previous))
          ("q" (call-interactively 'emms-volume-raise))
          ("w" (call-interactively 'emms-volume-lower))
          ("s" (call-interactively 'helm-swoop-emms))
          ("[" (call-interactively 'emms-seek-backward))
          ("]" (call-interactively 'emms-seek-forward))
          )
        (setq repeated-char char)
        (call-interactively 'repeat-command))
      (progn
        (setq repeated-char nil)
        (call-interactively (key-binding (kbd (string char))))
        )))

(defun helm-swoop-emms ()
  (interactive)
  (setq current (current-buffer))
  (split-window)
  (other-window 1)
  (emms-playlist-mode-go)
  (setq current-l (what-line))
  (helm-swoop :$query "")
  (unless (equal current-l (what-line))
    (emms-playlist-mode-play-smart))
  (delete-window)
  (switch-to-buffer current)
)



(provide 'my-extras)

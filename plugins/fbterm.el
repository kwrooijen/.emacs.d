;; -*- no-byte-compile: t -*-

(load "term/xterm")

(defun terminal-init-fbterm ()
  "Terminal initialization function for linux fbterm."
  (unless (terminal-coding-system)
    (set-terminal-coding-system 'utf-8-unix))

  ;; It can't really display underlines.
  (tty-no-underline)

  (ignore-errors (when gpm-mouse-mode (require 't-mouse) (gpm-mouse-enable)))

  (xterm-register-default-colors))
(provide 'fbterm)
;;; fbterm.el ends here

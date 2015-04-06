(defun modeline-region-counter ()
  (if (region-active-p)
      (format "%sC|%sW|%sL "
              (- (region-end) (region-beginning))
              (count-words (region-beginning) (region-end))
              (count-lines (region-beginning) (region-end)))
    ""))

(defun god-mode-bar ()
  (if mark-active
      (format "[VISUAL]")
    (if (and (boundp 'god-local-mode) god-local-mode)
        (format "[NORMAL]")
      (format "[INSERT]"))))

(setq mode-line-format-backup
      '(" " (:eval (concat "[" (number-to-string (escreen-get-current-screen-number)) "]")) " "
        (:eval (butlast (cdr (gnus-mst-notify-modeline-form)))) " "
        (:eval erc-modified-channels-object)
        "%*" "_"
        mode-line-remote " "
        (:eval (modeline-region-counter))
        "%3lL:%2cC "
        (:eval (format-time-string "%-I:%M%p")) " | "
        mode-line-buffer-identification " | "
        ;; (:eval (god-mode-bar)) " "
        mode-name " |"
        (vc-mode vc-mode) " "
        battery-mode-line-string))

(provide 'attic-powerline)

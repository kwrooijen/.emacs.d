;;==============================================================================
;;== Keys
;;==============================================================================

(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map (kbd "C-i") 'helm-esh-pcomplete)
             (define-key eshell-mode-map (kbd "M-m") 'eshell-back-to-indentation)
             (define-key eshell-mode-map (kbd "C-M-m") 'eshell-broadcast)))

;;==============================================================================
;;== Options
;;==============================================================================

;; Prompt for eshell, a bit buggy but it will do for now...
(setq eshell-prompt-function
      (lambda nil
        (if (equal (car (s-split "/scp:" (eshell/pwd))) (eshell/pwd))
            (setq tramp-prompt "")
          (setq tramp-prompt "/scp:"))
        (let ((split (s-split "/" (eshell/pwd))))
          (let ((bot (car (last split)))
                (top (car (last (butlast split)))))
            (concat tramp-prompt top "/" bot " λ ")))))

;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'eshell-mode-hook (lambda()
    (company-mode 0)))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun sh (buffer-name)
    "Start a terminal and rename buffer."
    (interactive "sbuffer name: ")
    (eshell)
    (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun eshell-back-to-indentation ()
 (interactive)
 (eshell-bol)
 (while (and
    (char-after (point))
    (equal (string (char-after (point))) " "))
    (forward-char 1)))

(defun spawn-eshell ()
  (interactive)
  (if (> (/ (window-width) 2) (window-height))
      (progn
        (split-window-horizontally)
        (other-window 1)
        (sh "TERM"))
    (progn
      (split-window)
      (other-window 1)
      (sh "TERM"))))

(defun eshell-broadcast(&optional yank-eshell-input)
  (interactive)
  (if eshell-mode
      (let ((buff (get-buffer-window))
            (col (current-column)))
        (eshell-back-to-indentation)
        (setq eshell-indentation-column (point))
        (move-end-of-line 1)
        (setq eshell-oel-column (point))
        (kill-ring-save eshell-indentation-column eshell-oel-column)
        (move-to-column col)
        (unless yank-eshell-input (eshell-send-input))
        (other-window 1)
        (while (not (eq (get-buffer-window) buff))
          (if eshell-mode
              (progn
                (end-of-buffer)
                (yank)
                (unless yank-eshell-input (eshell-send-input))))
          (other-window 1)))))

(defun eshell-broadcast-diff()
  (interactive)
  (let ((buff-win (get-buffer-window))
        (buff (current-buffer)))
    (other-window 1)
    (while (not (eq (get-buffer-window) buff-win))
      (if eshell-mode
          (progn
            (highlight-changes-mode -1)
            (highlight-compare-buffers buff (current-buffer))))
      (sleep-for 0.1)
      (end-of-buffer)
      (other-window 1))
    (highlight-changes-mode -1)))

(provide 'my-eshell)

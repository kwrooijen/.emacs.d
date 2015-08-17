;;==============================================================================
;;== Keys
;;==============================================================================
(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
             (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)
             (define-key eshell-mode-map (kbd "C-i") 'helm-esh-pcomplete)
             (define-key eshell-mode-map (kbd "M-m") 'eshell-bol)
             (define-key eshell-mode-map (kbd "C-M-m") 'eshell-broadcast)))

;;==============================================================================
;;== Options
;;==============================================================================

;;==============================================================================
;;== Hook
;;==============================================================================

;;==============================================================================
;;== Advice
;;==============================================================================

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
        (eshell-bol)
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

(provide 'attic-eshell)

;;==============================================================================
;;== Keys
;;==============================================================================
(add-hook 'eshell-mode-hook
          '(lambda ()
             (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
             (define-key eshell-mode-map (kbd "M-n") 'eshell-previous-input)
             (define-key eshell-mode-map (kbd "C-i") 'helm-esh-pcomplete)
             (define-key eshell-mode-map (kbd "M-m") 'eshell-bol)
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

;;==============================================================================
;;== Advice
;;==============================================================================

;; Not sure how eshell-bol is defined, but I think my custom prompt breaks it.
(defadvice eshell-bol (after eshell-bol activate)
  ;; Use a counter with a limit so we don't
  ;; continue infinitely in case of an error.
  (let ((count 0))
    (while (and (< count 50) (not (equal (string(char-after (point))) "λ")))
      (setq count (+ count 1))
      (forward-char))
    (forward-char 2)))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun sh (buffer-name)
    "Start a terminal and rename buffer."
    (interactive "sbuffer name: ")
    (eshell)
    (rename-buffer (format "%s%s" "$" buffer-name) t))

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

(provide 'attic-eshell)

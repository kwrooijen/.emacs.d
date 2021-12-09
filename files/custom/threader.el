(defface threader-error-face
  '((t :background "#ff2222" :foreground "#ffffff"))
  "" :group 'threader)

(setq threader-command-list
      '(self-insert-command
        evil-delete-backward-char-and-join
        kill-word
        backward-kill-word
        kill-region
        evil-delete
        evil-delete-backward-word
        evil-delete-char
        evil-delete-backward-char
        evil-delete-line
        kwrooijen-undo
        undo-tree-redo
        evil-paste-after
        evil-paste-before
        ;;
        ))

(setq threader-error-overlay nil)
(setq threader-max-char-count 280)

(defun kwrooijen-threader-end-of-buffer? ()
  (= (point) (point-max)))

(defun kwrooijen-threader-find-beginning ()
  (interactive)
  (save-excursion
    (if (kwrooijen-threader-end-of-buffer?)
        (point-max)
      (progn
        (search-backward "---")
        (next-line)
        (point)))))

(defun kwrooijen-threader-find-end ()
  (interactive)
  (save-excursion
    (condition-case _
        (progn
          (search-forward "---")
          (previous-line)
          (end-of-line)
          (point))
      (error
       (point-max)))))

(defun kwrooijen-threader-count-chars ()
  (interactive)
  (let* ((beginning (kwrooijen-threader-find-beginning))
         (end (kwrooijen-threader-find-end))
         (content (buffer-substring beginning end))
         (url-regexp "\\(https\\|http\\)://\\S-+")
         (replacement "......................."))
    (length (replace-regexp-in-string url-regexp replacement content))))

(defun kwrooijen-threader-add-to-modeline ()
  (let ((chars-left (- threader-max-char-count (kwrooijen-threader-count-chars))))
    (setq global-mode-string (format "| Chars left: %s |" chars-left))))

(defun kwrooijen-threader-check-length ()
  (message "COMMAND: %s" this-command)
  (when (member this-command threader-command-list)
    (when threader-error-overlay
      (delete-overlay threader-error-overlay))
    (let ((count (kwrooijen-threader-count-chars)))
      (kwrooijen-threader-add-to-modeline)
      (when (> count threader-max-char-count)
        (let* ((end (kwrooijen-threader-find-end))
               (begin (- end (- count threader-max-char-count))))
          (setq threader-error-overlay (make-overlay begin end))
          (overlay-put threader-error-overlay 'face 'threader-error-face))))))

(defun kwrooijen-threader-enable ()
  (interactive)
  (add-hook 'post-command-hook #'kwrooijen-threader-check-length nil t))

(comment

 defun


 )

(provide 'threader)

(echo "123")

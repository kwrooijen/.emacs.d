(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file
       (concat "/sudo:root@localhost:"
               (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ssh-add ()
  (interactive)
  (async-shell-command "ssh-add ~/.ssh/id_rsa"))

(defun tab-to-tab-stop-line-or-region (&optional left)
  (interactive)
  (if (region-active-p)
      (progn
        (if left
            (if (< (mark) (point))
                (indent-rigidly-left-to-tab-stop (mark) (point))
              (indent-rigidly-left-to-tab-stop (point) (mark)))
          (if (< (mark) (point))
              (indent-rigidly-right-to-tab-stop (mark) (point))
            (indent-rigidly-right-to-tab-stop (point) (mark))))
        (activate-mark)
        (error "Region tab"))
    (tab-to-tab-stop)))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun error-preview (buff)
  (interactive)
  (compile-goto-error)
  (switch-to-buffer-other-window buff))

(defun send-to-pastie (answer)
  "Start a terminal and rename buffer."
  (interactive "cSend region to Pastie?: (y/n) ")
  (if (equal answer ?\y) (pastie-region (region-beginning) (region-end))))

(defun get-current-buffer-major-mode ()
  (interactive)
  (message "%s" major-mode))

(defun ensure-buffer-name-begins-with-exl ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat "!" name) t))))

(defun run-make-input (input)
  "Run make with user input."
  (interactive "sMake: ")
  (run-make input "[Custom Make]"))

(defun run-make (arg name)
  (interactive)
  (if (get-buffer name) (kill-buffer name))
  (my-up-to-script "Makefile" (concat "make " arg) name))

(defun underscores-to-camel-case (str)
  "Converts STR, which is a word using underscores, to camel case."
  (interactive "S")
  (apply 'concat (mapcar 'capitalize (split-string str "_"))))

(defun get-return-code (s)
  (nth 1 (reverse (split-string s "\n"))))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg))))

(defun copy-line-fun (up)
  (let ((current (current-column)))
    (if (region-active-p)
        (progn
          (copy-region-as-kill (region-beginning) (region-end))
          (goto-char (if up (region-beginning) (region-end)))
          (if up (open-line 1) (newline 1))
          (yank))
      (progn
        (copy-line 1)
        (beginning-of-line)
        (yank)
        (if up (previous-line))
        (move-to-column current)))))

(defun copy-line-up ()
  (interactive)
  (copy-line-fun t))

(defun copy-line-down ()
  (interactive)
  (copy-line-fun nil))

(defun attic/comment ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun is-tramp-mode ()
  (file-remote-p default-directory))

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext)))
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun capitalize-previous-word ()
  (interactive)
  (save-excursion
    (backward-word)
    (capitalize-word 1)))

(defun camelcase-region (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "_\\(.\\)" nil t)
      (replace-match (upcase (match-string 1))))))

(defun camelcase-word-or-region ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region pos1 pos2)))

(defun camelcase-region+ (start end)
  "Changes region from snake_case to camelCase"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (capitalize-region start end)
    (goto-char (point-min))
    (while (re-search-forward "_\\(.\\)" nil t)
      (replace-match (upcase (match-string 1))))))

(defun camelcase-word-or-region+ ()
  "Changes word or region from snake_case to camelCase"
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region+ pos1 pos2)))

(defun snakecase-word-or-region ()
  (interactive)
  (if mark-active (message "Don't have snakecase-region yet")
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                        (1+ (car bounds)) (cdr bounds))
        (downcase-region (car bounds) (cdr bounds))))))

(defun set-window-width (args)
  (shrink-window-horizontally (window-body-width))
  (enlarge-window-horizontally (- args (window-body-width))))

(defun set-window-height (args)
  (shrink-window (window-body-height))
  (enlarge-window (- args (window-body-height))))

(defun doc-center-window ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (select-window-by-number 1)
  (set-window-width 40)
  (switch-to-buffer "empty-buffer")
  (select-window-by-number 3)
  (set-window-width 40)
  (switch-to-buffer "empty-buffer")
  (select-window-by-number 2)
  (doc-view-fit-width-to-window))

(defun toggle-modeline ()
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (setq mode-line-format attic-mode-line-format)))

(defun no-split ()
  (interactive)
  (setq-local split-width-threshold 2000)
  (setq-local split-height-threshold 2000))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun attic-lock ()
  (interactive)
  (deactivate-mark)
  (if (equal " *Minibuf-1*" (buffer-name))
      (keyboard-escape-quit)
    (unless (or multiple-cursors-mode macro-active
                (and (boundp 'god-local-mode) (not god-local-mode))
                (not macro-active))
      (progn
        (call-interactively (key-binding (kbd "C-g")))
        (keyboard-escape-quit))))
  (unless (member major-mode god-exempt-major-modes)
    (god-local-mode 1)))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(defun remove-newline-space ()
  (interactive)
  (flet ((point-is-blank () (member (thing-at-point 'char t) '("\n" "\s"))))
    (unless (and (point-is-blank)
                 (equal (current-column) 0))
      (backward-char 1))
    (while (point-is-blank)
      (backward-char 1))
    (forward-char 1)
    (while (point-is-blank)
      (delete-char 1))
    (newline-and-indent)
    (unless (member major-mode '(scheme-mode))
      (newline-and-indent))))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" tags-file-name (directory-file-name dir-name))))

(provide 'attic-functions)

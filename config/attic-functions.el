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

(defun get-battery-percentage ()
  (interactive)
  (concat (cdr (assoc '112 (funcall battery-status-function))) "%%"))

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

(defun sht (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (ansi-term "/bin/sh")
  (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun my/grep (term)
  "Start a terminal and rename buffer."
  (interactive "sGrep value: ")
  (let ((term-list (split-string term)))
    (grep-find
     (format
      (concat
       "find . -type f "
       "! -name '*.log' "
       "! -name '*.dump' "
       "! -name '*#*' "
       "! -path '*/\.*' "
       "! -wholename '*/deps/*' "
       "! -wholename '*/tmp/*' "
       "! -wholename '*/elpa/*' "
       "! -wholename '*/backups/*' "
       "-exec grep -nH -e %s {} + "
       "| grep -v 'Binary file' "
       (mapconcat (lambda(X) (concat " | grep " X)) (cdr term-list) "")) term))))

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

(defun guard ()
  (interactive)
  (my-up-to-script "Guardfile" "bundle exec guard start --clear" "[Guard]"))

(defun run-mix (arg)
  (interactive)
  (my-up-to-script "mix.exs" (concat "mix " arg) "[Mix]"))

(defun underscores-to-camel-case (str)
  "Converts STR, which is a word using underscores, to camel case."
  (interactive "S")
  (apply 'concat (mapcar 'capitalize (split-string str "_"))))

(defun god-mode-disable ()
  (interactive)
  (god-mode-all-set -1)
  (god-local-mode -1)
  (key-chord-mode 1)
  (if window-system
      (set-cursor-color "green")
    (if (getenv "DISPLAY")
        (if (getenv "TMUX")
            (send-string-to-terminal "\033Ptmux;\033\033]12;Green\007\033\\")
          (send-string-to-terminal "\033]12;Green\007"))))
  (message nil))

(defun god-mode-enable ()
  (interactive)
  (unless (member major-mode god-exempt-major-modes)
    (god-mode-all-set 1)
    (god-local-mode 1)
    (if window-system
        (set-cursor-color "#cc6666")
      (send-string-to-terminal "\033]12;White\007"))))

(defun god-mode-all-set (arg)
  "Set God mode in all buffers by argument."
  (interactive)
  (setq god-global-mode t)
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (god-mode-maybe-activate arg)))
   (buffer-list))
  (setq god-global-mode (= arg 1)))

(defun escape-key ()
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
  (attic-lock))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(defun vim-o (&optional up)
  (interactive)
  (if up
      (progn
        (beginning-of-line)
        (open-line 1))
    (progn
      (end-of-line)
      (newline)))
  (if indy-mode
      (indy)
    (indent-for-tab-command))
  (god-mode-disable))

(defun default-directory-full ()
  (if (equal (substring default-directory 0 1) "~")
      (format "/home/%s%s" (user-login-name) (substring default-directory 1))
    default-directory))

(defun home-directory ()
  (interactive)
  (format "/home/%s/" (user-login-name)))

(defun find-files-recursively-shell-command (x)
  (interactive)
  (format "find %s | grep -v '#' | grep -v '/\\.' | grep -v '/Downloads' |
        grep -v '/Dropbox' | grep -v '/Music' | grep -v '/Videos' | grep -v '/Pictures' |
        grep -v '/Mail' | grep -v 'ebin' | grep -v 'deps' | grep -v 'dist'" x))

(defun execute-c ()
  (interactive)
  (if (buffer-file-name)
      (progn
        (shell-command
         (format "gcc -g -o %s %s"
                 (file-name-sans-extension (buffer-name))
                 (buffer-file-name)))
        (async-shell-command
         (format " ./%s" (file-name-sans-extension (buffer-name)))))))

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

(defun my-comment ()
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

(defun what-line-int (&optional p)
  "Get the current line number as an int"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (if p
          (1+ (count-lines 1 p))
        (1+ (count-lines 1 (point)))))))

(setq reg-num 0)
(defun inc-register ()
  (interactive)
  (setq reg-num (+ 1 reg-num))
  (point-to-register reg-num)
  (message nil))

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

(defun cm-fast-step-upward ()
  "Step 3 lines up, recenteres the screen."
  (interactive)
  (forward-line -3)
  (recenter))

(defun cm-fast-step-downward ()
  "Step 3 lines down, recenteres the screen."
  (interactive)
  (forward-line 3)
  (recenter))

(defun toggle-modeline ()
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (setq mode-line-format attic-mode-line-format)))

(defun no-split ()
  (interactive)
  (setq-local split-width-threshold 2000)
  (setq-local split-height-threshold 2000))

(defun select-line-from-indentation ()
  (interactive)
  (back-to-indentation)
  (set-mark (point))
  (activate-mark)
  (move-end-of-line 1))

(defun attic-sauron-toggle ()
  (interactive)
  (setq sauron-active (not (get-buffer-window "*Sauron*")))
  (sauron-toggle-hide-show)
  (if sauron-active
      (let ((previous-window (window-numbering-get-number)))
        (switch-to-buffer-other-window "*Sauron*")
        (set-window-height sauron-max-line-height)
        (select-window-by-number previous-window))))

(defun sauron-select-last-event ()
  (interactive)
  (sauron-pop-to-buffer)
  (end-of-buffer)
  (sauron-activate-event-prev))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun attic-enable-and-save ()
  (interactive)
  (attic-lock)
  (save-buffer))

(defun attic-lock ()
  (interactive)
  (if attic-evil
      (evil-force-normal-state)
    (god-mode-enable)))

(provide 'attic-functions)

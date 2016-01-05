(defun new-shell (buffer)
  "Create a new shell."
  (interactive "sShell Name: ")
  (eshell (concat "*" buffer "*")))

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
  (let ((args) (function))
    (when (mark)
      (when (<  (mark) (point)) (setq args (list (mark)  (point))))
      (when (>= (mark) (point)) (setq args (list (point) (mark)))))
    (when (not left) (setq func 'indent-rigidly-right-to-tab-stop))
    (when left       (setq func 'indent-rigidly-left-to-tab-stop))
    (when (region-active-p)
      (apply func args)
      (activate-mark)
      (error "Region tab"))
    (unless (region-active-p)
      (tab-to-tab-stop))))

(defun tab-to-tab-stop-line-or-region-backward (&optional left)
  (interactive)
  (tab-to-tab-stop-line-or-region t))

(defun yank-pop-or-kill-ring ()
  (interactive)
  (if (or (equal last-command 'yank) (equal last-command 'yank-pop))
      (yank-pop)
    (helm-show-kill-ring)))

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

(defun send-to-gist (answer)
  ""
  (interactive "cSend region to Gist?: (y/n) ")
  (if (equal answer ?\y) (gist-region (region-beginning) (region-end))))

(defun get-current-buffer-major-mode ()
  (interactive)
  (message "%s" major-mode))

(defun ensure-buffer-name-begins-with-exl ()
  "change buffer name to end with slash"
  (let ((name (buffer-name)))
    (if (not (string-match "/$" name))
        (rename-buffer (concat "!" name) t))))

(defun cd-up-to-file (file)
  "Go up a directory until you find FILE or enter the root directory.
If file is found then return t else nil."
  (while (not (or (file-exists-p file) (equal default-directory "/")))
    (cd ".."))
  (file-exists-p file))

(defun kill-buffer-if-exists (buffer)
  "Attempt to kill BUFFER if it exists."
  (if (get-buffer buffer)
      (kill-buffer buffer)))

(defun reset-buffer (buffer)
  (kill-buffer-if-exists buffer)
  (generate-new-buffer buffer))

(defun run-make (arg name)
  (interactive)
  (reset-buffer "*Make Find Makefile*")
  (with-current-buffer "*Make Find Makefile*"
    (when (cd-up-to-file "Makefile")
      (kill-buffer-if-exists name)
      (cond
       ((equal arg "start")
        (async-shell-command (concat "make " arg) name))
       (t
        (let ((comp-buffer (compile (concat "make " arg))))
          (with-current-buffer comp-buffer
            (rename-buffer name)))))
      (message "Could not find Makefile"))))

(defun attic/make-go ()
  (interactive)
  (run-make "go" "[Make go]"))

(defun attic/make-restart ()
  (interactive)
  (run-make "restart" "[Make Restart]"))

(defun attic/make-start ()
  (interactive)
  (run-make "start" "[Make Start]"))

(defun attic/make-stop ()
  (interactive)
  (run-make "stop" "[Make Stop]"))

(defun attic/make-test ()
  (interactive)
  (run-make "test" "[Make Test]"))

(defun attic/make-default ()
  (interactive)
  (run-make "" "[Make]"))

(defun attic/make-custom (input)
  "run make with user input."
  (interactive "sMake: ")
  (run-make input "[Custom Make]"))

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

(defun mc-active ()
  (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))

(when (equal mode-lock 'god)
  (defun attic-lock ()
    (interactive)
    (setq cursor-type 'box)
    (deactivate-mark)
    (if (equal " *Minibuf-1*" (buffer-name))
        (keyboard-escape-quit)
      (unless (or (mc-active) macro-active
                  (not macro-active)
                  (and (equal mode-lock 'god)
                       (boundp 'god-local-mode)
                       (not god-local-mode)))
        (progn
          (call-interactively (key-binding (kbd "C-g")))
          (keyboard-escape-quit))))
    (unless (member major-mode god-exempt-major-modes)
      (god-local-mode 1))))

(when (equal mode-lock 'evil)
  (defun attic-lock ()
    (evil-force-normal-state)))

(defadvice keyboard-escape-quit (around attic-ad/my-keyboard-escape-quit-around activate)
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

(defun increment-decimal (&optional arg)
  "Increment the number forward at point by 'arg'."
  (interactive "p*")
  (let ((last-col (current-column)))
    (save-match-data
      (let ((inc-by (if arg arg 1))
            (field-width (- (match-end 0) (match-beginning 0)))
            answer)
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))
    (move-to-column last-col)))

(defun decrement-decimal (&optional arg)
  "Decrement the number forward at point by 'arg'."
  (interactive "p*")
  (increment-decimal (if arg (- arg) -1)))

(defadvice attic/erc (after attic-ad/attic/erc-after activate)
  (setq erc-password nil))

(defun attic/erc ()
  (interactive)
  (load "~/.erc.gpg")
  (erc :server "irc.freenode.net"
       :port 6667
       :nick erc-nick
       :password erc-password))

(defun frame-name (frame)
  (frame-parameter frame 'name))

(defun frame-exists (name)
  (member name (mapcar 'frame-name (frame-list))))

(defun attic/setup-frames ()
  (interactive)
  (set-frame-name "Main")
  (select-frame-by-name "Main")
  (unless (-any 'elscreen-screen-live-p '(1 2 3 4 5))
    (elscreen-create-initial-5-screens))

  ;; Twitter Frame
  (unless (frame-exists "Twitter")
    (new-frame '((name . "Twitter"))))
  (select-frame-by-name "Twitter")
  (unless (get-buffer ":home")
    (twit))
  (elscreen-create-initial-5-screens)
  (switch-to-buffer ":home")

  ;; IRC Frame
  (unless (frame-exists "IRC")
    (new-frame '((name . "IRC"))))
  (select-frame-by-name "IRC")
  (elscreen-create-initial-5-screens)
  (let ((irc-buffer-exists (get-buffer "irc.freenode.net:6667")))
    (unless irc-buffer-exists
      (attic/erc))
    (elscreen-goto-2)
    (switch-to-buffer "irc.freenode.net:6667")
    (elscreen-goto-3)
    (switch-to-buffer "irc.freenode.net:6667")
    (elscreen-goto-4)
    (switch-to-buffer "irc.freenode.net:6667")
    (elscreen-goto-5)
    (switch-to-buffer "irc.freenode.net:6667")
    (elscreen-goto-1)
    (switch-to-buffer "irc.freenode.net:6667")))

(defun buffer-toggle ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(provide 'attic-functions)

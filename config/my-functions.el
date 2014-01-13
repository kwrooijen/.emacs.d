(defun load-attic-workgroups ()
  (interactive)
  (setq b (current-buffer))
  (message "%s" b)
  (wg-load "~/.emacs.d/workgroups/Attic")
  (switch-to-buffer b))

(defun zsh (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (shell)
  (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun zsht (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (term "/bin/zsh")
  (rename-buffer (format "%s%s" "$" buffer-name) t))

(defun get-current-buffer-major-mode ()
  (interactive)
  (message "%s" major-mode))

(defun hoogle-search (query)
  "Search with hoogle commandline"
  (interactive "sHoogle query: ")
  (if (get-buffer "*Hoogle*")
      (kill-buffer "*Hoogle*"))
  ; get the version of hoogle so I don't have to manually adjust it for each update
  (shell-command (format "version=`hoogle --version | head -n 1 | awk '{print $2}' | cut -c 2- | rev | cut -c 2- | rev`;
                          data=\"/databases\";
                          two=$version$data;
                          hoogle \"%s\" --data=$HOME/.lazyVault/sandboxes/hoogle/cabal/share/hoogle-$two" query))
  (switch-to-buffer "*Shell Command Output*")
  (rename-buffer "*Hoogle*")
  (haskell-mode)
  (linum-mode 0)
  (previous-buffer)
)

(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
    'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
            (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/plugins/erlangscript" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
    '("\\.erl\\'" flymake-erlang-init))

(defun ensure-buffer-name-begins-with-exl ()
    "change buffer name to end with slash"
    (let ((name (buffer-name)))
        (if (not (string-match "/$" name))
            (rename-buffer (concat "!" name) t))))

(defun erlang-get-error ()
  (interactive)
  (shell-command (format "~/.emacs.d/scripts/erlangscript %s" buffer-file-name))
)

(defun elixir-keys-hook ()
  (local-set-key (kbd "C-c C-l") (lambda()
                                   (interactive)
                                   (elixir-mode-compile-file)
                                   (elixir-mode-iex)))
)

(defun run-haskell-test ()
  (interactive)
  (async-shell-command "~/.emacs.d/scripts/cabal-test" "[Haskell Tests]")
)

(defun underscores-to-camel-case (str)
  "Converts STR, which is a word using underscores, to camel case."
  (interactive "S")
  (apply 'concat (mapcar 'capitalize (split-string str "_"))))

; cmus functions

(defun cmus-pause ()
  (interactive)
  (shell-command (format "cmus-remote --pause"))
)

(defun cmus-volume-down ()
  (interactive)
  (shell-command (format "cmus-remote --volume -10%"))
)

(defun cmus-volume-up ()
  (interactive)
  (shell-command (format "cmus-remote --volume +10%"))
)

(defun cmus-next ()
  (interactive)
  (shell-command (format "cmus-remote --next"))
)
(defun cmus-previous ()
  (interactive)
  (shell-command (format "cmus-remote --prev"))
)

(defun ca-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))


; God functions

(defun god-mode-disable () (interactive)
  (setq god-local-mode nil)
  (if (getenv "TMUX")
    (send-string-to-terminal "\033Ptmux;\033\033]12;Green\007\033\\")
    (send-string-to-terminal "\033]12;Green\007")
  )
)

(defun god-mode-enable () (interactive)
  (setq god-local-mode t)
  (keyboard-escape-quit)
  (if (getenv "TMUX")
      (send-string-to-terminal "\033Ptmux;\033\033]12;White\007\033\\")
      (send-string-to-terminal "\033]12;White\007")
      )
)

(defun keyboard-escape-quit-mc () (interactive)
  (mc/keyboard-quit)
  (keyboard-escape-quit)
)

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(defun fsize (font-size)
  (interactive "sChoose your destiny: ")
  (if (getenv "TMUX")
    (send-string-to-terminal (format "\033Ptmux;\033\33]50;xft:Monaco:bold:antialias=true:pixelsize=%s\007\033\\" font-size))
    (send-string-to-terminal (format "\33]50;xft:Monaco:bold:antialias=true:pixelsize=%s\007" font-size))
  )
)

(defun toggle-linum-gutter ()
  (interactive)
  (if (and (boundp 'linum-mode) linum-mode)
      (progn
        (interactive)
        (git-gutter)
        (setq git-gutter t)
        (setq linum-mode nil))
      (progn
        (interactive)
        (setq git-gutter-mode nil)
        (git-gutter:clear)
        (setq linum-mode t))
      ))



(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning)
                                   (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(defun guard ()
  (interactive)
  (async-shell-command "~/.emacs.d/scripts/my-guard" "[Guard]")
)

(provide 'my-functions)

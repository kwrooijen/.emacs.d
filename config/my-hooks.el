(defun raw-modes ()
  (set (make-local-variable 'linum-mode) nil))

(add-hook 'inferior-haskell-mode-hook 'raw-modes)
(add-hook 'comint-mode-hook           'raw-modes)
(add-hook 'term-mode-hook             'raw-modes)
(add-hook 'speedbar-mode-hook         'raw-modes)
(add-hook 'shell-mode-hook            'raw-modes)
(add-hook 'magit-mode-hook            'raw-modes)
(add-hook 'shell-command-mode-hook    'raw-modes)
(add-hook 'fundamental-mode-hook      'raw-modes)
(add-hook 'dired-mode-hook            'raw-modes)
(add-hook 'gnus-group-mode-hook       'raw-modes)
(add-hook 'gnus-article-mode-hook     'raw-modes)
(add-hook 'gnus-summary-mode-hook     'raw-modes)
(add-hook 'garak-mode-hook            'raw-modes)
(add-hook 'w3m-mode-hook              'raw-modes)
(add-hook 'minibuffer-setup-hook      'raw-modes)

(add-hook 'erlang-mode-hook  'flymake-mode)
(add-hook 'erlang-mode-hook  'erlang-keys-hook)

(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(add-hook 'js2-mode-hook
          (lambda()
            (js2-keys-hook)
            (imenu-add-menubar-index)
            (hs-minor-mode t)))

(add-hook 'elixir-mode-hook  'elixir-keys-hook)

(add-hook 'dired-mode-hook 'ensure-buffer-name-begins-with-exl)

(provide 'my-hooks)

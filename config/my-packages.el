(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(defvar my-packages '(
    ace-jump-mode
    auto-complete
    elixir-mix
    elixir-mode
    elixir-yasnippets
    emms
    erlang
    expand-region
    god-mode
    hackernews
    haskell-mode
    helm
    helm-dash
    helm-ls-git
    helm-swoop
    highlight-symbol
    iy-go-to-char
    js2-mode
    key-chord
    magit
    multiple-cursors
    powerline
    rainbow-delimiters
    redo+
    web-mode
    window-numbering
    wrap-region
    yasnippet
    zenburn-theme
    ))

(defvar my-docs '(
    "Elixir"
    "Erlang"
    "Haskell"
    "Emacs_Lisp"
    "Ruby"
    "Javascript"
    ))

(defun my-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        my-packages))

(defun my-install-docs ()
  (interactive)
    (mapc #'(lambda (doc)
        (helm-dash-install-docset doc))
    my-docs))

(provide 'my-packages)

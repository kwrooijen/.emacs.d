(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

(defvar my-packages '(
    ace-jump-mode
    alchemist
    buffer-move
    company
    dash
    elixir-mode
    elixir-yasnippets
    erc-image
    erlang
    escreen
    expand-region
    git-gutter+
    god-mode
    gh
    hackernews
    haskell-mode
    helm
    helm-company
    helm-dash
    helm-descbinds
    helm-ls-git
    helm-swoop
    highlight-symbol
    hydra
    iy-go-to-char
    js2-mode
    key-chord
    magit
    magit-gh-pulls
    multiple-cursors
    rainbow-delimiters
    redo+
    s
    simpleclip
    twittering-mode
    underwater-theme
    w3m
    web-mode
    window-numbering
    wrap-region
    yasnippet
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

;; If elpa directory doesn't exist, install packages and reload
(if (not (file-exists-p "~/.emacs.d/elpa/")) (progn
    (my-install-packages)
    (load-file "~/.emacs.d/init.el")
    (delete-other-windows)
    (insert "Welcome to the Attic!")))

(require 'erlang)
(require 'god-mode)
(require 'helm)
(require 'helm-ls-git)
(require 'helm-swoop)
(require 'helm-dash)
(require 'multiple-cursors)
(require 'redo+)
(require 'magit)
(require 'magit-gh-pulls)
(require 'fbterm)
(require 'rebar)
(require 'flymake)
(require 'tempo)
(require 'web-mode)
(require 'indent-of-doom)
(require 'git-gutter+)
(require 'escreen)
(require 's)
(require 'dash)
(require 'pastie)
(require 'sticky-windows)
(require 'erc)
(require 'erc-image)
;(require 'setup-company)
(require 'elixir-mode)
(require 'alchemist)

(require 'gnus-notify)

;; Modes
(god-mode)
(key-chord-mode t)
(multiple-cursors-mode t)
(show-paren-mode t)
(window-numbering-mode t)
(wrap-region-global-mode t)
(yas-global-mode t)
(global-git-gutter+-mode t)
(winner-mode t)
(global-company-mode t)
(display-battery-mode t)
(ido-mode t)

(provide 'my-packages)


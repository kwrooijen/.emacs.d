(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))

(defvar my-packages '(
    ace-jump-mode
    alchemist
    auto-complete
    buffer-move
    color-theme-sanityinc-tomorrow
    dash
    elixir-mode
    elixir-yasnippets
    erc-image
    erlang
    escreen
    expand-region
    gh
    git-gutter+
    git-gutter-fringe+
    god-mode
    hackernews
    haskell-mode
    helm
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
    neotree
    nlinum
    racket-mode
    rainbow-delimiters
    redo+
    s
    sauron
    twittering-mode
    w3m
    web-mode
    window-numbering
    wrap-region
    yasnippet
    ))

(defvar my-docs
  '("Elixir"
    "Erlang"
    "Haskell"
    "Emacs_Lisp"
    "Ruby"
    "Javascript"))

(defun my-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc
   #'(lambda (package)
       (unless (package-installed-p package)
         (package-install package)))
   my-packages))

(defun my-install-docs ()
  (interactive)
  (mapc
   #'(lambda (doc)
       (helm-dash-install-docset doc))
   my-docs))

;; If elpa directory doesn't exist, install packages and reload
(if (not (file-exists-p "~/.emacs.d/elpa/"))
    (progn
      (my-install-packages)
      (load-file "~/.emacs.d/init.el")
      (delete-other-windows)
      (insert "Welcome to the Attic!")))

(require 'dash)
(require 'elixir-mode)
(require 'erc)
(require 'erc-image)
(require 'erlang)
(require 'escreen)
(require 'fbterm)
(require 'flymake)
(require 'gnus-notify)
(require 'god-mode)
(require 'helm)
(require 'helm-dash)
(require 'helm-ls-git)
(require 'helm-swoop)
(require 'indent-of-doom)
(require 'magit)
(require 'multiple-cursors)
(require 'pastie)
(require 'rebar)
(require 'redo+)
(require 's)
(require 'sticky-windows)
(require 'tempo)
(require 'web-mode)
(require 'linum)
(require 'highlight-symbol)
(require 'neotree)
(require 'twittering-mode)

(if window-system
    (require 'git-gutter-fringe+)
    (require 'git-gutter+))

;; Modes
(display-battery-mode t)
(global-git-gutter+-mode t)
(global-auto-complete-mode)
(god-mode)
(ido-mode t)
(key-chord-mode t)
(multiple-cursors-mode t)
(show-paren-mode t)
(window-numbering-mode t)
(winner-mode t)
(wrap-region-global-mode t)
(yas-global-mode t)
(electric-pair-mode t)
(hl-line-mode t)

(setq sauron-active nil)
(provide 'attic-packages)


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(defvar my-packages '(
    ace-jump-mode
    anti-zenburn-theme
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
    simpleclip
    w3m
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


(defun get-submodules ()
    (let ((git-init "cd ~/.emacs.d ; git submodule init ; git submodule update;")
          (make-distel "make -C ~/.emacs.d/plugins/distel/;"))
      (async-shell-command (format "%s %s" git-init make-distel) "[Clone submodules]")
    ))

;; If elpa directory doesn't exist, install packages and reload
(if (not (file-exists-p "~/.emacs.d/elpa/")) (progn
    (get-submodules)
    (my-install-packages)
    (load-file "~/.emacs.d/init.el")
    (delete-other-windows)
    (insert "Welcome to the Attic!")))

(require 'auto-complete-config)
(require 'god-mode)
(require 'helm)
(require 'helm-ls-git)
(require 'helm-swoop)
(require 'multiple-cursors)
(require 'redo+)
(require 'magit)
(require 'fbterm)
(require 'rebar)
(require 'distel)
(require 'flymake)
(require 'tempo)
(require 'web-mode)
(require 'indent-of-doom)

;; Modes
(global-linum-mode t)
(global-rainbow-delimiters-mode)
(god-mode)
(key-chord-mode 1)
(multiple-cursors-mode 1)
(show-paren-mode t)
(window-numbering-mode 1)
(wrap-region-global-mode t)
(yas-global-mode 1)

(provide 'my-packages)

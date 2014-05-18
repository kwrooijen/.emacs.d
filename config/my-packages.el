(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(defvar my-packages '(
    ace-jump-mode
    auto-complete
    emms
    erlang
    expand-region
    god-mode
    hackernews
    haskell-mode
    helm
    helm-ls-git
    helm-swoop
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
    ))

(defun my-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
        my-packages))

(provide 'my-packages)

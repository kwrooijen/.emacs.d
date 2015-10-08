(require 'package)
(require 'ert)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defvar attic-mode-map (make-keymap) "attic-mode keymap.")
(define-minor-mode attic-mode
  "A minor mode for key mapping."
  t " attic" 'attic-mode-map)

(use-package ac-cider
  :ensure t)

(use-package ace-jump-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package auto-complete
  :ensure t
  :init
  (global-auto-complete-mode)
  :config
  (setq ac-auto-show-menu 0.3
        ac-candidate-limit 15
        ac-delay 0.3)
  (bind-key "<return>" (lambda() (interactive) (ac-stop) (call-interactively (key-binding (kbd "C-m")))) ac-complete-mode-map)
  (bind-key "SPC" (lambda() (interactive) (ac-stop) (insert " ")) ac-complete-mode-map)
  (bind-key "C-m" (lambda() (interactive) (ac-stop) (newline)) ac-complete-mode-map)
  (bind-key ":" (lambda() (interactive) (ac-stop) (insert ":")) ac-complete-mode-map)
  (bind-key "." (lambda() (interactive) (ac-stop) (insert ".")) ac-complete-mode-map)
  (bind-key "C-p" 'ac-previous ac-complete-mode-map)
  (bind-key "M-j" 'yas/expand ac-complete-mode-map)
  (bind-key "C-n" 'ac-next ac-complete-mode-map))

(use-package clojure-mode
  :ensure t
  :config
  (bind-key "C-x C-e" 'cider-eval-last-sexp clojure-mode-map)
  (defun attic-clojure-hook ()
    (attic-lock)
    (paredit-mode 1)
    (electric-pair-mode)
    (electric-pair-mode 0)
    (cider-mode 1)
    (setq-local helm-dash-docsets '("Clojure")))
  (add-hook 'clojure-mode-hook 'attic-clojure-hook))

(use-package cider
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (bind-key "M-g" (lambda() (interactive) (company-abort) (god-local-mode-resume)) company-active-map)
  (bind-key "M-f" 'company-complete-selection company-active-map)
  (bind-key "<return>" (lambda() (interactive) (company-abort) (newline)) company-active-map)
  (bind-key "SPC" (lambda() (interactive) (company-abort) (insert " ")) company-active-map)
  (bind-key "C-m" (lambda() (interactive) (company-abort) (newline)) company-active-map)
  (bind-key ":" (lambda() (interactive) (company-abort) (insert ":")) company-active-map)
  (bind-key "." (lambda() (interactive) (company-abort) (insert ".")) company-active-map)
  (bind-key "M-h" 'helm-company company-active-map)
  (bind-key "M-j" 'yas/expand company-active-map)
  (bind-key "C-n" 'company-select-next company-active-map)
  (bind-key "C-p" 'company-select-previous company-active-map))

(use-package dash
  :ensure t)

(use-package dired
  :config
  (require 'dired)
  (bind-key "c f" 'helm-ls-git-ls dired-mode-map)
  (bind-key "z" 'helm-buffers-list dired-mode-map)
  (bind-key "c m" 'magit-status dired-mode-map)
  (bind-key ";" 'attic-semi-colon/body dired-mode-map)
  (bind-key "c z" 'attic-make-map dired-mode-map))

(use-package elixir-mode
  :ensure t
  :config
  (defun attic-elixir-hook ()
    (attic-lock)
    (electric-pair-mode)
    (auto-complete-mode 0)
    (setq tab-stop-list tab-stop-list-2)
    (company-mode)
    (alchemist-mode)
    (setq-local helm-dash-docsets '("Elixir")))
  (add-hook 'elixir-mode-hook 'attic-elixir-hook))

(use-package elm-mode
  :ensure t
  :config
  (defun elm-reactor ()
    (interactive)
    (async-shell-command "elm-reactor" "*elm-reactor*"))
  (defun attic-elm-hook ()
    (attic-lock)
    (fix-tabs 4)
    (electric-pair-mode)
    (elm-indentation-mode 0)
    (setq iod--use-tab-cycle t))
  (add-hook 'elm-mode-hook 'attic-elm-hook))

(use-package erc
  :config
  (erc-truncate-mode 1)
  (erc-scrolltobottom-mode 1)
  (setq erc-nick "attichacker")
  (setq erc-prompt-for-password nil)
  (setq erc-ignore-list '("*Flowdock*" "Flowdock" "-Flowdock-"))
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (bind-key "" 'function erc-mode-map)
  (bind-key "C-M-m" 'erc-send-current-line erc-mode-map)
  (bind-key "RET" (lambda() (interactive) (message "Use C-M-m to send")) erc-mode-map)
  (defadvice erc (before erc activate)
    (setq erc-prompt-for-password nil)
    (load "~/.erc.gpg")
    (setq erc-password ercpass)))

(use-package erlang
  :ensure t
  :config
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (bind-key "M-/" 'erlang-get-error erlang-mode-map)
  (bind-key "C-c C-k"
    (lambda() (interactive)
      (inferior-erlang)
      (split-window)
      (other-window 1)
      (other-window -1)) erlang-mode-map)
  (bind-key "M-n" 'highlight-symbol-next erlang-mode-map)
  (bind-key "M-p" 'highlight-symbol-prev erlang-mode-map)
  (bind-key "M-n" 'highlight-symbol-next erlang-mode-map)
  (bind-key "M-p" 'highlight-symbol-prev erlang-mode-map)
  (bind-key ">"   (lambda() (interactive) (insert ">")) erlang-mode-map)

  (defun flymake-erlang-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-intemp))
           (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/scripts/erlang/erlang-flymake" (list local-file))))

  (defun erlang-get-error ()
    (interactive)
    (async-shell-command
     (format "~/.emacs.d/scripts/erlang/erlang-flymake %s" buffer-file-name) "[Erlang Errors]"))

  (defun attic-erlang-hook ()
    (if (not (is-tramp-mode))
        (progn
          (flymake-erlang-init)
          (flymake-mode 1)))
    (attic-lock)
    (electric-pair-mode)
    (fix-tabs 4)
    (setq-local helm-dash-docsets '("Erlang")))
  (add-hook 'erlang-mode-hook 'attic-erlang-hook))

(use-package escreen
  :ensure t)

(use-package eshell
  :config
  (defun eshell-broadcast(&optional yank-eshell-input)
    (interactive)
    (if (or eshell-mode (equal major-mode 'shell-mode))
        (let ((buff (get-buffer-window))
              (col (current-column)))
          (eshell-bol)
          (setq eshell-indentation-column (point))
          (move-end-of-line 1)
          (setq eshell-oel-column (point))
          (kill-ring-save eshell-indentation-column eshell-oel-column)
          (move-to-column col)
          (if eshell-mode (unless yank-eshell-input (eshell-send-input))
            (unless yank-eshell-input (comint-send-input)))
          (other-window 1)
          (while (not (eq (get-buffer-window) buff))
            (if (or eshell-mode (equal major-mode 'shell-mode))
                (progn
                  (end-of-buffer)
                  (yank)
                  (if eshell-mode (unless yank-eshell-input (eshell-send-input))
                    (unless yank-eshell-input (comint-send-input)))))
            (other-window 1)))))

  (defun eshell-broadcast-diff()
    (interactive)
    (let ((buff-win (get-buffer-window))
          (buff (current-buffer)))
      (other-window 1)
      (while (not (eq (get-buffer-window) buff-win))
        (if (or eshell-mode (equal major-mode 'shell-mode))
            (progn
              (highlight-changes-mode -1)
              (highlight-compare-buffers buff (current-buffer))))
        (sleep-for 0.1)
        (end-of-buffer)
        (recenter-top-bottom (window-height))
        (other-window 1))
      (highlight-changes-mode -1)
      (recenter-top-bottom (window-height))))

  (defun attic-eshell-hook ()
    (bind-key "M-p" 'eshell-previous-input eshell-mode-map)
    (bind-key "M-n" 'eshell-next-input eshell-mode-map)
    (bind-key "C-i" 'helm-esh-pcomplete eshell-mode-map)
    (bind-key "M-m" 'eshell-bol eshell-mode-map)
    (bind-key "C-M-m" 'eshell-broadcast eshell-mode-map))
  (add-hook 'eshell-mode-hook attic-eshell-hook))

(use-package eww
  :config
  (bind-key "n" (lambda() (interactive) (scroll-up 1)) eww-mode-map)
  (bind-key "p" (lambda() (interactive) (scroll-down 1)) eww-mode-map)
  (bind-key "v" 'scroll-up-command eww-mode-map))

(use-package expand-region
  :ensure t
  :init
  (bind-key "M-@" 'er/expand-region attic-mode-map))

(use-package geiser
  :ensure t
  :config
  (defun attic-geiser-hook ()
    (define-key geiser-mode-map (kbd "M-.") 'find-tag)
    (paredit-mode 1))
  (add-hook 'geiser-mode-hook 'attic-geiser-hook)
  (add-hook 'geiser-repl-mode-hook 'attic-geiser-hook))

(use-package git-gutter+
  :ensure t
  :config
  (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (global-git-gutter+-mode t))

(use-package god-mode
  :ensure t
  :config
  (god-mode)
  (bind-key "i" (lambda () (interactive) (god-local-mode -1)) god-local-mode-map)
  (bind-key "u" 'undo god-local-mode-map)
  (bind-key "M-u" 'redo god-local-mode-map)
  (bind-key "J" '(lambda () (interactive) (join-line -1)) god-local-mode-map)
  (bind-key "/" 'attic/comment god-local-mode-map)

  (add-to-list 'god-exempt-major-modes 'gnus-summary-mode)
  (add-to-list 'god-exempt-major-modes 'gnus-group-mode)
  (add-to-list 'god-exempt-major-modes 'term-mode)
  (add-to-list 'god-exempt-major-modes 'help-mode)
  (add-to-list 'god-exempt-major-modes 'grep-mode)
  (add-to-list 'god-exempt-major-modes 'doc-view-mode)
  (add-to-list 'god-exempt-major-modes 'top-mode)
  (add-to-list 'god-exempt-major-modes 'dired-mode)
  (add-to-list 'god-exempt-major-modes 'magit-status-mode)
  (add-to-list 'god-exempt-major-modes 'magit-revision-mode)
  (add-to-list 'god-exempt-major-modes 'twittering-mode))

(use-package grep
  :config
  (bind-key "n" 'next-line grep-mode-map)
  (bind-key "p" 'previous-line grep-mode-map)
  (bind-key "TAB" (lambda() (interactive) (error-preview "*grep*")) grep-mode-map)
  (bind-key "v" 'scroll-up-command grep-mode-map)
  (bind-key ";" 'attic-semi-colon/body grep-mode-map)
  (bind-key ";" 'attic-semi-colon/body grep-mode-map)
  (bind-key "z" 'helm-buffers-list grep-mode-map))

(use-package hackernews
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (defun run-haskell-test ()
    (interactive)
    (my-up-to-script "*.cabal" "cabal build ; cabal test --log=/dev/stdout" "[Haskell Tests]"))

  (defun hoogle-search (query)
    "Search with hoogle commandline"
    (interactive "sHoogle query: ")
    (if (get-buffer "*Hoogle*") (kill-buffer "*Hoogle*"))
                                        ; get the version of hoogle so
                                        ; I don't have to manually
                                        ; adjust it for each update
    (shell-command
     (format "version=`hoogle --version | head -n 1
        | awk '{print $2}' | cut -c 2- | rev | cut -c 2- | rev`;
        data=\"/databases\"; two=$version$data; hoogle \"%s\"
        --data=$HOME/.lazyVault/sandboxes/hoogle/cabal/share/hoogle-$two"
             query))
    (switch-to-buffer "*Shell Command Output*")
    (rename-buffer "*Hoogle*")
    (haskell-mode)
    (previous-buffer))

  (defun attic-haskell-hook ()
    (attic-lock)
    (electric-pair-mode)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indentation)
    (setq-local helm-dash-docsets '("Haskell")))
  (add-hook 'haskell-mode-hook 'attic-haskell-hook))

(use-package helm
  :ensure t
  :config
  (setq ;; truncate long lines in helm completion
   helm-truncate-lines t
   ;; may be overridden if 'ggrep' is in path (see below)
   helm-grep-default-command
   "grep -a -d skip %e -n%cH -e %p %f"
   helm-grep-default-recurse-command
   "grep -a -d recurse %e -n%cH -e %p %f"
   ;; do not display invisible candidates
   helm-quick-update t
   ;; be idle for this many seconds, before updating in delayed sources.
   helm-idle-delay 0.01
   ;; be idle for this many seconds, before updating candidate buffer
   helm-input-idle-delay 0.01
   ;; open helm buffer in another window
   helm-split-window-default-side 'other
   ;; limit the number of displayed canidates
   helm-candidate-number-limit 200
   ;; don't use recentf stuff in helm-ff
   helm-ff-file-name-history-use-recentf nil
   ;; move to end or beginning of source when reaching top or bottom
   ;; of source
   helm-move-to-line-cycle-in-source t
   ;; fuzzy matching
   helm-buffers-fuzzy-matching t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-echo-input-in-header-line t
   helm-ls-git-project-source '((name . "Helm git ls project")
                                (candidates . helm-ls-git-project-list)
                                (action . helm-ls-git-project-action)
                                (default . ""))
   helm-ls-git-project-list-file "~/.emacs.d/.helm-ls-git-project-list"
   ;; Don't ask to create new file
   helm-ff-newfile-prompt-p nil
   helm-grep-default-recurse-command "grep --exclude-dir=\"dist\" -a -d recurse %e -n%cH -e %p %f"
   helm-reuse-last-window-split-state t
   helm-ff-transformer-show-only-basename nil
   ;; Split window down
   helm-split-window-in-side-p t
   ;; Split when multiple windows open
   helm-swoop-split-with-multiple-windows t
   ;; Show relative path
   helm-ls-git-ls-show-abs-or-relative 'relative
   ;; Don't show colors in Tramp mode
   helm-ff-tramp-not-fancy t
   ;; Smarter completion for Helm
   helm-ff-smart-completion t
   ;; Helm-dash should use W3m for showing documentation
   helm-dash-browser-func 'eww
   ;; Don't add delay when choosing
   helm-exit-idle-delay 0
   ;; Don't display header
   helm-display-header-line nil
   ;; Set a min / max height of 30% of current buffer
   helm-autoresize-max-height 30
   helm-autoresize-min-height 30)
  ;; Try to hide source header as much as possible
  (set-face-attribute 'helm-source-header nil :height 0.1 :background "#000"  :foreground "#000")

  ;; Work around for the [Display not ready] error when typing to awesomely fast
  (defun my/helm-exit-minibuffer ()
    (interactive)
    (helm-exit-minibuffer))
  (eval-after-load "helm"
    '(progn
       (define-key helm-map (kbd "<RET>") 'my/helm-exit-minibuffer)))

  (bind-key "M-[" 'helm-resume attic-mode-map)
  (bind-key "M-x" 'helm-M-x attic-mode-map)
  (bind-key "C-b" 'nil helm-map)
  (bind-key "C-f" 'nil helm-map)
  (bind-key "M-b" 'nil helm-map)
  (bind-key "M-f" 'forward-word helm-map)
  (bind-key "M-s" 'helm-select-action helm-map)
  (bind-key "TAB" 'helm-execute-persistent-action helm-map)
  (bind-key "C-a" 'helm-buffers-toggle-show-hidden-buffers helm-buffer-map)
  (bind-key "M-e" 'helm-swoop-edit helm-swoop-map)
  (bind-key "M-?" 'helm-help helm-map)
  (bind-key "M-g" 'helm-keyboard-quit helm-map)
  (bind-key "M-g" 'helm-keyboard-quit helm-find-files-map)
  (bind-key "M-g" 'helm-keyboard-quit helm-generic-find-files-map)
  (bind-key "M-g" 'helm-keyboard-quit helm-buffer-map)

  (defun helm-highlight-files (x)
    nil)

  (defadvice helm-register (before helm-register activate)
    (setq helm-register-active t))

  (defadvice helm-register (after helm-register activate)
    (makunbound 'helm-register-active))

  (defadvice helm-swoop (before helm-swoop activate)
    (set-mark-command nil)
    (deactivate-mark)
    (setq helm-swoop-active t))

  (defadvice helm-swoop (after helm-swoop activate)
    (makunbound 'helm-swoop-active))

  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))

(use-package helm-dash
  :ensure t
  :init
  (bind-key "C-c C-s C-d" 'helm-dash attic-mode-map))

(use-package helm-descbinds
  :ensure t)

(use-package helm-ls-git
  :ensure t)

(use-package helm-swoop
  :ensure t
  :init
  (bind-key "C-c C-s C-s" 'helm-multi-swoop attic-mode-map)
  (bind-key "C-c C-s C-f" 'helm-swoop-find-files-recursively attic-mode-map)
  (define-key helm-map (kbd "C-n")
    (lambda()
      (interactive)
      (if (or (boundp 'helm-swoop-active)
              (boundp 'helm-register-active))
          (progn (helm-next-line) (helm-execute-persistent-action))
        (helm-next-line))))
  (define-key helm-map (kbd "C-p")
    (lambda()
      (interactive)
      (if (or (boundp 'helm-swoop-active)
              (boundp 'helm-register-active))
          (progn (helm-previous-line) (helm-execute-persistent-action))
        (helm-previous-line)))))

(use-package highlight-symbol
  :ensure t
  :config
  ;; Highlight delay for multiple occurences
  (setq highlight-symbol-idle-delay 0))

(use-package hydra
  :ensure t
  :config
  (defun god-repeater--set-hydra-function (var)
    `(defhydra ,(make-symbol (concat "hydra-god-repeater-" var))
       (god-local-mode-map "g")
       "God repeater"
       (,var (call-interactively (key-binding (kbd ,(concat "M-" var))))
             ,(let ((function-name (format "%s" (lookup-key
                                                 (current-global-map)
                                                 (kbd (concat "M-" var))))))
                (if (> (length function-name) 50)
                    (concat "M-" var)
                  function-name)))))

  (defmacro god-repeater--set-characters (&rest vars)
    (let ((forms (mapcar 'god-repeater--set-hydra-function vars)))
      `(progn ,@forms)))

  (god-repeater--set-characters
   "q" "w" "e" "r" "t" "y" "u" "i" "o"
   "p" "a" "s" "d" "f" "g" "h" "j" "k"
   "l" "z" "x" "c" "v" "b" "n" "m"
   "1" "2" "3" "4" "5" "6" "7" "8" "9"
   "0" "!" "@" "#" "$" "%" "^" "&" "*"
   "(" ")" "_" "+" "{" "}" "|" ":" "\""
   "<" ">" "?" "-" "=" "[" "]" ";" "'"
   "\\" "," "." "/" "`" "~")

  ;; Special cases
  (defhydra hydra-god-repeater-g (god-local-mode-map "g") ("g" goto-line))
  (defhydra hydra-god-repeater-G (god-local-mode-map "g") ("G" goto-line)))

(use-package indy
  :ensure t)

(use-package iy-go-to-char
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (defun attic-js2-hook ()
    (attic-lock))
  (add-hook 'js2-mode-hook 'attic-js2-hook))

(use-package key-chord
  :ensure t
  :init
  (key-chord-mode t)
  :config
  (key-chord-define-global "xs" '(lambda ()
                                   (interactive)
                                   (god-local-mode 1)
                                   (save-buffer)))

  (key-chord-define helm-map ";j" 'helm-keyboard-quit)
  (key-chord-define-global ";j" 'attic-lock)
  (key-chord-define attic-mode-map ";j" 'attic-lock)
  (key-chord-define isearch-mode-map ";j" 'isearch-abort))

(use-package linum
  :config
  ;; Always display 2 columns in linum mode (no stuttering)
  (setq linum-format (quote "%3d"))
  (setq linum-disabled-modes-list '(
    mu4e-compose-mode
    mu4e-headers-mode
    mu4e-main-mode)))

(use-package litable
  :ensure t)

(use-package macrostep
  :ensure t)

(use-package magit
  :ensure t
  :config
  (bind-key "RET" (lambda () (interactive) (magit-visit-item t)) magit-status-mode-map)
  (bind-key "g" 'magit-refresh magit-status-mode-map)
  (bind-key ";" 'attic-semi-colon/body magit-status-mode-map)
  (bind-key ";" 'attic-semi-colon/body magit-revision-mode-map)
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package material-theme
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :init
  (bind-key "M-N" 'mc/mark-next-like-this attic-mode-map)
  (bind-key "M-P" 'mc/mark-previous-like-this attic-mode-map)
  :config
  (bind-key "<return>" 'newline mc/keymap)
  (multiple-cursors-mode t))

(use-package neotree
  :ensure t
  :config
  ;; For this module to work properly you need to have the package window-numbering
  ;; installed and activated. neotree tries to take focus when toggling / changing
  ;; directory and I use the window-numbering package to return to the previous
  ;; window.

  (defmacro neotree-root-hook (function-name)
    `(defadvice ,function-name (after ,function-name activate)
       (if neotree-active
           (set-neo-root-project))))

  (neotree-root-hook dired-find-file)
  (neotree-root-hook select-window-1)
  (neotree-root-hook select-window-2)
  (neotree-root-hook select-window-3)
  (neotree-root-hook select-window-4)
  (neotree-root-hook select-window-5)
  (neotree-root-hook select-window-6)
  (neotree-root-hook select-window-7)
  (neotree-root-hook select-window-8)
  (neotree-root-hook select-window-9)
  (neotree-root-hook select-window-0)
  (neotree-root-hook magit-visit-item)
  (neotree-root-hook neotree-enter)
  (neotree-root-hook helm-find-files)
  (neotree-root-hook helm-ls-git-ls)
  (neotree-root-hook helm-mini)
  (neotree-root-hook kill-this-buffer)

  (setq neo-theme 'ascii
        neo-window-width 30
        ;; Made up options
        neotree-active nil
        neotree-ignore-list '(erc-mode
                              sauron-mode
                              help-mode
                              eww-mode
                              doc-view-mode
                              top-mode
                              Custom-mode
                              message-mode
                              twittering-mode
                              alchemist-iex-mode
                              shell-mode)
        neotree-overlay nil)
  (defface neotree-overlay-face
    '((t :background "#696969"))
    "" :group 'neotree)

  (bind-key "RET" 'neotree-enter neotree-mode-map)
  (bind-key "c s a" 'helm-bookmarks neotree-mode-map)
  (bind-key "z" 'helm-mini neotree-mode-map)
  (bind-key ";" 'attic-semi-colon/body neotree-mode-map)
  (defun set-neo-root-project ()
    (interactive)
    (unless (or (member major-mode neotree-ignore-list)
                (is-tramp-mode)
                (equal (buffer-name) " *NeoTree*"))
      (let ((previous-window (window-numbering-get-number)))
        (neotree-dir (or (magit-get-top-dir) default-directory))
        (select-window-by-number previous-window)
        (neotree-find)
        (if neotree-overlay (delete-overlay neotree-overlay))
        (setq neotree-overlay (make-overlay (point) (progn (end-of-line) (point))))
        (beginning-of-line)
        (overlay-put neotree-overlay 'face 'neotree-overlay-face)
        (select-window-by-number previous-window))))
  (defun attic-neotree-toggle ()
    (interactive)
    (setq neotree-active (not (get-buffer-window " *NeoTree*")))
    (neotree-toggle)
    (if neotree-active
        (let ((previous-window (window-numbering-get-number)))
          (select-window-by-number (+ 1 previous-window))
          (set-neo-root-project)))))

(use-package org
  :config
  (org-babel-do-local-languages
   'org-babel-load-languages
   '(sh . t))
  (setq org-log-done 'time)
  (setq org-capture-templates '())
  (setq org-capture-templates
        '(("1" "Done" entry
           (file+headline "~/Documents/notes/Org/Done.org" "Done")
           (file "~/.emacs.d/Templates/Done.orgtpl"))
          ("2" "Retro" entry
           (file+headline "~/Documents/notes/Org/Retro.org" "Done")
           (file "~/.emacs.d/Templates/Retro.orgtpl"))
          ("3" "Todo" entry
           (file+headline "~/Documents/notes/Org/Todo.org" "Todo")
           (file "~/.emacs.d/Templates/Todo.orgtpl"))))
  (defun add-my-todos-to-org (list)
    "Adds All the files in Todo directory to my list of todo subjects."
    (let ((c 0)
          (r '()))
      (while (nth c list)
        (let ((key (char-to-string (+ c 97)))
              (val (nth c list)))
          (add-to-list 'org-capture-templates
                       `(,key ,val entry
                              (file+headline ,(concat "~/Documents/notes/Org/Todo/" val) ,val)
                              (file "~/.emacs.d/Templates/GenericTodo.orgtpl")))
          (setq c (+ c 1))))))

  (defun org-keys-hook ()
    (define-prefix-command 'org-mode-custom-map)
    (define-key org-mode-map (kbd "C-c C-o") 'org-mode-custom-map)
    (define-key org-mode-custom-map (kbd "C-l") 'browse-url-at-point)
    (define-key org-mode-custom-map (kbd "C-t") 'org-todo))
  (defun attic-org-mode-hook ()
    (attic-lock))
  (add-my-todos-to-org
   (directory-files
    (expand-file-name "~/Documents/notes/Org/Todo")
    nil
    "^\\([^#|^.]\\|\\.[^.]\\|\\.\\..\\)"))
  (add-hook 'org-mode-hook 'attic-org-mode-hook)
  (add-hook 'org-mode-hook 'org-keys-hook))

(use-package paredit
  :ensure t
  :config
  (define-key paredit-mode-map (kbd "M-R") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "C-j") 'iy-go-up-to-char)
  (define-key paredit-mode-map (kbd "C-z") 'paredit-reindent-defun)
  (define-key paredit-mode-map (kbd "C-q") 'paredit-backward-delete)
  (define-key paredit-mode-map (kbd "M-q") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-c C-p") 'paredit-copy-sexp-up)
  (define-key paredit-mode-map (kbd "C-c C-n") 'paredit-copy-sexp-down)
  (bind-key ";"
            (lambda ()
              (interactive)
              (if god-local-mode
                  (attic-semi-colon/body)
                (paredit-semicolon)))
            paredit-mode-map)
  (define-key paredit-mode-map (kbd ")")
    (lambda () (interactive)
      (if god-local-mode
          (call-interactively (key-binding (kbd "C-)")))
        (paredit-close-round))))
  (define-key paredit-mode-map (kbd "(")
    (lambda () (interactive)
      (if (or (not god-local-mode) (region-active-p))
          (paredit-open-round)
        (call-interactively
         (key-binding (kbd "C-("))))))

  (defun paredit-copy-sexp-down ()
    (interactive)
    (let ((prev-column (current-column)))
      (paredit-forward)
      (paredit-backward)
      (kill-sexp)
      (yank)
      (paredit-newline)
      (yank)
      (paredit-reindent-defun)
      (paredit-backward)
      (move-to-column prev-column)))

  (defun paredit-copy-sexp-up ()
    (interactive)
    (let ((prev-column (current-column)))
      (paredit-forward)
      (paredit-backward)
      (kill-sexp)
      (yank)
      (paredit-backward)
      (let ((copy-indent (current-column)))
        (beginning-of-line)
        (open-line 1)
        (insert (make-string copy-indent 32))
        (yank)
        (paredit-reindent-defun)
        (paredit-backward)
        (move-to-column prev-column)))))

(use-package pcmpl-args
  :ensure t)

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave))

(use-package rainbow-delimiters
  :ensure t)

(use-package redo+
  :ensure t
  :init
  (bind-key "M-_" 'redo attic-mode-map))

(use-package ruby
  :config
  ;; Don't use deep indent in Ruby
  (setq ruby-deep-indent-paren nil)

  (defun attic-ruby-hook ()
    (electric-pair-mode)
    (setq-local helm-dash-docsets '("Ruby")))
  (add-hook 'ruby-mode-hook 'attic-ruby-hook))

(use-package rust-mode
  :ensure t
  :config
  (defun attic-rust-hook ()
    (attic-lock)
    (electric-pair-mode)
    (setq-local tab-width 4)
    (setq-local helm-dash-docsets '("Rust")))
  (add-hook 'rust-mode-hook 'attic-rust-hook))

(use-package s
  :ensure t)

(use-package sauron
  :ensure t
  :config
  (setq sauron-active nil)
  (setq sauron-hide-mode-line t)
  (setq sauron-separate-frame nil)
  (setq sauron-max-line-length (- (window-total-width) 10))
  ;; Custom made variable for max line height
  (setq sauron-max-line-height 4)

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
    (sauron-activate-event-prev)))

(use-package scheme
  :config
  (defun attic-scheme-mode-hook ()
    (attic-lock)
    (paredit-mode 1))
  (add-hook 'scheme-mode-hook 'attic-scheme-mode-hook))

(use-package scheme-complete
  :ensure t
  :config
  (eval-after-load 'scheme
    '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))
  (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
  (add-hook 'scheme-mode-hook
            (lambda ()
              (make-local-variable 'eldoc-documentation-function)
              (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
              (eldoc-mode))))

(use-package transpose-mark
  :ensure t
  :init
  (bind-key "C-c C-t" 'transpose-mark attic-mode-map))

(use-package twittering-mode
  :ensure t
  :config
  (setq twittering-icon-mode t
        ;; Use master password for twitter instead of authenticating every time
        twittering-cert-file "/etc/ssl/certs/ca-bundle.crt"
        twittering-use-master-password t)
  (bind-key "s" 'twittering-search twittering-mode-map)
  (bind-key ";" 'attic-semi-colon/body twittering-mode-map)
  (bind-key "q" (lambda () (interactive) (switch-to-buffer nil)) twittering-mode-map)
  (bind-key "w" 'delete-window twittering-mode-map)
  (add-hook 'twittering-mode-hook 'toggle-modeline))

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(use-package window-numbering
  :ensure t
  :init
  (window-numbering-mode t))

(use-package winner
  :config
  (winner-mode t)
  ;; Buffers to be ignored by Winner
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*dvc-error*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*")))

(use-package wrap-region
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode t))

(if window-system
    (require 'git-gutter-fringe+)
  (require 'git-gutter+))

(require 'sticky-windows)

;;;; TODO require emacs lisp?
(defun attic-emacs-lisp-hook ()
  (attic-lock)
  (paredit-mode 1)
  (setq-local helm-dash-docsets '("Emacs Lisp")))

(add-hook 'emacs-lisp-mode-hook 'attic-emacs-lisp-hook)

;; Modes
(display-battery-mode t)
(show-paren-mode t)
(wrap-region-global-mode t)
(electric-pair-mode t)

(provide 'attic-packages)

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

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Multiple Cursors to be loaded first to use the correct path
(use-package multiple-cursors
  :load-path "~/.emacs.d/multiple-cursors.el/"
  :bind* (("M-K" . mc/mark-previous-like-this)
          ("M-J" . mc/mark-next-like-this))
  :bind (:map mc/keymap
              ("<return>" . newline))
  :init
  (unless (file-exists-p "~/.emacs.d/multiple-cursors.el/multiple-cursors.el")
    (shell-command "cd ~/.emacs.d && git submodule init && git submodule update"))
  :config
  (multiple-cursors-mode t))

(use-package ac-cider
  :ensure t)

(use-package ace-jump-mode
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'scheme-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'racket-mode-hook 'aggressive-indent-mode))

(use-package alchemist
  :ensure t
  :bind (:map alchemist-mode-map
              ("M-N" . mc/mark-next-like-this)
              ("M-P" . mc/mark-previous-like-this)
              ("M-n" . alchemist-goto-jump-to-next-def-symbol)
              ("M-p" . alchemist-goto-jump-to-previous-def-symbol))
  :init
  (setq alchemist-hooks-compile-on-save nil
        alchemist-hooks-test-on-save nil)
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode))

(use-package auto-complete
  :init
  (setq ac-auto-show-menu 0.3
        ac-candidate-limit 15
        ac-delay 0.3)
  :config
  (set-face-attribute 'ac-candidate-face nil :inherit 'company-tooltip-common)
  (set-face-attribute 'ac-completion-face nil :inherit 'company-preview-common :background nil :foreground nil)
  (set-face-attribute 'ac-selection-face nil :inherit 'company-tooltip-common-selection))

(use-package back-button
  :ensure t
  :init
  (back-button-mode 1))

(use-package bind-key
  :ensure t
  :bind* (("C-c C-o" . switch-to-minibuffer)
          ("C-SPC" . attic-main/body)
          ("M-u" . redo)
          ("C-q" . backward-delete-char)
          ("C-S-V" . x-clipboard-yank)
          ("C-S-C" . clipboard-kill-ring-save)
          ("C-x C-2" . split-window-below)
          ("C-x C-3" . split-window-right)
          ("C-x C-4" . delete-window)
          ("C-x C-8" . fill-paragraph)
          ("C-x C-k" . kill-this-buffer)
          ("M-P" . evil-paste-pop-or-kill-ring)
          ("M-+" . align-regexp)
          ("M-C" . capitalize-previous-word)
          ("M-i" . tab-to-tab-stop-line-or-region)
          ("M-I" . tab-to-tab-stop-line-or-region-backward)
          ([f1] . get-current-buffer-major-mode)
          ([f3] . describe-key))
  :config
  (global-unset-key "\C-x\C-z")
  (global-unset-key "\C-z"))

(use-package buffer-move
  :ensure t)

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'toml-mode-hook 'cargo-minor-mode))

(use-package clojure-mode
  :ensure t
  :bind (:map clojure-mode-map
              ("C-x C-e" . cider-eval-last-sexp)))

(use-package clj-refactor
  :ensure t
  :bind (:map clojure-mode-map
              ("C-c C-a" . cljr-refactor-all/body))
  :config
  (defhydra cljr-refactor-all (:color blue)
    "[Clojure refactor]"
    ("c"  hydra-cljr-cljr-menu/body "cljr-menu")
    ("o"  hydra-cljr-code-menu/body "code-menu")
    ("h"  hydra-cljr-help-menu/body "help-menu")
    ("n"  hydra-cljr-ns-menu/body "ns-menu")
    ("p"  hydra-cljr-project-menu/body "project-menu")
    ("t"  hydra-cljr-toplevel-form-menu/body "toplevel-form-menu"))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package cljr-helm
  :ensure t
  :bind (:map cljr-helm  ("C-c C-l" . cljr-helm)))

(use-package cider
  :ensure t
  :init
  (setq cider-auto-jump-to-error nil)
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package comint
  :init
  (setq tramp-default-method "ssh"          ; uses ControlMaster
        comint-scroll-to-bottom-on-input t  ; always insert at the bottom
        comint-scroll-to-bottom-on-output nil ; always add output at the bottom
        comint-scroll-show-maximum-output t ; scroll to show max possible output
        comint-completion-autolist t     ; show completion list when ambiguous
        comint-input-ignoredups t           ; no duplicates in command history
        comint-completion-addsuffix t       ; insert space/slash after file completion
        comint-buffer-maximum-size 20000    ; max length of the buffer in lines
        comint-prompt-read-only nil         ; if this is t, it breaks shell-command
        comint-get-old-input (lambda () "") ; what to run when i press enter on a
                                        ; line above the current prompt
        comint-input-ring-size 5000         ; max shell history size
        protect-buffer-bury-p nil)

  (defun make-my-shell-output-read-only (text)
    "Add to comint-output-filter-functions to make stdout read only in my shells."
    (interactive)
    (if (equal major-mode 'shell-mode)
        (let ((inhibit-read-only t)
              (output-end (process-mark (get-buffer-process (current-buffer)))))
          (put-text-property comint-last-output-start output-end 'read-only t))))
  :config
  (add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer))

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("M-f" . company-complete-selection)
              ("<return>" . company-abort-and-newline)
              ("C-m" . company-abort-and-newline)
              ("M-h" . helm-company)
              ("M-j" . yas/expand)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1)
  (defun company-abort-and-newline ()
    (interactive)
    (company-abort)
    (newline))
  :config
  (add-hook 'alchemist-iex-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'scheme-mode-hook 'company-mode)
  (add-hook 'erlang-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'elm-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-elm))

(use-package company-racer
  :ensure t)

(use-package compile
  :bind (:map compilation-mode-map
              ("<SPC>" . attic-main/body))
  :init
  ;; Scroll on in the *compilation* buffer
  (setq compilation-scroll-output t))

(use-package css-mode
  :mode ("\\.less\\'"
         "\\.scss\\'"))

(use-package dash
  :ensure t)

(use-package delsel
  :config
  ;; Delete seleted text when typing
  (delete-selection-mode 1))

(use-package dired
  :config
  (defun ensure-buffer-name-begins-with-exl ()
    "change buffer name to end with slash"
    (let ((name (buffer-name)))
      (if (not (string-match "/$" name))
          (rename-buffer (concat "!" name) t))))
  (add-hook 'dired-mode-hook 'ensure-buffer-name-begins-with-exl))

(use-package time
  :init
  (setq display-time-default-load-average nil)
  :config
  (display-time-mode 1))

(use-package doc-view
  :bind (:map doc-view-mode-map
              ("j" . doc-view-next-line-or-next-page)
              ("k" . doc-view-previous-line-or-previous-page)
              ("l" . image-forward-hscroll)
              ("h" . image-backward-hscroll)))

(use-package dockerfile-mode
  :ensure t)

(use-package elec-pair
  :init
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\{ . ?\})))
  :config
  (electric-pair-mode t)
  (add-hook* 'message-mode-hook (electric-pair-mode -1))
  (add-hook* 'clojure-mode-hook (electric-pair-mode -1))
  (add-hook 'prog-mode-hook 'electric-pair-mode))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package elfeed
  :ensure t
  :init
  (setq elfeed-search-filter "@12-months-ago"
        elfeed-feeds
        '("http://feeds.5by5.tv/changelog"
          "http://feeds.twit.tv/floss.xml"
          "http://thecommandline.net/cmdln"
          "http://cloudevangelist.jellycast.com/podcast/feed/123"))
  (defun url-copy-file-to-path (url path)
    (let* ((file-name (car (last (split-string  url "/"))))
           (full-path (expand-file-name file-name path)))
      (unless (file-exists-p full-path)
        (url-copy-file url full-path))
      full-path))
  (defun elfeed-open-in-emms ()
    (interactive)
    (save-excursion
      (goto-char 1)
      (let ((done nil))
        (while (and (re-search-forward ".*mp3$" nil t)
                    (not done))
          (backward-char 1)
          (when (get-text-property (point) 'shr-url)
            (setq kill-ring (cdr kill-ring))
            (let* ((url (string-remove-prefix  "Copied " (shr-copy-url)))
                   (full-path (url-copy-file-to-path url "~/Podcasts/")))
              (emms-play-file full-path)
              (setq done t)))))))
  :config
  (bind-key "j" 'elfeed-open-in-emms elfeed-show-mode-map))

(use-package elixir-mode
  :ensure t)

(use-package elm-mode
  :ensure t
  :config
  (defun elm-reactor ()
    (interactive)
    (async-shell-command "elm-reactor" "*elm-reactor*"))
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(use-package elscreen
  :ensure t
  :init
  (elscreen-start)
  (setq elscreen-display-screen-number nil
        elscreen-prefix-key nil
        elscreen-tab-display-control nil
        elscreen-tab-display-kill-screen nil)
  (defun elscreen-create-initial-5-screens ()
    (interactive)
    (elscreen-kill-others)
    (elscreen-create) (elscreen-create) (elscreen-create)
    (elscreen-create) (elscreen-create) (elscreen-kill 0)
    (elscreen-goto 1))
  (elscreen-create-initial-5-screens)
  :config
  (defun elscreen-goto-template (num)
    `(defun ,(read  (concat "elscreen-goto-" (number-to-string num))) ()
       ,(concat "Go to elscreen workspace " (number-to-string num) ".")
       (interactive)
       (elscreen-goto ,num)))
  (defmacro elscreen-goto-workspace-list (&rest nums)
    (let ((forms (mapcar 'elscreen-goto-template nums)))
      `(progn ,@forms)))
  (elscreen-goto-workspace-list 1 2 3 4 5))

(use-package emms
  :ensure t
  :init
  (setq emms-setup-default-player-list '(emms-player-vlc)
        emms-volume-change-amount 5)
  :config
  (when (and (file-exists-p "~/Music/")
             (> (length (directory-files "~/Music/")) 2))
    (emms-standard)
    (emms-default-players)
    (emms-add-directory-tree "~/Music/")
    (emms-toggle-repeat-playlist)
    (emms-shuffle)
    (emms-playing-time-enable-display)))

(use-package erc
  :bind (:map erc-mode-map
              ("C-M-m" . erc-send-current-line)
              ("RET" . erc-no-return))
  :init
  (defadvice attic/erc (after attic-ad/attic/erc-after activate)
    (setq erc-password nil))
  (defun attic/erc ()
    (interactive)
    (load "~/.erc.gpg")
    (erc :server "irc.freenode.net"
         :port 6667
         :nick erc-nick
         :password erc-password))
  (defun erc-no-return ()
    (interactive)
    (message "Use C-M-m to send"))
  (setq erc-scrolltobottom-mode 1
        erc-nick "kwrooijen"
        erc-prompt-for-password nil
        erc-ignore-list '("*Flowdock*" "Flowdock" "-Flowdock-")
        erc-hide-list '("JOIN" "PART" "QUIT"))
  :config
  (erc-truncate-mode 1)
  (set-face-attribute 'erc-nick-default-face nil :foreground "#528B8B")
  (add-hook 'erc-mode-hook 'toggle-modeline))

(use-package erlang
  :ensure t
  :mode ("\\.app.src\\'"
         "rebar.config")
  :commands erlang-mode
  :init
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  :config
  (setq-mode-local erlang-mode tab-width 4))

(use-package etags
  :init
  ;; Ctags location
  (setq tags-file-name "~/.ctags")
  ;; Reread a TAGS table without querying, if it has changed.
  (setq tags-revert-without-query t))

(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-d" . delete-char)
              ("<SPC>" . attic-main/body)
              ("TAB" . evil-bracket-open)
              :map evil-visual-state-map
              ("<SPC>" . attic-main/body))
  :bind* (("M-q" . evil-normal-state))
  :config
  (add-hook 'minibuffer-setup-hook #'turn-off-evil-mode)
  (add-hook 'prog-mode-hook #'turn-on-evil-mode)
  (add-hook 'org-mode-hook #'turn-on-evil-mode)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  ;; TODO fix properly
  (defun evil-bracket-open ()
    (interactive)
    (if (member major-mode '(lisp-interaction-mode scheme-mode emacs-lisp-mode))
        (evil-lispy/enter-state-left))
    (indent-for-tab-command))
  (defun evil-normal-state-and-save ()
    (interactive)
    (evil-normal-state)
    (save-buffer)))

(use-package evil-nerd-commenter
  :ensure t
  :bind* (("C-/" . evilnc-comment-or-uncomment-lines)))

(use-package evil-numbers
  :ensure t
  :bind* (("C-c C-=" . evil-numbers/inc-at-pt)
          ("C-c C--" . evil-numbers/dec-at-pt)))

(use-package evil-paredit
  :ensure t
  :config
  (add-hook 'paredit-mode-hook 'evil-paredit-mode))

(use-package evil-lispy
  :ensure t)

(use-package expand-region
  :ensure t
  :bind* (("M-@" . er/expand-region)))

(use-package fancy-battery
  :ensure t
  :init
  (fancy-battery-mode))

(use-package files
  :init
  (setq remote-file-name-inhibit-cache nil)
  ;; Make sure the file ends with a newline
  (setq require-final-newline t)
  ;; Backup ~ files in seperate directory
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  ;; No confirmation when creating new buffer
  (setq confirm-nonexistent-file-or-buffer nil))

(use-package flyspell
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-buffer))

(use-package flycheck-rust
  :ensure t)

(use-package flyspell)

(use-package fringe
  :config
  (set-fringe-mode '(1 . 0)))

(use-package geiser
  :ensure t
  :init
  (setq geiser-popup--no-jump t)
  :config
  (defun helm-geiser ()
    (interactive)
    (unless (geiser-doc--manual-available-p)
      (error "No manual available"))
    (let ((symbol (helm :sources (helm-build-sync-source "Geiser"
                                   :candidates (geiser-completion--symbol-list ""))
                        :buffer "*helm Geiser")))
      (geiser-doc--external-help geiser-impl--implementation
                                 symbol
                                 (geiser-eval--get-module))))
  (defun evil-geiser-eval-last-sexp ()
    (interactive)
    (save-excursion
      (forward-char 1)
      (geiser-eval-last-sexp nil))))

(use-package gist
  :ensure t
  :init
  (defun send-to-gist (answer)
    (interactive "cSend region to Gist?: (y/n) ")
    (if (equal answer ?\y) (gist-region (region-beginning) (region-end)))))

(use-package git-gutter+
  :if (not window-system)
  :ensure t
  :config
  (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :if window-system
  :ensure t
  :config
  (global-git-gutter+-mode t))

(use-package grep
  :bind (:map grep-mode-map
              ("n" . next-line)
              ("p" . previous-line)
              ("TAB" . grep-error-preview)
              ("v" . scroll-up-command)
              ("z" . helm-buffers-list))
  :init
  (defun grep-error-preview ()
    (interactive)
    (compile-goto-error)
    (switch-to-buffer-other-window "*grep*")))

(use-package hackernews
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package helm
  :ensure t
  :bind* (("M-[" . helm-resume)
          ("M-x" . helm-M-x))
  :bind (:map helm-map
              ("C-b" . nil)
              ("C-f" . nil)
              ("M-b" . nil)
              ("M-f" . forward-word)
              ("M-s" . helm-select-action)
              ("TAB" . helm-execute-persistent-action)
              ("M-?" . helm-help)
              ("<RET>" . my/helm-exit-minibuffer))
  :init
  (setq
   ;; truncate long lines in helm completion
   helm-truncate-lines t
   ;; may be overridden if 'ggrep' is in path (see below)
   helm-grep-default-command "grep -a -d skip %e -n%cH -e %p %f"
   helm-grep-default-recurse-command "grep --exclude-dir=\"dist\" -a -d recurse %e -n%cH -e %p %f"
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
   ;; fuzzy matching
   helm-buffers-fuzzy-matching t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-echo-input-in-header-line t
   ;; Don't ask to create new file
   helm-ff-newfile-prompt-p nil
   helm-reuse-last-window-split-state t
   helm-ff-transformer-show-only-basename nil
   ;; Split window down
   helm-split-window-in-side-p t
   ;; Split when multiple windows open
   helm-swoop-split-with-multiple-windows t
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
   helm-autoresize-min-height 30
   helm-bookmark-show-location t
   helm-always-two-windows t
   helm-imenu-execute-action-at-once-if-one nil)
  :config
  (use-package helm-files
    :config
    (set-face-attribute 'helm-ff-directory nil
                        :foreground 'unspecified
                        :background 'unspecified
                        :inherit 'dired-directory)
    (set-face-attribute 'helm-ff-file nil
                        :foreground 'unspecified
                        :background 'unspecified
                        :inherit 'default)
    (set-face-attribute 'helm-ff-symlink nil
                        :foreground 'unspecified
                        :background 'unspecified
                        :inherit 'dired-symlink))
  (use-package helm-buffers
    :config
    (set-face-attribute 'helm-buffer-directory nil
                        :foreground 'unspecified
                        :background 'unspecified
                        :inherit 'dired-directory)
    (set-face-attribute 'helm-buffer-file nil
                        :foreground 'unspecified
                        :background 'unspecified
                        :inherit 'default)
    (set-face-attribute 'helm-buffer-process nil
                        :foreground "#cd8500"
                        :background 'unspecified))
  ;; Try to hide source header as much as possible
  (set-face-attribute 'helm-source-header nil :height 0.1 :background "#000"  :foreground "#000")

  ;; Work around for the [Display not ready] error when typing too awesomely fast
  (defun my/helm-exit-minibuffer ()
    (interactive)
    (helm-exit-minibuffer))
  (defhydra helm-like-unite ()
    ("q" nil "Quit" :color blue)
    ("<spc>" helm-toggle-visible-mark "mark")
    ("a" helm-toggle-all-marks "(un)mark all")
    ("v" helm-execute-persistent-action)
    ("g" helm-beginning-of-buffer "top")
    ("h" helm-previous-source)
    ("l" helm-next-source)
    ("G" helm-end-of-buffer "bottom")
    ("j" helm-next-line "down")
    ("J" helm-next-source "down source")
    ("K" helm-prev-source "up source")
    ("k" helm-previous-line "up")
    ("i" nil "cancel"))
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
  :bind* (("C-c C-s C-d" . helm-dash))
  :config
  (setq-mode-local clojure-mode helm-dash-docsets '("Clojure"))
  (setq-mode-local elixir-mode helm-dash-docsets '("Elixir"))
  (setq-mode-local emacs-lisp-mode helm-dash-docsets '("Emacs Lisp"))
  (setq-mode-local erlang-mode helm-dash-docsets '("Erlang"))
  (setq-mode-local haskell-mode helm-dash-docsets '("Haskell"))
  (setq-mode-local ruby-mode helm-dash-docsets '("Ruby"))
  (setq-mode-local rust-mode helm-dash-docsets '("Rust")))

(use-package helm-descbinds
  :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode 1))

(use-package helm-swoop
  :ensure t
  :bind* (("C-c C-s C-s" . helm-multi-swoop))
  :bind (:map helm-swoop-map
              ("M-e" . helm-swoop-edit)))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-symbol
  :ensure t
  :bind  (:map prog-mode-map
               ("M-j" . highlight-symbol-next)
               ("M-k" . highlight-symbol-prev))
  :init
  (setq highlight-symbol-ignore-list
        '("def" "defun" "define" "defmacro"
          "use-package" "defmodule" "do"
          "require" "alias" "use" "let" "="
          "-" "+" "/"))
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 0))

(use-package hl-defined
  :ensure t
  :init
  (setq hdefd-highlight-type 'functions)
  :config
  (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode)
  (add-hook 'scheme-mode-hook 'hdefd-highlight-mode)
  (add-hook 'clojure-mode-hook 'hdefd-highlight-mode)
  (set-face-attribute 'hdefd-functions nil :foreground nil :inherit 'font-lock-function-name-face))

(use-package hl-line
  :init
  (add-hook 'prog-mode-hook 'hl-line-mode))

(use-package hydra
  :ensure t
  :config
  (defhydra attic-emms (:color red)
    "EMMS"
    ("a" emms-pause "Pause")
    ("g" emms-playlist-mode-go "Playlist")
    ("n" emms-next "Next")
    ("p" emms-previous "Previous")
    ("]" emms-volume-raise "+")
    ("[" emms-volume-lower "-")
    ("f" emms-seek-forward "f")
    ("b" emms-seek-backward "b")
    ("q" nil "Quit" :color blue)))

(use-package indy
  :ensure t
  :init
  (setq indy-rules
        '((erlang-mode
           .
           (((indy--prev 'indy--ends-on "->" "fun" "of" "begin") (indy--prev-tab 1))
            ((indy--prev 'indy--ends-on ";") (indy--prev-tab -1))
            ((and (indy--prev 'indy--ends-on "end") (indy--current 'indy--starts-with "end")) (indy--prev-tab -1))
            ((indy--current 'indy--ends-on "end") (indy--prev-tab -1))
            ((and (indy--prev 'indy--ends-on "[") (indy--current 'indy--starts-with "]")) (indy--prev-tab))
            ((and (indy--prev 'indy--ends-on "{") (indy--current 'indy--starts-with "}")) (indy--prev-tab))
            ((and (indy--prev 'indy--ends-on "(") (indy--current 'indy--starts-with ")")) (indy--prev-tab))
            ((indy--current 'indy--starts-with "]" "}" ")") (indy--prev-tab -1))
            ((indy--prev 'indy--ends-on "[" "{" "(") (indy--prev-tab 1))
            ((indy--prev 'indy--ends-on ",") (indy--prev-tab))))))
  :config
  (add-hook 'erlang-mode-hook 'indy-mode))

(use-package key-chord
  :ensure t
  :config
  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))
  (key-chord-define-global "xs" 'evil-normal-state-and-save))

(use-package linum
  :init
  (setq linum-format (quote "%3d")
        linum-disabled-modes-list '(mu4e-compose-mode
                                    mu4e-headers-mode
                                    mu4e-main-mode))
  :config
  (set-face-attribute 'linum nil :inherit 'default :background nil))

(use-package lispy
  :ensure t
  :bind (:map lispy-mode-map
              ("TAB" . lispy-left-no-mark)
              ("d" . lispy-different)
              ("o" . lispy-other-mode)
              ("f" . lispy-flow)
              ("i" . evil-insert)
              ("e" . attic/lispy--eval)
              ("J" . evil-join))
  :init
  (defun attic/lispy--eval ()
    (interactive)
    (if (equal major-mode 'scheme-mode)
        (geiser-eval-next-sexp nil)
      (special-lispy-eval)))
  :config
  (defun lispy-left-no-mark ()
    (interactive)
    (deactivate-mark)
    (lispy-left 1)))

(use-package macrostep
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (bind-key "g" 'magit-refresh magit-status-mode-map))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :bind (:map mu4e-main-mode-map
              :map mu4e-main-mode-map
              ("p" . previous-line)
              ("n" . next-line)
              ("z" . helm-buffers-list)
              ("v" . scroll-up-command)
              :map mu4e-compose-mode-map
              ("M-s" . mml-secure-sign-pgp)
              :map mu4e-headers-mode-map
              ("v" . scroll-up-command)
              :map mu4e-view-mode-map
              ("f" . epa-mail-verify)
              ("v" . scroll-up-command))
  :init
  (setq message-send-mail-function 'smtpmail-send-it
        mu4e-get-mail-command "offlineimap"
        mu4e-maildir (expand-file-name "~/Mail")
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("mail.hover.com" 587 nil nil))
        smtpmail-default-smtp-server "mail.hover.com"
        smtpmail-smtp-server "mail.hover.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        mu4e-update-interval 60
        message-kill-buffer-on-exit t
        mu4e-hide-index-messages t
        ;; Requires html2text package
        mu4e-html2text-command "html2text -utf8 -width 72"
        mu4e-view-show-images t))

(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-maildirs-extension
  :ensure t
  :config
  (mu4e-maildirs-extension))

(use-package org
  :init
  (setq org-log-done 'time
        org-capture-templates '()
        org-src-fontify-natively t
        org-ellipsis " ⤵"))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list '("◉"))
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("C-w" . paredit-kill-region)
              ("M-R" . paredit-splice-sexp-killing-backward)
              ("C-c C-r" . paredit-reindent-defun)
              ("M-j" . paredit-join-sexps))
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'elixir-mode-hook 'paredit-mode)
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (add-hook 'racket-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode))

(use-package paren
  :config
  (show-paren-mode t))

(use-package prog-mode
  :init
  (setq prettify-lisp-alist '(("lambda" . 955)))
  (add-hook 'scheme-mode-hook 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
  (add-hook 'racket-mode-hook 'prettify-symbols-mode)
  (add-hook* 'scheme-mode-hook (setq-local prettify-symbols-alist prettify-lisp-alist))
  (add-hook* 'emacs-lisp-mode-hook (setq-local prettify-symbols-alist prettify-lisp-alist))
  (add-hook* 'clojure-mode-hook (setq-local prettify-symbols-alist prettify-lisp-alist))
  (add-hook* 'racket-mode-hook (setq-local prettify-symbols-alist prettify-lisp-alist)))

(use-package racer
  :ensure t
  :init
  (setq racer-cmd "/usr/local/bin/racer"
        racer-rust-src-path "/usr/local/src/rust/src/")
  :config
  (setq-mode-local rust-mode company-backends '(company-racer))
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'racer-turn-on-eldoc))

(use-package racket-mode
  :ensure t)

(use-package redo+
  :ensure t
  :bind* (("M-_" . redo)))

(use-package ruby-mode
  :mode ("Gemfile$"
         "Rakefile$"
         "\\.gemspec$"
         "\\.rake$"
         "\\.rb$"
         "\\.ru$")
  :init
  (setq ruby-deep-indent-paren nil))

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
              ("C-c C-c C-z" . racer-find-definition))
  :config
  (setq-mode-local rust-mode tab-width 4))

(use-package s
  :ensure t)

(use-package scheme-complete
  :ensure t
  :config
  (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t))

(use-package simple
  :init
  ;; C-u C-SPC will repeat if C-SPC is pressed again
  (setq set-mark-command-repeat-pop t)
  :config
  (setq-mode-local fundamental-mode require-final-newline nil)
  ;; Kill buffer on remote machine
  (defadvice async-shell-command (before attic-ad/async-shell-command activate)
    (when (get-buffer "*Async Shell Command*")
      (kill-buffer "*Async Shell Command*"))))

(use-package string-edit
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package term
  :init
  (defun term-toggle-mode ()
    (interactive)
    (if (term-in-line-mode)
        (term-char-mode)
      (term-line-mode)))
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (setq-mode-local term-mode yas-dont-activate t))

(use-package tiny
  :ensure t
  :bind* (("C-;" . tiny-expand)))

(use-package toml-mode
  :ensure t)

(use-package tramp
  :init
  (defun is-tramp-mode ()
    (file-remote-p default-directory))
  ;; Immediately reread remote directories
  (setq tramp-completion-reread-directory-timeout nil)
  ;; Set Tramp backup file location
  (setq tramp-backup-directory-alist backup-directory-alist))

(use-package transpose-mark
  :ensure t)

(use-package twittering-mode
  :ensure t
  :bind (:map twittering-mode-map
              ("s" . twittering-search)
              ("q" . previous-buffer)
              ("w" . delete-window))
  :init
  (defvar twittering-mode-map (make-sparse-keymap))
  (setq twittering-icon-mode t
        ;; Use master password for twitter instead of authenticating every time
        twittering-cert-file "/etc/ssl/certs/ca-bundle.crt"
        twittering-use-master-password t
        twittering-convert-fix-size 24)
  :config
  (add-hook 'twittering-mode-hook 'toggle-modeline))

(use-package uuidgen
  :ensure t)

(use-package vc-hooks
  :init
  ;; follow symlinks and don't ask
  (setq vc-follow-symlinks t)
  ;; Don't use version control for all files
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package vi-tilde-fringe
  :ensure t
  :config
  (global-vi-tilde-fringe-mode 1))

(use-package web-mode
  :ensure t
  :mode ("\\.dtl\\'"
         "\\.eex\\'"
         "\\.erb\\'"
         "\\.tpl\\'")
  :init
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4))

(use-package whitespace
  :config
  (setq whitespace-style
        '(face tabs spaces trailing
               space-before-tab indentation
               space-after-tab space-mark tab-mark)))

(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode t))

(use-package winner
  :init
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
          "*Ibuffer*"))
  :config
  (winner-mode t))

(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode)
  ;; Disable aftersave
  (defun ws-butler-after-save ()))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (setq-mode-local snippet-mode require-final-newline nil))

(use-package zoom-window
  :ensure t
  :bind* ("C-x C-1" . zoom-window-zoom))

(use-package spaceline-config
  ;; Needs to be loaded last
  :ensure spaceline
  :init
  (setq powerline-default-separator 'bar
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-anzu-off))

(use-package darktooth-theme
  :ensure t
  :config
  (set-face-attribute 'default nil :foreground "#c6a57b"))

(provide 'attic-packages)

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
  :init
  (setq alchemist-hooks-compile-on-save t
        alchemist-hooks-test-on-save t)
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (bind-key "M-N" 'mc/mark-next-like-this alchemist-mode-map)
  (bind-key "M-P" 'mc/mark-previous-like-this alchemist-mode-map)
  (bind-key "M-n" 'alchemist-goto-jump-to-next-def-symbol alchemist-mode-map)
  (bind-key "M-p" 'alchemist-goto-jump-to-previous-def-symbol alchemist-mode-map))

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode))

(use-package auto-complete
  :ensure t
  :init
  (setq ac-auto-show-menu 0.3
        ac-candidate-limit 15
        ac-delay 0.3)
  :config
  (bind-key "<return>" (lambda() (interactive) (ac-stop) (call-interactively (key-binding (kbd "C-m")))) ac-complete-mode-map)
  (bind-key "SPC" (lambda() (interactive) (ac-stop) (insert " ")) ac-complete-mode-map)
  (bind-key "C-m" (lambda() (interactive) (ac-stop) (newline)) ac-complete-mode-map)
  (bind-key ":" (lambda() (interactive) (ac-stop) (insert ":")) ac-complete-mode-map)
  (bind-key "." (lambda() (interactive) (ac-stop) (insert ".")) ac-complete-mode-map)
  (bind-key "C-p" 'ac-previous ac-complete-mode-map)
  (bind-key "M-j" 'yas/expand ac-complete-mode-map)
  (bind-key "C-n" 'ac-next ac-complete-mode-map))

(use-package beacon
  :ensure t
  :init
  (beacon-mode t)
  (add-to-list 'beacon-dont-blink-major-modes 'mu4e-compose-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'mu4e-headers-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'mu4e-main-mode))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'toml-mode-hook 'cargo-minor-mode))

(use-package clojure-mode
  :ensure t
  :config
  (bind-key "C-x C-e" 'cider-eval-last-sexp clojure-mode-map))

(use-package clj-refactor
  :ensure t
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
  (define-key clojure-mode-map (kbd "C-c C-a") 'cljr-refactor-all/body)
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package cljr-helm
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "C-c C-l") 'cljr-helm))


(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1)
  :config
  (add-hook 'alchemist-iex-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'scheme-mode-hook 'company-mode)
  (add-hook 'erlang-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'elm-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-elm)
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

(use-package company-racer
  :ensure t)

(use-package dash
  :ensure t)

(use-package delsel
  :init
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
  :config
  (define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll))

(use-package dockerfile-mode
  :ensure t)

(use-package elec-pair
  :config
  (electric-pair-mode t)
  (add-hook* 'message-mode-hook (electric-pair-mode -1))
  (add-hook* 'clojure-mode-hook (electric-pair-mode -1))
  (add-hook 'prog-mode-hook 'electric-pair-mode))

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
  (elscreen-goto-workspace-list 1 2 3 4 5 6 7 8 9))

(use-package emms
  :ensure t
  :init
  (setq emms-setup-default-player-list '(emms-player-vlc)
        emms-volume-change-amount 5)
  (when (and (file-exists-p "~/Music/")
             (>  (length (directory-files "~/Music/")) 2))
    (emms-standard)
    (emms-default-players)
    (emms-add-directory-tree "~/Music/")
    (emms-toggle-repeat-playlist)
    (emms-shuffle)
    (emms-playing-time-enable-display)))

(use-package erc
  :init
  (setq erc-scrolltobottom-mode 1
        erc-nick "kwrooijen"
        erc-prompt-for-password nil
        erc-ignore-list '("*Flowdock*" "Flowdock" "-Flowdock-")
        erc-hide-list '("JOIN" "PART" "QUIT"))
  :config
  (erc-truncate-mode 1)
  (bind-key "" 'function erc-mode-map)
  (bind-key "C-M-m" 'erc-send-current-line erc-mode-map)
  (bind-key "RET" (lambda() (interactive) (message "Use C-M-m to send")) erc-mode-map)
  (add-hook 'erc-mode-hook 'toggle-modeline))

(use-package erlang
  :ensure t
  :commands erlang-mode
  :init
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  :config
  (setq-mode-local erlang-mode tab-width 4)
  (bind-key "M-/" 'erlang-get-error erlang-mode-map)
  (bind-key "M-n" 'highlight-symbol-next erlang-mode-map)
  (bind-key "M-p" 'highlight-symbol-prev erlang-mode-map)
  (bind-key "M-n" 'highlight-symbol-next erlang-mode-map)
  (bind-key "M-p" 'highlight-symbol-prev erlang-mode-map))

(use-package evil
  :ensure t
  :config
  (add-hook 'minibuffer-setup-hook #'turn-off-evil-mode)
  (add-hook 'elixir-mode-hook #'turn-on-evil-mode)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'magit-status-mode 'emacs)
  (define-key evil-normal-state-map (kbd "C-d") 'delete-char)
  (define-key evil-normal-state-map (kbd "<SPC>") 'attic-main/body)
  (define-key evil-visual-state-map (kbd "<SPC>") 'attic-main/body)
  (define-key evil-normal-state-map (kbd "TAB") 'evil-bracket-open)
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

(use-package evil-paredit
  :ensure t
  :config
  (add-hook 'paredit-mode-hook 'evil-paredit-mode))

(use-package evil-lispy
  :ensure t
  :config
  (add-hook 'scheme-mode-hook 'evil-lispy-mode)
  (add-hook 'emacs-lisp-mode-hook 'evil-lispy-mode)
  (add-hook 'clojure-mode-hook 'evil-lispy-mode)
  (add-hook 'racket-mode-hook 'evil-lispy-mode))

(use-package eww
  :config
  (bind-key "n" (lambda() (interactive) (scroll-up 1)) eww-mode-map)
  (bind-key "p" (lambda() (interactive) (scroll-down 1)) eww-mode-map)
  (bind-key "v" 'scroll-up-command eww-mode-map))

(use-package expand-region
  :ensure t
  :init
  (bind-key* "M-@" 'er/expand-region))

(use-package fancy-battery
  :ensure t
  :init
  (fancy-battery-mode))

(use-package flycheck-rust
  :ensure t)

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
  :ensure t)

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
  :config
  (bind-key "n" 'next-line grep-mode-map)
  (bind-key "p" 'previous-line grep-mode-map)
  (bind-key "TAB" (lambda() (interactive) (error-preview "*grep*")) grep-mode-map)
  (bind-key "v" 'scroll-up-command grep-mode-map)
  (bind-key "z" 'helm-buffers-list grep-mode-map))

(use-package hackernews
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
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
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package helm
  :ensure t
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
  ;; Try to hide source header as much as possible
  (set-face-attribute 'helm-source-header nil :height 0.1 :background "#000"  :foreground "#000")

  ;; Work around for the [Display not ready] error when typing too awesomely fast
  (defun my/helm-exit-minibuffer ()
    (interactive)
    (helm-exit-minibuffer))
  (eval-after-load "helm"
    '(progn
       (define-key helm-map (kbd "<RET>") 'my/helm-exit-minibuffer)))

  (bind-key* "M-[" 'helm-resume)
  (bind-key* "M-x" 'helm-M-x)
  (define-key helm-map (kbd "C-b") 'nil)
  (define-key helm-map (kbd "C-f") 'nil)
  (define-key helm-map (kbd "M-b") 'nil)
  (define-key helm-map (kbd "M-f") 'forward-word)
  (define-key helm-map (kbd "M-s") 'helm-select-action)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "M-?") 'helm-help)
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
  (key-chord-define helm-map "jh" 'helm-like-unite/body)

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
  (setq-mode-local clojure-mode helm-dash-docsets '("Clojure"))
  (setq-mode-local elixir-mode helm-dash-docsets '("Elixir"))
  (setq-mode-local emacs-lisp-mode helm-dash-docsets '("Emacs Lisp"))
  (setq-mode-local erlang-mode helm-dash-docsets '("Erlang"))
  (setq-mode-local haskell-mode helm-dash-docsets '("Haskell"))
  (setq-mode-local ruby-mode helm-dash-docsets '("Ruby"))
  (setq-mode-local rust-mode helm-dash-docsets '("Rust"))
  (bind-key* "C-c C-s C-d" 'helm-dash))

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
  :config
  (define-key helm-swoop-map (kbd "M-e") 'helm-swoop-edit)
  :init
  (bind-key* "C-c C-s C-s" 'helm-multi-swoop)
  (bind-key* "C-c C-s C-f" 'helm-swoop-find-files-recursively))

(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-symbol
  :ensure t
  :init
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

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-exit)

(use-package jazz-theme
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package key-chord
  :ensure t
  :config
  (add-hook* 'prog-mode-hook (key-chord-mode 1))
  (add-hook* 'isearch-mode-hook (key-chord-mode 1))
  (key-chord-define-global "xs" 'evil-normal-state-and-save)
  (key-chord-define-global ";j" 'evil-force-normal-state)
  (key-chord-define helm-map ";j" 'helm-keyboard-quit)
  (key-chord-define isearch-mode-map ";j" 'isearch-abort))

(use-package linum
  :init
  (setq linum-format (quote "%3d")
        linum-disabled-modes-list
        '(mu4e-compose-mode
          mu4e-headers-mode
          mu4e-main-mode)))

(use-package lispy
  :ensure t
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
    (lispy-left 1))
  (define-key lispy-mode-map (kbd "TAB") 'lispy-left-no-mark)
  (define-key lispy-mode-map (kbd "d") 'lispy-different)
  (define-key lispy-mode-map (kbd "o") 'lispy-other-mode)
  (define-key lispy-mode-map (kbd "f") 'lispy-flow)
  (define-key lispy-mode-map (kbd "i") 'evil-insert)
  (define-key lispy-mode-map (kbd "e") 'attic/lispy--eval))

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
        mu4e-view-show-images t)
  :config
  (require 'smtpmail)
  (define-key mu4e-main-mode-map (kbd "p") 'previous-line)
  (define-key mu4e-main-mode-map (kbd "n") 'next-line)
  (define-key mu4e-main-mode-map (kbd "z") 'helm-buffers-list)
  (define-key mu4e-main-mode-map (kbd "v") 'scroll-up-command)
  (define-key mu4e-headers-mode-map (kbd "v") 'scroll-up-command)
  (define-key mu4e-view-mode-map (kbd "f") 'epa-mail-verify)
  (define-key mu4e-view-mode-map (kbd "v") 'scroll-up-command)
  (define-key mu4e-compose-mode-map (kbd "M-s") 'mml-secure-sign-pgp))

(use-package mu4e-alert
  :ensure t
  :init
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-maildirs-extension
  :ensure t
  :init
  (mu4e-maildirs-extension))

(use-package org
  :config
  (when (file-exists-p "~/Documents/notes/Org")
    (setq org-log-done 'time
          org-capture-templates '()
          org-src-fontify-natively t
          org-capture-templates '(("1" "Done" entry
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
    (add-my-todos-to-org
     (directory-files
      (expand-file-name "~/Documents/notes/Org/Todo")
      nil
      "^\\([^#|^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(use-package paredit
  :ensure t
  :init
  (setq ignore-paredit-copy '(elixir-mode erlang-mode))
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'elixir-mode-hook 'paredit-mode)
  (add-hook 'geiser-mode-hook 'paredit-mode)
  (add-hook 'racket-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region)
  (define-key paredit-mode-map (kbd "M-R") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "C-c C-r") 'paredit-reindent-defun)
  (define-key paredit-mode-map (kbd "M-j") 'paredit-join-sexps)
  (define-key paredit-mode-map (kbd "C-q") 'paredit-backward-delete)
  (define-key paredit-mode-map (kbd "M-q") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "C-c C-p") 'maybe-paredit-copy-sexp-up)
  (define-key paredit-mode-map (kbd "C-c C-n") 'maybe-paredit-copy-sexp-down)

  (defun maybe-paredit-copy-sexp-down ()
    (interactive)
    (if (member major-mode ignore-paredit-copy)
        (copy-line-down)
      (paredit-copy-sexp-down)))

  (defun maybe-paredit-copy-sexp-up ()
    (interactive)
    (if (member major-mode ignore-paredit-copy)
        (copy-line-up)
      (paredit-copy-sexp-up)))

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

(use-package paren
  :config
  (show-paren-mode t))

(use-package racer
  :ensure t
  :init
  (setq racer-cmd "/usr/local/bin/racer"
        racer-rust-src-path "/usr/local/src/rust/src/")
  (setq-mode-local rust-mode company-backends '(company-racer))
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'rust-mode-hook 'racer-turn-on-eldoc))

(use-package racket-mode
  :ensure t)

(use-package redo+
  :ensure t
  :init
  (bind-key* "M-_" 'redo))

(use-package ruby-mode
  :init
  (setq ruby-deep-indent-paren nil))

(use-package rust-mode
  :ensure t
  :bind (("M-." . racer-find-definition))
  :config
  (setq-mode-local rust-mode tab-width 4))

(use-package s
  :ensure t)

(use-package scheme-complete
  :ensure t
  :config
  (setq-mode-local scheme-mode eldoc-documentation-function 'scheme-get-current-symbol-info)
  (autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
  (add-hook 'scheme-mode-hook 'eldoc-mode))

(use-package string-edit
  :ensure t)

(use-package term
  :config
  (setq-mode-local term-mode yas-dont-activate t))

(use-package tiny
  :ensure t
  :config
  (bind-key "C-;" 'tiny-expand))

(use-package toml-mode
  :ensure t)

(use-package transpose-mark
  :ensure t)

(use-package twittering-mode
  :ensure t
  :init
  (setq twittering-icon-mode t
        ;; Use master password for twitter instead of authenticating every time
        twittering-cert-file "/etc/ssl/certs/ca-bundle.crt"
        twittering-use-master-password t
        twittering-convert-fix-size 24)
  :config
  (bind-key "s" 'twittering-search twittering-mode-map)
  (bind-key "q" (lambda () (interactive) (switch-to-buffer nil)) twittering-mode-map)
  (bind-key "w" 'delete-window twittering-mode-map)
  (add-hook 'twittering-mode-hook 'toggle-modeline))

(use-package uuidgen
  :ensure t)

(use-package vi-tilde-fringe
  :ensure t
  :init
  (global-vi-tilde-fringe-mode 1))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4))

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
  :ensure t)

(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :config
  ;; Disable aftersave
  (defun ws-butler-after-save ()))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode t)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (setq-mode-local snippet-mode require-final-newline nil))

(use-package simple
  :config
  (setq-mode-local fundamental-mode require-final-newline nil)
  ;; Kill buffer on remote machine
  (defadvice async-shell-command (before attic-ad/async-shell-command activate)
    (when (get-buffer "*Async Shell Command*")
      (kill-buffer "*Async Shell Command*"))))

(use-package wrap-region
  :config
  (wrap-region-global-mode t))

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

(provide 'attic-packages)

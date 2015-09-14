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

(use-package alchemist
  :ensure t)

(use-package auto-complete
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package clojure-snippets
  :ensure t)

(use-package cider
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package company
  :ensure t)

(use-package dash
  :ensure t)

(use-package dired
  :init
  (require 'dired)
  (bind-key "c f" 'helm-ls-git-ls dired-mode-map)
  (bind-key "z" 'helm-buffers-list dired-mode-map)
  (bind-key "c m" 'magit-status dired-mode-map)
  (bind-key ";" 'semi-colon-map dired-mode-map)
  (bind-key "c z" 'attic-make-map dired-mode-map))

(use-package elixir-mode
  :ensure t)

(use-package elixir-yasnippets
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package erlang
  :ensure t)

(use-package escreen
  :ensure t)

(use-package evil
  :init
  (evil-mode t)
  :ensure t)

(use-package evil-leader
      :commands (evil-leader-mode)
      :ensure evil-leader
      :demand evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
        (undo-tree-mode -1)
        (evil-leader/set-leader "<SPC>")
        (evil-leader/set-key
          "g" 'magit-status
          "r" 'attic/grep
          "n" 'sauron-select-last-event
          "p" 'escreen-goto-prev-screen
          "x" 'helm-M-x
          "<SPC>" 'escreen-goto-last-screen
          "d" (lambda() (interactive) (helm-swoop :$query ""))
          "M-d" 'helm-swoop
          "a" 'async-shell-command
          "s" 'shell-command
          "f" 'helm-ls-git-ls
          "e" 'eww
          "b" 'helm-bookmarks
          "[" 'winner-undo
          "]" 'winner-redo
          "1" 'escreen-goto-screen-1
          "2" 'escreen-goto-screen-2
          "3" 'escreen-goto-screen-3
          "4" 'escreen-goto-screen-4
          "5" 'escreen-goto-screen-5
          "6" 'escreen-goto-screen-6
          "7" 'escreen-goto-screen-7
          "8" 'escreen-goto-screen-8
          "9" 'escreen-goto-screen-9
          "0" 'xsescreen-goto-screen-0
          "'" 'helm-org-capture-templates
          "qt" (lambda() (interactive) (run-make "test"    "[Make Test]"))
          "qp" (lambda() (interactive) (run-make "stop"    "[Make Stop]"))
          "qr" (lambda() (interactive) (run-make "restart" "[Make Restart]"))
          "qs" (lambda() (interactive) (run-make "start"   "[Make Start]"))
          "qo" (lambda() (interactive) (run-make "go"      "[Make Go]"))
          "qq" (lambda() (interactive) (run-make ""        "[Make]"))
          "qc" 'run-make-input)))

(use-package eww
  :config
  (progn
    (bind-key "n" (lambda() (interactive) (scroll-up 1)) eww-mode-map)
    (bind-key "p" (lambda() (interactive) (scroll-down 1)) eww-mode-map)
    (bind-key "v" 'scroll-up-command eww-mode-map)))

(use-package expand-region
  :init
  (bind-key "M-@" 'er/expand-region attic-mode-map)
  :ensure t)

(use-package git-gutter+
  :ensure t
  :init
  (require 'git-gutter+)
  :config
  (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :ensure t
  :init
  (require 'git-gutter-fringe+)
  :config
  (global-git-gutter+-mode t))

(use-package grep
  :init
  (require 'grep)
  (bind-key "n" 'next-line grep-mode-map)
  (bind-key "p" 'previous-line grep-mode-map)
  (bind-key "TAB" (lambda() (interactive) (error-preview "*grep*")) grep-mode-map)
  (bind-key "v" 'scroll-up-command grep-mode-map)
  (bind-key ";" 'semi-colon-map grep-mode-map)
  (bind-key ";" 'semi-colon-map grep-mode-map)
  (bind-key "z" 'helm-buffers-list grep-mode-map))

(use-package hackernews
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package helm
  :init
  (bind-key "M-[" 'helm-resume attic-mode-map)
  (bind-key "M-x" 'helm-M-x attic-mode-map)
  :ensure t)

(use-package helm-dash
  :init
  (bind-key "C-c C-s C-d" 'helm-dash attic-mode-map)
  :ensure t)

(use-package helm-descbinds
  :ensure t)

(use-package helm-ls-git
  :ensure t)

(use-package helm-swoop
  :init
  (bind-key "C-c C-s C-s" 'helm-multi-swoop attic-mode-map)
  (bind-key "C-c C-s C-f" 'helm-swoop-find-files-recursively attic-mode-map)
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :config
  ;; Highlight delay for multiple occurences
  (setq highlight-symbol-idle-delay 0))

(use-package hydra
  :ensure t)

(use-package indy
  :ensure t)

(use-package key-chord
  :ensure t
  :init
  (key-chord-define-global "xs" '(lambda ()
                                   (interactive)
                                   (evil-force-normal-state)
                                   (save-buffer)))
  (key-chord-define-global ";j" 'attic-lock)
  (key-chord-define attic-mode-map ";j" 'attic-lock)
  (key-chord-define isearch-mode-map ";j" 'isearch-abort)
  :config
  (key-chord-mode t))

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
  :init
  (require 'magit)
  (bind-key "RET" (lambda () (interactive) (magit-visit-item t)) magit-status-mode-map)
  (bind-key "g" 'magit-refresh magit-status-mode-map)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package multiple-cursors
  :ensure t
  :init
  (bind-key "M-N" 'mc/mark-next-like-this attic-mode-map)
  (bind-key "M-P" 'mc/mark-previous-like-this attic-mode-map)
  (bind-key "<return>" 'newline mc/keymap)
  :config
  (multiple-cursors-mode t))

(use-package neotree
  :ensure t)

(use-package paredit
  :ensure t)

(use-package pcmpl-args
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package redo+
  :ensure t
  :init
  (bind-key "M-_" 'redo attic-mode-map))

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
  (setq sauron-max-line-height 4))

(use-package transpose-mark
  :init
  (bind-key "C-c C-t" 'transpose-mark attic-mode-map)
  :ensure t)

(use-package twittering-mode
  :ensure t)

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

;; Modes
(display-battery-mode t)
(show-paren-mode t)
(wrap-region-global-mode t)
(electric-pair-mode t)

(provide 'attic-packages)

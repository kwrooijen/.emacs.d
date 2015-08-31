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
  :init
  (bind-key "M-q" 'ace-jump-mode attic-mode-map)
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package auto-complete
  :ensure t)

(use-package buffer-move
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
  (bind-key "c s a" 'helm-bookmarks dired-mode-map)
  (bind-key "c s r" 'my/grep dired-mode-map)
  (bind-key "c m" 'magit-status dired-mode-map)
  (bind-key ";" 'semi-colon-map dired-mode-map)
  (bind-key "c z" 'attic-make-map dired-mode-map))

(use-package elixir-mode
  :ensure t)

(use-package elixir-yasnippets
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package erc-image
  :ensure t)

(use-package erlang
  :ensure t)

(use-package escreen
  :ensure t)

(use-package eww
  :init
  (bind-key "n" (lambda() (interactive) (scroll-up 1)) eww-mode-map)
  (bind-key "p" (lambda() (interactive) (scroll-down 1)) eww-mode-map)
  (bind-key "v" 'scroll-up-command eww-mode-map))

(use-package expand-region
  :init
  (bind-key "M-@" 'er/expand-region attic-mode-map)
  :ensure t)

(use-package git-gutter+
  :ensure t
  :config
  (global-git-gutter+-mode t)
  (setq git-gutter+-unchanged-sign " "))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (global-git-gutter+-mode t)
  (setq git-gutter+-unchanged-sign " "))

(use-package god-mode
  :ensure t
  :init
  (require 'god-mode)
  ;; (bind-key "g" 'goto-line god-local-mode-map)
  (bind-key "i" 'god-mode-disable god-local-mode-map)
  (bind-key ";" 'semi-colon-map god-local-mode-map)
  (bind-key "/" 'my-comment god-local-mode-map)
  (god-mode)
  :config
  (add-to-list 'god-exempt-major-modes 'gnus-summary-mode)
  (add-to-list 'god-exempt-major-modes 'gnus-group-mode)
  (add-to-list 'god-exempt-major-modes 'term-mode)
  (add-to-list 'god-exempt-major-modes 'help-mode)
  (add-to-list 'god-exempt-major-modes 'grep-mode)
  (add-to-list 'god-exempt-major-modes 'doc-view-mode)
  (add-to-list 'god-exempt-major-modes 'top-mode)
  (add-to-list 'god-exempt-major-modes 'dired-mode)
  (add-to-list 'god-exempt-major-modes 'twittering-mode))

(use-package grep
  :init
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
  (bind-key "C-c C-s C-a" 'helm-bookmarks attic-mode-map)
  (bind-key "M-x" 'helm-M-x attic-mode-map)
  :ensure t)

(use-package helm-dash
  :init
  (bind-key "C-c C-s C-d" 'helm-dash attic-mode-map)
  :ensure t)

(use-package helm-descbinds
  :ensure t)

(use-package helm-ls-git
  :init
  (bind-key "C-c C-f" 'helm-ls-git-ls attic-mode-map)
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

(use-package iy-go-to-char
  :init
  (bind-key "C-q" 'iy-go-up-to-char attic-mode-map)
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package key-chord
  :ensure t
  :init
  (key-chord-define-global "xs" 'god-enable-and-save)
  (key-chord-define-global ";j" 'escape-key)
  (key-chord-define attic-mode-map ";j" 'escape-key)
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
  (bind-key "C-c C-m" 'magit-status attic-mode-map)
  (bind-key "RET" (lambda () (interactive) (magit-visit-item t)) magit-status-mode-map)
  (bind-key "g" 'magit-refresh magit-status-mode-map)
  (bind-key ";" 'semi-colon-map magit-status-mode-map)
  (bind-key ";" 'semi-colon-map magit-diff-mode-map)
  (bind-key ";" 'semi-colon-map magit-commit-mode-map)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package magit-gh-pulls
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :init
  (bind-key "M-N" 'mc/mark-next-like-this attic-mode-map)
  (bind-key "M-P" 'mc/mark-previous-like-this attic-mode-map)
  (bind-key "<return>" 'newline mc/keymap)
  :config
  (multiple-cursors-mode t)
  ;; Make mc work better with iy-go-to-char
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))

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

(use-package top-mode
  :ensure t
  :init
  (bind-key ";" 'semi-colon-map top-mode-map)
  (bind-key "z" 'helm-buffers-list top-mode-map))

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

(require 'gnus-notify)
(require 'sticky-windows)

;; Modes
(display-battery-mode t)
(show-paren-mode t)
(wrap-region-global-mode t)
(electric-pair-mode t)

(provide 'attic-packages)

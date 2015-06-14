(require 'lispy)
;;==============================================================================
;;== Keys
;;==============================================================================

;; (define-key emacs-lisp-mode-map (kbd "M-p") 'highlight-symbol-prev)
;; (define-key emacs-lisp-mode-map (kbd "M-n") 'highlight-symbol-next)
(define-key emacs-lisp-mode-map (kbd "C-q") 'special-lispy-ace-paren)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'litable-accept-as-pure)

;; (define-key lispy-mode-map (kbd "M-m") 'back-to-indentation)

;; (define-key lispy-mode-map (kbd "a") 'special-lispy-left)
;; (define-key lispy-mode-map (kbd "h") 'special-lispy-ace-symbol)

;; (define-key lispy-mode-map (kbd "e") 'special-lispy-right)
;; (define-key lispy-mode-map (kbd "l") 'special-lispy-eval)

;; (define-key lispy-mode-map (kbd "n") 'special-lispy-down)
;; (define-key lispy-mode-map (kbd "j") 'special-lispy-new-copy)

;; (define-key lispy-mode-map (kbd "p") 'special-lispy-up)
;; (define-key lispy-mode-map (kbd "k") 'special-lispy-eval-other-window)

(define-key lispy-mode-map (kbd "M-m") 'back-to-indentation)

(define-key lispy-mode-map (kbd "M-C-\\")
  (lambda ()
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (find-file "~/Documents/lispy-keys")
    (other-window 1)))

(define-key lispy-mode-map-x (kbd "1") 'delete-other-windows)
(define-key lispy-mode-map-x (kbd "2") 'split-window-below)
(define-key lispy-mode-map-x (kbd "3") 'split-window-right)
(define-key lispy-mode-map-x (kbd "0") 'delete-window)

;;==============================================================================
;;== Options
;;==============================================================================

;;==============================================================================
;;== Hooks
;;==============================================================================

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local emacs-lisp-mode t)
            (paredit-mode 1)
            (electric-pair-mode 0)
            (default-language-settings)
            (litable-mode)
            (setq-local helm-dash-docsets '("Emacs Lisp"))))

(provide 'attic-emacs-lisp)

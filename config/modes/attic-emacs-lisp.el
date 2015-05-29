;;==============================================================================
;;== Keys
;;==============================================================================

(define-key emacs-lisp-mode-map (kbd "M-p") 'highlight-symbol-prev)
(define-key emacs-lisp-mode-map (kbd "M-n") 'highlight-symbol-next)

;;==============================================================================
;;== Options
;;==============================================================================

;;==============================================================================
;;== Hooks
;;==============================================================================

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode)
            (electric-pair-mode 0)
            (default-language-settings)
            (setq-local helm-dash-docsets '("Emacs Lisp"))))

(provide 'attic-emacs-lisp)

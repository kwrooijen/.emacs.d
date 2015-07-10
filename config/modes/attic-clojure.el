;;==============================================================================
;;== Keys
;;==============================================================================

(define-key clojure-mode-map (kbd "C-x C-e") 'cider-eval-last-sexp)

;;==============================================================================
;;== Options
;;==============================================================================

;;==============================================================================
;;== Hooks
;;==============================================================================

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (default-language-settings)
            (electric-pair-mode 0)
            (setq-local helm-dash-docsets '("Clojure"))))

(provide 'attic-emacs-lisp)

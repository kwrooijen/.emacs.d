(require 'paredit)

(define-key paredit-mode-map (kbd "M-R") 'paredit-splice-sexp-killing-backward)

(provide 'attic-paredit)

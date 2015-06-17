(require 'paredit)

(define-key paredit-mode-map (kbd "M-R") 'paredit-splice-sexp-killing-backward)

(define-key paredit-mode-map (kbd ";")
  (lambda () (interactive)
    (if god-global-mode
        (let ((key (read-key-sequence ";-")))
          (call-interactively
           (key-binding (kbd (format "C-; C-%s" key)))))
      (paredit-semicolon))))

(define-key paredit-mode-map (kbd ")")
  (lambda () (interactive)
    (if god-global-mode
        (call-interactively (key-binding (kbd "C-)")))
      (paredit-close-round))))

(define-key paredit-mode-map (kbd "(")
  (lambda () (interactive)
    (if (or (not god-global-mode) (region-active-p))
        (paredit-open-round)
      (call-interactively
       (key-binding (kbd "C-("))))))

(provide 'attic-paredit)

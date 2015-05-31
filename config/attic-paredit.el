(require 'paredit)
(require 'lispy)

(define-key paredit-mode-map (kbd "M-R") 'paredit-splice-sexp-killing-backward)
(define-key paredit-mode-map (kbd "C-j") (lambda() (interactive) (join-line -1)))
(define-key paredit-mode-map (kbd "C-q") 'iy-go-up-to-char)
(define-key paredit-mode-map (kbd "C-c C-n") 'lispy-clone)
(define-key paredit-mode-map (kbd "M-q") 'lispy-ace-paren)
(define-key paredit-mode-map (kbd "M-n") 'lispy-move-down)
(define-key paredit-mode-map (kbd "M-p") 'lispy-move-up)


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
        (call-interactively
         (key-binding (kbd "C-)")))
      (paredit-close-round))))

(define-key paredit-mode-map (kbd "(")
  (lambda () (interactive)
    (if (or (not god-global-mode) (region-active-p))
        (paredit-open-round)
      (call-interactively
       (key-binding (kbd "C-("))))))

(provide 'attic-paredit)

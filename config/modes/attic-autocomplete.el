;;==============================================================================
;;== Keys
;;==============================================================================

;; Auto Complete Mode
(define-key ac-complete-mode-map (kbd "M-g")
    (lambda() (interactive) (ac-stop) (escape-key)))
(define-key ac-complete-mode-map (kbd "M-f") 'ac-complete)
(define-key ac-complete-mode-map (kbd "<return>")
    (lambda() (interactive) (ac-stop) (call-interactively (key-binding (kbd "C-m")))))
(define-key ac-complete-mode-map (kbd "SPC")
    (lambda() (interactive) (ac-stop) (insert " ")))
(define-key ac-complete-mode-map (kbd "C-m")
    (lambda() (interactive) (ac-stop) (newline)))
(define-key ac-complete-mode-map (kbd ":")
    (lambda() (interactive) (ac-stop) (insert ":")))
(define-key ac-complete-mode-map (kbd ".")
  (lambda() (interactive) (ac-stop) (insert ".")))
(define-key ac-complete-mode-map (kbd "M-j") 'yas/expand)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)

;; Company Mode
(define-key company-active-map (kbd "M-g")
    (lambda() (interactive) (company-abort) (escape-key)))
(define-key company-active-map (kbd "M-f") 'company-complete-selection)
(define-key company-active-map (kbd "<return>")
    (lambda() (interactive) (company-abort) (newline)))
(define-key company-active-map (kbd "SPC")
    (lambda() (interactive) (company-abort) (insert " ")))
(define-key company-active-map (kbd "C-m")
    (lambda() (interactive) (company-abort) (newline)))
(define-key company-active-map (kbd ":")
    (lambda() (interactive) (company-abort) (insert ":")))
(define-key company-active-map (kbd ".")
  (lambda() (interactive) (company-abort) (insert ".")))
(define-key company-active-map (kbd "M-h") 'helm-company)
(define-key company-active-map (kbd "M-j") 'yas/expand)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;;==============================================================================
;;== Options
;;==============================================================================

;; Auto Complete Mode
(setq ac-auto-show-menu 0.3)
(setq ac-candidate-limit 15)
(setq ac-delay 0.3)

;; Company Mode
(setq company-idle-delay 0.3)

;;==============================================================================
;;== Hooks
;;==============================================================================

(provide 'attic-autocomplete)

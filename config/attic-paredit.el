(require 'paredit)

;; (defun set-paredit (var)
;;   `(define-key paredit-mode-map (kbd (car ,var))
;;      (lambda () (interactive)
;;        (if god-global-mode
;;            (call-interactively
;;             (key-binding (kbd (concat "C-" (car ,var))))
;;          (last ,var))))))

;; (defmacro paredit-macro (&rest vars)
;;   (let ((forms (mapcar 'set-paredit vars)))
;;     `(progn ,@forms)))

;; (eval (macroexpand
;;        '(paredit-macro
;;          '(")" 'paredit-close-round) '("(" 'paredit-open-round))))


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

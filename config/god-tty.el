(defun call-meta-by-char (char)
  (call-interactively (key-binding (kbd (format "M-%s" (string char))))))


(defun call-control-by-char (char)
  (call-interactively (key-binding (kbd (format "%s" (string char))))))

(defun god-g (_ char)
  (interactive "p\nc ")
  (call-meta-by-char char)
  (setq god-repeat-char char)
  (call-interactively 'god-g-repeat)
)
(defun god-g-repeat (_ char)
  (interactive "p\nc ")
  (if (equal char god-repeat-char)
      (progn
        (call-meta-by-char char)
        (call-interactively 'god-g-repeat))
      (if god-local-mode
          (call-control-by-char char)
          (insert char))
      ))

(define-key god-local-mode-map (kbd "g") 'god-g)

(provide 'god-tty)

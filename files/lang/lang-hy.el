(use-package hy-mode
  :straight t
  :config
  (define-key hy-mode-map (kbd "C-x C-e") 'hy-shell-eval-last-sexp-stay)
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'flycheck-mode)
  (mode-leader-def
    'visual hy-mode-map
    "er" 'hy-shell-eval-region-stay)
  (mode-leader-def
    'normal hy-mode-map
    "eb" 'hy-shell-eval-buffer-stay
    "bb" 'run-hy
    "'" 'run-hy

    "rt" '(:ignore t :which-key "Thread")
    "rtf"  #'clojure-thread-first-all
    "rth"  #'clojure-thread
    "rtl"  #'clojure-thread-last-all))

(defun hy-shell-eval-buffer-stay ()
  "Eval Hy buffer but don't jump to REPL."
  (interactive)
  (save-window-excursion
    (hy-shell-eval-buffer)))

(defun hy-shell-eval-region-stay ()
  "Eval Hy buffer but don't jump to REPL."
  (interactive)
  (save-window-excursion
    (hy-shell-eval-region)))

(defun hy-shell-eval-last-sexp-stay ()
  "Eval last Hy expression but don't jump to REPL."
  (interactive)
  (save-window-excursion
    (hy-shell-eval-last-sexp)))

(provide 'lang-hy)

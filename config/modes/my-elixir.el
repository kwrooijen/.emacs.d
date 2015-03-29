;===============================================================================
;=== Keys
;===============================================================================

(defun elixir-keys-hook ()
  (setq-local doom-indent-key "") ;;; HACK FOR ELIXIR MODE
  (define-key elixir-mode-map (kbd "TAB") (lambda() (interactive)
    (indent-of-doom))) ;;; HACK FOR ELIXIR MODE
  (define-key elixir-mode-map (kbd "C-c C-l") 'iex-compile)
  (define-key elixir-mode-map (kbd "C-c C-c C-e")
      (lambda(x) (interactive "sRun Mix > ") (run-mix x)))

  (define-key elixir-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key elixir-mode-map (kbd "M-p") 'highlight-symbol-prev)
  (define-key elixir-mode-map (kbd "C-c C-c C-v")
      (lambda() (interactive) (run-mix "compile")))
  (define-key elixir-mode-map (kbd "C-c C-c C-s")
      (lambda() (interactive) (run-mix "start")))
  (define-key elixir-mode-map (kbd "C-c C-c C-c")
      (lambda() (interactive) (run-mix "coveralls")))
  (define-key elixir-mode-map (kbd "C-c C-c C-d")
      (lambda() (interactive) (run-mix "coveralls.detail")))
  (define-key elixir-mode-map (kbd "C-c C-c C-l")
      (lambda() (interactive) (run-mix "help")))
  (define-key elixir-mode-map (kbd "C-c C-c C-i")
      (lambda() (interactive) (run-mix "dialyzer"))))

;===============================================================================
;=== Hook
;===============================================================================

(add-hook 'elixir-mode-hook (lambda ()
    (key-chord-force)
    (elixir-keys-hook)
    (fix-tabs 2)
    (setq tab-stop-list tab-stop-list-2)
    (setq-local doom-indent-fallback t)
    (setq-local doom-use-tab-cycle nil)
    (setq-local helm-dash-docsets '("Elixir"))))

;===============================================================================
;=== Functions
;===============================================================================

(defun iex-compile ()
    (interactive)
    (let ((current (buffer-name)))
        (elixir-mode-iex)
        (kill-line 0)
        (insert (format "c(\"%s\")" current))
        (comint-send-input)))

(provide 'my-elixir)

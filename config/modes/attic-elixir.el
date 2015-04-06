;;==============================================================================
;;== Keys
;;==============================================================================

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
    (lambda() (interactive) (run-mix "dialyzer")))
  ;; Alchemist uses non Control keys in its key combinations, e.g. C-c a h h.
  ;; This is fine but when using God mode, it's a pain. So let's
  ;; redefine them with Control keys.
  (define-key elixir-mode-map (kbd "C-c C-a C-c C-b")     'alchemist-compile-this-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-c C-c")     'alchemist-compile) 
  (define-key elixir-mode-map (kbd "C-c C-a C-c C-f")     'alchemist-compile-file) 

  (define-key elixir-mode-map (kbd "C-c C-a C-e C-b")     'alchemist-execute-this-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-e C-e")     'alchemist-execute) 
  (define-key elixir-mode-map (kbd "C-c C-a C-e C-f")     'alchemist-execute-file) 

  (define-key elixir-mode-map (kbd "C-c C-a C-h C-e")     'alchemist-help-search-at-point) 
  (define-key elixir-mode-map (kbd "C-c C-a C-h C-h")     'alchemist-help) 
  (define-key elixir-mode-map (kbd "C-c C-a C-h C-m")     'alchemist-help-search-marked-region) 

  (define-key elixir-mode-map (kbd "C-c C-a C-i C-b")     'alchemist-iex-compile-this-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-i C-c")     'alchemist-iex-send-current-line-and-go) 
  (define-key elixir-mode-map (kbd "C-c C-a C-i C-i")     'alchemist-iex-run) 
  (define-key elixir-mode-map (kbd "C-c C-a C-i C-l")     'alchemist-iex-send-current-line) 
  (define-key elixir-mode-map (kbd "C-c C-a C-i C-m")     'alchemist-iex-send-region-and-go) 
  (define-key elixir-mode-map (kbd "C-c C-a C-i C-p")     'alchemist-iex-project-run) 
  (define-key elixir-mode-map (kbd "C-c C-a C-i C-r")     'alchemist-iex-send-region) 

  (define-key elixir-mode-map (kbd "C-c C-a C-m C-t C-.") 'alchemist-mix-test-at-point) 
  (define-key elixir-mode-map (kbd "C-c C-a C-m C-t C-b") 'alchemist-mix-test-this-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-m C-t C-f") 'alchemist-mix-test-file) 

  (define-key elixir-mode-map (kbd "C-c C-a C-p C-f")     'alchemist-project-find-test) 
  (define-key elixir-mode-map (kbd "C-c C-a C-p C-t")     'alchemist-project-open-tests-for-current-file)

  (define-key elixir-mode-map (kbd "C-c C-a C-t")         'alchemist-mix-test) 

  (define-key elixir-mode-map (kbd "C-c C-a C-v C-e")     'alchemist-eval-quoted-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-h")     'alchemist-eval-print-quoted-current-line) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-i")     'alchemist-eval-print-region) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-j")     'alchemist-eval-quoted-current-line) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-k")     'alchemist-eval-print-current-line) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-l")     'alchemist-eval-current-line) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-o")     'alchemist-eval-region) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-q")     'alchemist-eval-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-r")     'alchemist-eval-print-quoted-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-u")     'alchemist-eval-quoted-region) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-w")     'alchemist-eval-print-buffer) 
  (define-key elixir-mode-map (kbd "C-c C-a C-v C-y")     'alchemist-eval-print-quoted-region))

;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'elixir-mode-hook (lambda ()
    (key-chord-force)
    (elixir-keys-hook)
    (fix-tabs 2)
    (if window-system (nlinum-mode))
    (if window-system (hl-line-mode 1))
    (setq tab-stop-list tab-stop-list-2)
    (setq-local doom-indent-fallback t)
    (setq-local doom-use-tab-cycle nil)
    (highlight-symbol-mode 1)
    (company-mode)
    (alchemist-mode)
    (setq-local helm-dash-docsets '("Elixir"))))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun iex-compile ()
    (interactive)
    (let ((current (buffer-name)))
        (elixir-mode-iex)
        (kill-line 0)
        (insert (format "c(\"%s\")" current))
        (comint-send-input)))

(provide 'attic-elixir)

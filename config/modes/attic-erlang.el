;;==============================================================================
;;== Keys
;;==============================================================================

(define-key erlang-mode-map (kbd "M-/") 'erlang-get-error)
(define-key erlang-mode-map (kbd "M-q") 'backward-kill-word)
(define-key erlang-mode-map (kbd "C-c C-k")
  (lambda() (interactive)
    (rebar-compile)
    (inferior-erlang)
    (split-window)
    (other-window 1)
    (switch-to-buffer "*rebar-compilation*")
    (other-window -1)))
(define-key erlang-mode-map (kbd "M-n") 'highlight-symbol-next)
(define-key erlang-mode-map (kbd "M-p") 'highlight-symbol-prev)
(define-key erlang-mode-map (kbd "M-n") 'highlight-symbol-next)
(define-key erlang-mode-map (kbd "M-p") 'highlight-symbol-prev)
(define-key erlang-mode-map (kbd ">")   (lambda() (interactive) (insert ">")))

;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'erlang-mode-hook
          (lambda ()
            (if (not (is-tramp-mode))
                (progn
                  (flymake-erlang-init)
                  (flymake-mode 1)))
            (default-language-settings)
            (setq inferior-erlang-machine-options '("-sname" "emacs"))
            (fix-tabs 4)
            (rebar-mode 1)
            (setq-local helm-dash-docsets '("Erlang"))
            (subword-mode t)))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-intemp))
         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/scripts/erlang/erlang-flymake" (list local-file))))

(defun erlang-get-error ()
    (interactive)
    (async-shell-command
     (format "~/.emacs.d/scripts/erlang/erlang-flymake %s" buffer-file-name) "[Erlang Errors]"))


;;==============================================================================
;;== Advice
;;==============================================================================

(defadvice inferior-erlang (before inferior-erlang activate)
    (setq-local deps (remove-if-not 'identity
        (mapcar (lambda(x)
            (unless (or (equal x ".") (equal x ".."))
                (concat "../deps/" x "/ebin")))
             (directory-files "../deps"))))
    (setq-local core '("../ebin"))

    (setq inferior-erlang-machine-options
        (-flatten (mapcar (lambda(x)
            (setq-local ll '())
            (add-to-list 'll x)
            (add-to-list 'll "-pa")
            (add-to-list 'll "include")
            (add-to-list 'll "-I")
            ll) (append core deps)))))

(defadvice erlang-compile (before erlang-compile activate)
    (setq-local deps (remove-if-not 'identity
        (mapcar (lambda(x)
            (unless (or (equal x ".") (equal x ".."))
                (concat "../deps/" x "/ebin")))
             (directory-files "../deps"))))
    (setq-local core '("../ebin"))
    (setq inferior-erlang-machine-options
        (-flatten (mapcar (lambda(x)
            (setq-local ll '())
            (add-to-list 'll x)
            (add-to-list 'll "-pa")
            (add-to-list 'll "include")
            (add-to-list 'll "-I")
            ll) (append core deps)))))
    ;; (setq inferior-erlang-machine-options '("-pa" "../ebin/" "-pa" "../deps/ranch/ebin/"))

(provide 'attic-erlang)

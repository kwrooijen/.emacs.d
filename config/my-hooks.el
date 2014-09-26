;; Hooks
(defun clean-hook ()
  (interactive)
  (god-local-mode 0)
  (key-chord-mode 0)
  (linum-mode 0))

(defun key-chord-force ()
  (key-chord-mode 1)
  (message nil))

(defadvice ansi-term (after advice-term-line-mode activate)
  (clean-hook))

(defun fix-tabs (x)
  (indent-of-doom-mode t)
  (setq-local tab-width x)
  (god-local-mode t))

(defun my-before-save()
    (delete-trailing-whitespace)
    (let ((current (point)))
        (goto-char (point-max))
        (newline)
        (goto-char current)))

(add-hook 'before-save-hook 'my-before-save)
(add-hook 'c-initialization-hook 'c-keys-hook)
(add-hook 'dired-mode-hook 'ensure-buffer-name-begins-with-exl)
(add-hook 'magit-mode-hook 'clean-hook)
(add-hook 'shell-mode-hook 'god-local-mode)
(add-hook 'doc-view-mode-hook 'clean-hook)
(add-hook 'w3m-mode-hook 'clean-hook)


(add-hook 'isearch-mode-hook (lambda()
    (key-chord-mode 1)
))

(add-hook 'erlang-mode-hook (lambda ()
    (if (not (is-tramp-mode)) (progn
        (flymake-erlang-init)
        (flymake-mode 1)))
    (rainbow-delimiters-mode)
    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    (highlight-symbol-mode t)
    (key-chord-force)
    (fix-tabs 4)
    (setq-local doom-indent-fallback t)
    (setq-local doom-use-tab-cycle nil)
    (rebar-mode 1)
    (subword-mode t)
    (setq-local helm-dash-docsets '("Erlang"))
    (distel-setup)
    (erlang-extended-mode)
    (subword-mode t)
    (erlang-keys-hook)
))

(add-hook 'elixir-mode-hook (lambda ()
    (key-chord-force)
    (elixir-keys-hook)
    (global-elixir-mix-mode 1)
    (fix-tabs 2)
    (setq-local doom-indent-fallback t)
    (setq-local doom-use-tab-cycle nil)
    (setq-local helm-dash-docsets '("Elixir"))
))

(add-hook 'ruby-mode-hook (lambda ()
    (god-local-mode t)
    (rainbow-delimiters-mode)
    (setq-local helm-dash-docsets '("Ruby"))
))

(add-hook 'haskell-mode-hook (lambda ()
    (fix-tabs 4)
    (rainbow-delimiters-mode)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indentation)
    (setq-local doom-indent-fallback t)
    (setq-local helm-dash-docsets '("Haskell"))
))

(add-hook 'emacs-lisp-mode-hook (lambda ()
    (fix-tabs 4)
    (rainbow-delimiters-mode)
    (setq-local helm-dash-docsets '("Emacs Lisp"))
))

(add-hook 'rust-mode-hook (lambda ()
    (if (not (is-tramp-mode)) (progn
        (flymake-rust-load)))
    (rainbow-delimiters-mode)
    (setq-local tab-width 4)
    (rust-keys-hook)
    (setq-local helm-dash-docsets '("Rust"))
))

;; Doom Indent Config
(setq doom-use-tab-cycle t)
(setq doom-region-cycle nil)

(setq my-doom '(
    (all . (
        ((and (prev 'ends-on "[") (current 'starts-with "]")) (prev 'indent))
        ((and (prev 'ends-on "{") (current 'starts-with "}")) (prev 'indent))
        ((and (prev 'ends-on "(") (current 'starts-with ")")) (prev 'indent))

        ((current 'starts-with "]" "}" ")") (prev 'indent -1))
        ((prev 'ends-on "[" "{" "(")        (prev 'indent 1))
        ((prev 'ends-on ",")        (prev 'indent))
    ))
    (erlang-mode . (
        ((prev 'ends-on "->" "fun" "of") (prev 'indent 1))
        ((prev 'ends-on ";") (prev 'indent -1))
        ((current 'ends-on "end") (prev 'indent -1))
    ))
    (haskell-mode . (
        ((prev 'indent-char-is ",") (prev 'indent))
        ((prev 'indent-char-is "[") (prev 'indent))
        ((prev 'ends-on "=" "= do" "=do") (prev 'indent 1))
    ))
    (elixir-mode . (
        ((and (prev 'ends-on ") ->") (current 'starts-with "end")) (prev 'indent))
        ((prev 'ends-on ") ->") (prev 'indent 1))
    ))
))

;; Flymake Hook
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-intemp))
	 (local-file (file-relative-name temp-file
		(file-name-directory buffer-file-name))))
    (list "~/.emacs.d/scripts/erlang/erlang-flymake" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

;; Load mode on certain file extensions
(setq auto-mode-alist (append '(
    ("\\.less\\'"    . css-mode)
    ("\\.scss\\'"    . css-mode)
    ("Gemfile$"      . ruby-mode)
    ("Rakefile$"     . ruby-mode)
    ("\\.gemspec$"   . ruby-mode)
    ("\\.rake$"      . ruby-mode)
    ("\\.rb$"        . ruby-mode)
    ("\\.ru$"        . ruby-mode)
    ("\\.app.src\\'" . erlang-mode)
    ("rebar.conf"    . erlang-mode)
    ("\\.elm\\'"     . haskell-mode)
    ("\\.js\\'"      . js2-mode)
    ("\\.dtl\\'"     . web-mode)
    ("\\.eex\\'"     . web-mode)
    ("\\.erb\\'"     . web-mode)
    ("\\.tpl\\'"     . web-mode)
    ) auto-mode-alist))


(provide 'my-hooks)


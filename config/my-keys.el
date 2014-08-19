(defvar attic-minor-mode-map (make-keymap) "attic-minor-mode keymap.")

(defun attic-key(key function)
  (define-key attic-minor-mode-map (kbd key) function)
  (global-set-key (kbd key) function))

(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a))) '(

("<escape>" escape-key)

;; Control Keys
("C--" undo)
("C-." helm-resume)
("C-/" my-comment)
("C-=" repeat)
("C-\\" glasses-mode)
("C-h" iy-go-to-char)
("C-j" ace-jump-mode)
("C-l" helm-register)
("C-o" vim-o)
("C-q" backward-delete-char)
("C-u" pop-to-mark-command)
("C-z" helm-buffers-list)
("C-," (lambda() (interactive (winner-undo) (deactivate-mark))))

;; Control Prefix
("C-c C-e" kmacro-end-or-call-macro-repeat)
("C-c C-f" helm-ls-git-ls)
("C-c C-m" magit-status)
("C-c C-o" hoogle-search)
("C-c C-q" kmacro-start-macro)
("C-c C-t" transpose-lines-at-point)
("C-c M-t" transpose-paragraphs)
("C-c C-w" kill-rectangle)
("C-c C-v" inc-register)
("C-c C-y" yank-rectangle)
("C-c C-p" copy-line-up)
("C-c C-n" copy-line-down)
("C-x C-/" comment-or-uncomment-region)
("C-c C--" camelcase-word-or-region)
("C-c C-=" camelcase-word-or-region+)
("C-c C-_" snakecase-word-or-region)
("C-x C-0" sticky-window-delete-window)
("C-x   0" sticky-window-delete-window)
("C-x C-1" sticky-window-delete-other-windows-focus)
("C-x   1" sticky-window-delete-other-windows-focus)
("C-x C-2" split-window-below)
("C-x C-3" split-window-right)
("C-x C-f" helm-find-files)

;; Control Prefix 3
("C-c C-s C-a" helm-bookmarks)
("C-c C-s C-d" helm-dash)
("C-c C-s C-f" helm-swoop-find-files-recursively)
("C-c C-s C-m" mu4e)
("C-c C-s C-r" (lambda () (interactive)
    (let ((current-prefix-arg '(1)))
        (call-interactively 'helm-do-grep))))
("C-c C-s C-s" helm-multi-swoop)

;; Make keys
("C-c C-z C-p" (lambda() (interactive) (run-make "stop")))
("C-c C-z C-r" (lambda() (interactive) (run-make "restart")))
("C-c C-z C-s" (lambda() (interactive) (run-make "start")))
("C-c C-z C-t" (lambda() (interactive) (run-make "test")))
("C-c C-z C-z" (lambda() (interactive) (run-make "")))

;; Escreen Keys
("C-; C-n" escreen-goto-next-screen)
("C-; C-p" escreen-goto-prev-screen)
("C-; C-;" escreen-goto-last-screen)
("C-; C-c" escreen-create-screen)
("C-; C-k" escreen-kill-screen)
("C-; C-1" escreen-goto-screen-1)
("C-; C-2" escreen-goto-screen-2)
("C-; C-3" escreen-goto-screen-3)
("C-; C-4" escreen-goto-screen-4)
("C-; C-5" escreen-goto-screen-5)
("C-; C-6" escreen-goto-screen-6)
("C-; C-7" escreen-goto-screen-7)
("C-; C-8" escreen-goto-screen-8)
("C-; C-9" escreen-goto-screen-9)
("C-; C-0" escreen-goto-screen-0)

;; Meta keys
("M-*" mc/mark-all-like-this)
("M-+" align-regexp)
("M--" redo)
("M-;" capitalize-previous-word)
("M-@" er/expand-region)
("M-N" mc/mark-next-like-this)
("M-P" mc/mark-previous-like-this)
("M-S" helm-swoop)
("M-_" negative-argument)
("M-g" escape-key)
("M-h" iy-go-to-char-backward)
("M-j" (lambda() (interactive) (join-line -1)))
("M-k" kill-this-buffer)
("M-o" (lambda() (interactive) (vim-o 1)))
("M-q" backward-kill-word)
("M-s" (lambda() (interactive) (helm-swoop :$query "")))
("M-x" helm-M-x)
))

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "M-g") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-exit)

;; Other Keys
(global-set-key [f1] 'simpleclip-copy)
(global-set-key [f2] 'simpleclip-paste)
(global-set-key [f3] 'describe-key)
(global-set-key [f4] 'pastie-region)
(global-set-key [f6] 'describe-mode)
(global-set-key [f7] 'get-current-buffer-major-mode)
(global-set-key [f11] 'screenshot-frame)

;; Erlang Keys
(defun erlang-keys-hook ()
    (local-set-key (kbd "M-/") 'erlang-get-error)
    (local-set-key (kbd "M-q") 'backward-kill-word)

    ;; (local-set-key (kbd "M-/") 'dabbrev-expand)      ;- Complete a module or remote function name.
    ;; (local-set-key (kbd "M-.") )      ;- Jump from a function call to its definition.
    ;; (local-set-key (kbd "M-,") )      ;- Jump back from a function definition (multi-level).
    (local-set-key (kbd "C-c C-d C-n") 'erl-choose-nodename) ;- set the erlang node name
    (local-set-key (kbd "C-c C-d C-g") 'erl-ping) ;- upload the distel modules to the erlang node
    (local-set-key (kbd "C-c C-d C-d") 'erl-eval-expression) ;- Evaluate an erlang expression from the minibuffer.
    (local-set-key (kbd "C-c C-d C-f") 'erl-refactor-subfunction) ;- Refactor expressions in the region as a new function.
    (local-set-key (kbd "C-c C-d C-F") 'erl-find-module) ;- Find a module.
    (local-set-key (kbd "C-c C-d C-i") 'edb-toggle-interpret) ;- Toggle debug interpretping of the module.
    (local-set-key (kbd "C-c C-d C-b") 'edb-toggle-breakpoint) ;- Toggle a debugger breakpoint at the current line.
    (local-set-key (kbd "C-c C-d C-p") 'fprof) ;- Profile (with fprof) an expression from the minibuffer.
    (local-set-key (kbd "C-c C-d C-l") 'erl-process-list) ;- Reload an Erlang module.
    (local-set-key (kbd "C-c C-d C-r") 'erl-reload-modules) ;- Reload all Erlang modules that are out of date.
    (local-set-key (kbd "C-c C-d C-w") 'erl-who-calls) ;- Who calls function under point.
    (local-set-key (kbd "C-c C-d C-H") 'erl-find-doc) ;- Show the html documentation for a function.
    (local-set-key (kbd "C-c C-d C-Z") 'erl-find-sig) ;- Show the signature for a function.
    (define-key erl-process-list-mode-map (kbd "n") 'next-line)
    (define-key erl-process-list-mode-map (kbd "p") 'previous-line)
    (define-key erl-process-list-mode-map (kbd "s") 'isearch-forward)
    (define-key erl-process-list-mode-map (kbd "r") 'isearch-backward)
    (define-key erl-process-list-mode-map (kbd "v") 'scroll-up-command)

    ;; Auto complete mode
    (define-key ac-menu-map (kbd "M-n") 'capitalize-previous-word)
)

;; Elixir Keys
(defun elixir-keys-hook ()
  (setq-local doom-indent-key "") ;;; HACK FOR ELIXIR MODE
  (define-key elixir-mode-map (kbd "TAB") (lambda() (interactive)
    (if (and (equal major-mode 'elixir-mode) (not auto-complete-mode))
    (auto-complete-mode 1))
    (indent-of-doom))) ;;; HACK FOR ELIXIR MODE

  (define-key elixir-mode-map (kbd "C-c C-l") 'iex-compile)
  (define-key elixir-mode-map (kbd "C-c C-c C-e")
      (lambda(x) (interactive "sRun Mix > ") (run-mix x)))

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
)

;; Rust Keys
(defun rust-keys-hook ()
  (define-key rust-mode-map (kbd "C-c C-l") 'execute-rust)
)
;; C Keys
(defun c-keys-hook ()
  (define-key c-mode-base-map (kbd "C-c C-l") 'execute-c)
)

;; Dired keys
(define-key dired-mode-map (kbd "c f")   'helm-ls-git-ls)
(define-key dired-mode-map (kbd "z")     'helm-buffers-list)
(define-key dired-mode-map (kbd "c s a") 'helm-bookmarks)
(define-key dired-mode-map (kbd "c m")   'magit-status)
(define-key dired-mode-map (kbd "; n") 'escreen-goto-next-screen)
(define-key dired-mode-map (kbd "; p") 'escreen-goto-prev-screen)
(define-key dired-mode-map (kbd "; ;") 'escreen-goto-last-screen)
(define-key dired-mode-map (kbd "; c") 'escreen-create-screen)
(define-key dired-mode-map (kbd "; k") 'escreen-kill-screen)
(define-key dired-mode-map (kbd "; 1") 'escreen-goto-screen-1)
(define-key dired-mode-map (kbd "; 2") 'escreen-goto-screen-2)
(define-key dired-mode-map (kbd "; 3") 'escreen-goto-screen-3)
(define-key dired-mode-map (kbd "; 4") 'escreen-goto-screen-4)
(define-key dired-mode-map (kbd "; 5") 'escreen-goto-screen-5)
(define-key dired-mode-map (kbd "; 6") 'escreen-goto-screen-6)
(define-key dired-mode-map (kbd "; 7") 'escreen-goto-screen-7)
(define-key dired-mode-map (kbd "; 8") 'escreen-goto-screen-8)
(define-key dired-mode-map (kbd "; 9") 'escreen-goto-screen-9)
(define-key dired-mode-map (kbd "; 0") 'xsescreen-goto-screen-0)

;; Key Chord
(key-chord-define-global "xs"
    (lambda() (interactive) (god-mode-enable) (save-buffer)))

;; Helm keys
(define-key helm-map (kbd "C-b") 'nil)
(define-key helm-map (kbd "C-f") 'nil)
(define-key helm-map (kbd "M-b") 'nil)
(define-key helm-map (kbd "M-f") 'helm-select-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "C-a") 'helm-buffers-toggle-show-hidden-buffers)
(define-key helm-swoop-map (kbd "M-e") 'helm-swoop-edit)

;; Special up / down for helm-register
(define-key helm-map (kbd "C-n")
    (lambda() (interactive) (if (or (boundp 'helm-swoop-active)
                                    (boundp 'helm-register-active))
        (progn (helm-next-line) (helm-execute-persistent-action))
        (helm-next-line))))

(define-key helm-map (kbd "C-p")
    (lambda() (interactive) (if (or (boundp 'helm-swoop-active)
                                    (boundp 'helm-register-active))
        (progn (helm-previous-line) (helm-execute-persistent-action))
        (helm-previous-line))))

; Helm keyboard quits
(define-key helm-map                (kbd "M-?") 'helm-help)
(define-key helm-map                (kbd "M-g") 'helm-keyboard-quit)
(define-key helm-find-files-map     (kbd "M-g") 'helm-keyboard-quit)
(define-key helm-generic-files-map  (kbd "M-g") 'helm-keyboard-quit)
(define-key helm-buffer-map         (kbd "M-g") 'helm-keyboard-quit)

(define-key mc/keymap (kbd "<return>")  'newline)

;; eShell
(add-hook 'eshell-mode-hook
'(lambda ()
   (define-key eshell-mode-map (kbd "C-i") 'helm-esh-pcomplete)))

;; God mode
(define-key god-local-mode-map (kbd "g") 'goto-line)
(define-key god-local-mode-map (kbd ",") (lambda() (interactive (winner-undo) (deactivate-mark))))
(define-key god-local-mode-map (kbd "i") 'god-mode-disable)
(define-key god-local-mode-map (kbd "[") (lambda ()
    (interactive) (scroll-down-line 3)))
(define-key god-local-mode-map (kbd "]") (lambda ()
    (interactive) (scroll-up-line 3)))

(define-key god-local-mode-map (kbd "; n")  'escreen-goto-next-screen)
(define-key god-local-mode-map (kbd "; p")  'escreen-goto-prev-screen)
(define-key god-local-mode-map (kbd "; ;")  'escreen-goto-last-screen)
(define-key god-local-mode-map (kbd "; c")  'escreen-create-screen)
(define-key god-local-mode-map (kbd "; k")  'escreen-kill-screen)
(define-key god-local-mode-map (kbd "; 1")  'escreen-goto-screen-1)
(define-key god-local-mode-map (kbd "; 2")  'escreen-goto-screen-2)
(define-key god-local-mode-map (kbd "; 3")  'escreen-goto-screen-3)
(define-key god-local-mode-map (kbd "; 4")  'escreen-goto-screen-4)
(define-key god-local-mode-map (kbd "; 5")  'escreen-goto-screen-5)
(define-key god-local-mode-map (kbd "; 6")  'escreen-goto-screen-6)
(define-key god-local-mode-map (kbd "; 7")  'escreen-goto-screen-7)
(define-key god-local-mode-map (kbd "; 8")  'escreen-goto-screen-8)
(define-key god-local-mode-map (kbd "; 9")  'escreen-goto-screen-9)
(define-key god-local-mode-map (kbd "; 0")  'escreen-goto-screen-0)

;; Magit mode
(define-key magit-status-mode-map (kbd "RET") (lambda () (interactive) (magit-visit-item t)))

;; Modes
(define-minor-mode attic-minor-mode
"A minor mode so that my key settings override annoying major modes."

t " attic" 'attic-minor-mode-map)
(defun attic-minibuffer-setup-hook ()
(attic-minor-mode 0))

;; Other unset keys
(global-unset-key "\C-x\C-z")
(provide 'my-keys)

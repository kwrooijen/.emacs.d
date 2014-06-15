(defvar attic-minor-mode-map (make-keymap) "attic-minor-mode keymap.")

(defun attic-key(key function)
  (define-key attic-minor-mode-map (kbd key) function)
  (global-set-key (kbd key) function)
)
(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a))) '(

("<escape>" escape-key)

;; Control Keys
("C--" undo)
("C-." helm-resume)
("C-/" my-comment)
("C-h" iy-go-to-char-backward)
("C-j" ace-jump-mode)
("C-l" iy-go-to-char)
("C-q" backward-delete-char)
("C-u" pop-to-mark-command)
("C-z" helm-buffers-list)
("C-o" vim-o)
("C-;" helm-M-x)

;; Control Prefix
("C-c C-e" kmacro-end-or-call-macro-repeat)
("C-c C-f" helm-ls-git-ls)
("C-c C-m" magit-status)
("C-c C-o" hoogle-search)
("C-c C-q" kmacro-start-macro)
("C-c C-t" transpose-paragraphs)
("C-c C-w" kill-rectangle)
("C-c C-y" yank-rectangle)
("C-c C-p" copy-line-up)
("C-c C-n" copy-line-down)
("C-c C-u" winner-undo)
("C-c C-i" winner-redo)
("C-x C-/" comment-or-uncomment-region)
("C-x C-0" delete-window)
("C-x C-1" delete-other-windows)
("C-x C-2" split-window-below)
("C-x C-3" split-window-right)
("C-x C-f" helm-find-files)
("C-c C-d" (lambda () (interactive)
    (winner-undo)
    (command-repeater '(("d" . winner-undo)))))

;; Control Prefix 3
("C-c C-s C-f" helm-swoop-find-files-recursively)
("C-c C-s C-a" helm-bookmarks)
("C-c C-s C-d" helm-dash)
("C-c C-s C-m" mu4e)
("C-c C-s C-s" helm-multi-swoop)
("C-c C-s C-r" (lambda () (interactive)
    (let ((current-prefix-arg '(1)))
      (call-interactively 'helm-do-grep))))

;; Make keys
("C-c C-z C-p" (lambda() (interactive) (run-make "stop")))
("C-c C-z C-r" (lambda() (interactive) (run-make "restart")))
("C-c C-z C-s" (lambda() (interactive) (run-make "start")))
("C-c C-z C-t" (lambda() (interactive) (run-make "test")))
("C-c C-z C-z" (lambda() (interactive) (run-make "")))

;; Meta keys
("M-*" mc/mark-all-like-this)
("M-+" align-regexp)
("M-@" er/expand-region)
("M--" redo)
("M-_" negative-argument)
("M-N" mc/mark-next-like-this)
("M-P" mc/mark-previous-like-this)
("M-g" escape-key)
("M-j" (lambda() (interactive) (join-line -1)))
("M-k" kill-this-buffer)
("M-o" (lambda() (interactive) (vim-o 1)))
("M-q" backward-kill-word)
("M-s" helm-swoop)
("M-x" helm-M-x)
))

(key-chord-define-global "xs"
    (lambda() (interactive) (god-mode-enable) (save-buffer)))

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "M-g") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-exit)

;; Other Keys
(global-set-key [f1] 'copy-to-clipboard)
(global-set-key [f2] 'paste-from-clipboard)
(global-set-key [f3] 'describe-key)
(global-set-key [f6] 'describe-mode)
(global-set-key [f7] 'get-current-buffer-major-mode)

;; Erlang Keys
(defun erlang-keys-hook ()
  (local-set-key (kbd "M-?") 'erlang-get-error)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "M-q") 'backward-kill-word)
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
(define-key dired-mode-map (kbd "c f") 'helm-ls-git-ls)
(define-key dired-mode-map (kbd "z")   'helm-buffers-list)

;; Key Chord
(space-chord-define-global                   " " 'escape-key)
(space-chord-define isearch-mode-map         " " 'isearch-abort)
(space-chord-define helm-map                 " " 'helm-keyboard-quit)
(space-chord-define helm-find-files-map      " " 'helm-keyboard-quit)
(space-chord-define helm-generic-files-map   " " 'helm-keyboard-quit)
(space-chord-define helm-buffer-map          " " 'helm-keyboard-quit)

;; Helm keys
(define-key helm-map (kbd "C-b") 'nil)
(define-key helm-map (kbd "C-f") 'nil)
(define-key helm-map (kbd "M-b") 'nil)
(define-key helm-map (kbd "M-f") 'helm-select-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "C-a") 'helm-buffers-toggle-show-hidden-buffers)
(define-key helm-swoop-map (kbd "M-e") 'helm-swoop-edit)
; Helm keyboard quits
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
(define-key god-local-mode-map (kbd "i") 'god-mode-disable)
(define-key god-local-mode-map (kbd "[") (lambda ()
    (interactive) (scroll-down-line 3)))
(define-key god-local-mode-map (kbd "]") (lambda ()
    (interactive) (scroll-up-line 3)))

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

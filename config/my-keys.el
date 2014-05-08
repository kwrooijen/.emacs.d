(defvar insert-mode-map (make-keymap) "insert-mode keymap.")

(key-chord-define-global "qq" 'god-mode-enable)
(key-chord-define-global "xs" (lambda()
                                (interactive)
                                (god-mode-enable)
                                (save-buffer)))

;; Control Keys
(define-key god-local-mode-map (kbd "C-u") 'pop-to-mark-command)
(define-key god-local-mode-map (kbd "C-q") 'backward-delete-char)
(define-key god-local-mode-map (kbd "C--") 'undo)
(define-key god-local-mode-map (kbd "C-l") 'iy-go-up-to-char)
(define-key god-local-mode-map (kbd "C-h") 'helm-M-x)
(define-key god-local-mode-map (kbd "C-z") 'helm-buffers-list)
(define-key god-local-mode-map (kbd "C-.") 'helm-resume)
(define-key god-local-mode-map (kbd "C-j") 'ace-jump-mode)
(define-key god-local-mode-map (kbd "C-/") 'comment-or-uncomment-region)

;; Control Prefix
(define-key god-local-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key god-local-mode-map (kbd "C-c C-f") 'helm-ls-git-ls)
(define-key god-local-mode-map (kbd "C-c C-o") 'hoogle-search)
(define-key god-local-mode-map (kbd "C-c C-t") 'transpose-paragraphs)
(define-key god-local-mode-map (kbd "C-c C-m") 'magit-status)
(define-key god-local-mode-map (kbd "C-c C-q") 'kmacro-start-macro)
(define-key god-local-mode-map (kbd "C-c C-e") 'kmacro-end-or-call-macro-repeat)
(define-key god-local-mode-map (kbd "C-x C-1") 'delete-other-windows)
(define-key god-local-mode-map (kbd "C-x C-2") 'split-window-below)
(define-key god-local-mode-map (kbd "C-x C-3") 'split-window-right)
(define-key god-local-mode-map (kbd "C-x C-0") 'delete-window)

;; Control Prefix 3
(define-key god-local-mode-map (kbd "C-c C-w C-e") 'load-attic-workgroups)
(define-key god-local-mode-map (kbd "C-c C-w C-w") 'load-attic-workgroups2)
(define-key god-local-mode-map (kbd "C-c C-s C-g") 'helm-do-grep)
(define-key god-local-mode-map (kbd "C-c C-s C-s") 'helm-swoop)
(define-key god-local-mode-map (kbd "C-c C-s C-m") 'helm-multi-swoop)
(define-key god-local-mode-map (kbd "C-c C-s C-r") (lambda () (interactive)
    (let ((current-prefix-arg '(1)))
      (call-interactively 'helm-do-grep))))

;; Make keys
(define-key god-local-mode-map (kbd "C-c C-z C-z") 'run-make)
(define-key god-local-mode-map (kbd "C-c C-z C-j") 'run-make-js)
(define-key god-local-mode-map (kbd "C-c C-z C-s") 'run-make-start)
(define-key god-local-mode-map (kbd "C-c C-z C-r") 'run-make-restart)

;; Meta Keys
(define-key god-local-mode-map (kbd "M-q") 'backward-kill-word)
(define-key god-local-mode-map (kbd "M-+") 'align-regexp)
(define-key god-local-mode-map (kbd "M-@") 'er/expand-region)
(define-key god-local-mode-map (kbd "M-x") nil)
(define-key god-local-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key god-local-mode-map (kbd "M-P") 'mc/mark-previous-like-this)
(define-key god-local-mode-map (kbd "M-N") 'mc/mark-next-like-this)
(define-key god-local-mode-map (kbd "M-*") 'mc/mark-all-like-this)
(define-key god-local-mode-map (kbd "M-_") 'redo)
(define-key god-local-mode-map (kbd "M-C-_") 'redo)
(define-key god-local-mode-map (kbd "M-C--") 'redo)
(define-key god-local-mode-map (kbd "M-j") (lambda() (interactive) (join-line -1)))
(define-key god-local-mode-map (kbd "M-g") 'goto-line)
(define-key minibuffer-local-map (kbd "M-g") 'keyboard-escape-quit)
(define-key mc/keymap (kbd "M-g") 'keyboard-escape-quit-mc)

;; Other Keys
(define-key god-local-mode-map [f7] 'get-current-buffer-major-mode)
(define-key god-local-mode-map [f1] 'copy-to-clipboard)
(define-key god-local-mode-map [f2] 'paste-from-clipboard)

;; Erlang Keys
(defun erlang-keys-hook ()
  (local-set-key (kbd "M-?") 'erlang-get-error)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "M-q") 'backward-kill-word)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
)

;; Helm keys
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-f") 'helm-select-action)
(define-key helm-map (kbd "M-b") 'nil)
(define-key helm-map (kbd "C-f") 'nil)
(define-key helm-map (kbd "C-b") 'nil)
(define-key helm-buffer-map (kbd "C-a") 'helm-buffers-toggle-show-hidden-buffers)
(define-key helm-map (kbd "M-g M-g") 'helm-keyboard-quit)

;; God mode
(define-key god-local-mode-map (kbd "i")   'god-mode-disable)
(define-key god-local-mode-map (kbd "[") (lambda ()
    (interactive) (scroll-down-line 3)))
(define-key god-local-mode-map (kbd "]") (lambda ()
    (interactive) (scroll-up-line 3)))

; Disable Control keys in insert mode
(defvar insert-minor-mode-map (make-keymap)
  "insert-minor-mode keymap.")


(let ((f (lambda () `(lambda () (interactive)
              (message (concat "Exit insert mode first."))))))
  (dolist (l '(("C-a") ("C-q") ("C-w") ("C-e") ("C-r") ("C-t")
               ("C-y") ("C-u") ("C-i") ("C-o") ("C-p") ("C-a")
               ("C-s") ("C-d") ("C-f") ("C-g") ("C-h") ("C-j")
               ("C-k") ("C-l") ("C-z") ("C-x") ("C-c") ("C-v")
               ("C-b") ("C-n") ("C-m")))
    (define-key insert-minor-mode-map
      (read-kbd-macro (car l)) (funcall f))))


(define-minor-mode insert-minor-mode
  "Insert mode" nil nil 'insert-minor-mode-map)

(provide 'my-keys)

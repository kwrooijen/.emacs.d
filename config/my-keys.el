(defvar insert-mode-map (make-keymap) "insert-mode keymap.")

(key-chord-define-global "qq" 'god-mode-enable)
(key-chord-define-global "xs" (lambda()
                                (interactive)
                                (god-mode-enable)
                                (save-buffer)))

;; Control Keys
(global-set-key (kbd "C-u") 'pop-to-mark-command)
(global-set-key (kbd "C-q") 'backward-delete-char)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-l") 'iy-go-up-to-char)
(global-set-key (kbd "C-h") 'helm-M-x)
(global-set-key (kbd "C-z") 'helm-buffers-list)
(global-set-key (kbd "C-.") 'helm-resume)
(global-set-key (kbd "C-j") 'ace-jump-mode)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; Control Prefix
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c C-f") 'helm-ls-git-ls)
(global-set-key (kbd "C-c C-o") 'hoogle-search)
(global-set-key (kbd "C-c C-t") 'transpose-paragraphs)
(global-set-key (kbd "C-c C-m") 'magit-status)
(global-set-key (kbd "C-c C-q") 'kmacro-start-macro)
(global-set-key (kbd "C-c C-e") 'kmacro-end-or-call-macro-repeat)
(global-set-key (kbd "C-c C-z") 'run-make)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; Control Prefix 3
(global-set-key (kbd "C-c C-w C-e") 'load-attic-workgroups)
(global-set-key (kbd "C-c C-w C-w") 'load-attic-workgroups2)
(global-set-key (kbd "C-c C-s C-g") 'helm-do-grep)
(global-set-key (kbd "C-c C-s C-s") 'helm-swoop)
(global-set-key (kbd "C-c C-s C-m") 'helm-multi-swoop)
(global-set-key (kbd "C-c C-s C-r") (lambda () (interactive)
    (let ((current-prefix-arg '(1)))
      (call-interactively 'helm-do-grep))))

;; Meta Keys
(global-set-key (kbd "M-q") 'backward-kill-word)
(global-set-key (kbd "M-+") 'align-regexp)
(global-set-key (kbd "M-@") 'er/expand-region)
(global-set-key (kbd "M-x") nil)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-N") 'mc/mark-next-like-this)
(global-set-key (kbd "M-*") 'mc/mark-all-like-this)
(global-set-key (kbd "M-_") 'redo)
(global-set-key (kbd "M-C-_") 'redo)
(global-set-key (kbd "M-C--") 'redo)
(global-set-key (kbd "M-j") (lambda() (interactive) (join-line -1)))
(global-set-key (kbd "M-g") 'goto-line)
(define-key minibuffer-local-map (kbd "M-g") 'keyboard-escape-quit)
(define-key mc/keymap (kbd "M-g") 'keyboard-escape-quit-mc)

;; Other Keys
(global-set-key [f7] 'get-current-buffer-major-mode)
(global-set-key [f1] 'copy-to-clipboard)
(global-set-key [f2] 'paste-from-clipboard)

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

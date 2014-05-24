(defvar attic-minor-mode-map (make-keymap) "attic-minor-mode keymap.")
(defvar insert-mode-map (make-keymap) "insert-mode keymap.")

(defun attic-key(key function)
  (define-key attic-minor-mode-map (kbd key) function)
  (global-set-key (kbd key) function)
)

(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a))) '(

("<escape>" god-mode-enable)

;; Control Keys
("C--" undo)
("C-." helm-resume)
("C-/" comment-or-uncomment-region)
("C-h" iy-go-to-char-backward)
("C-j" ace-jump-mode)
("C-l" iy-go-to-char)
("C-q" backward-delete-char)
("C-u" pop-to-mark-command)
("C-z" helm-buffers-list)

;; Control Prefix
("C-c C-e" kmacro-end-or-call-macro-repeat)
("C-c C-f" helm-ls-git-ls)
("C-c C-m" magit-status)
("C-c C-o" hoogle-search)
("C-c C-q" kmacro-start-macro)
("C-c C-t" transpose-paragraphs)
("C-c C-w" kill-rectangle)
("C-c C-y" yank-rectangle)
("C-x C-0" delete-window)
("C-x C-1" delete-other-windows)
("C-x C-2" split-window-below)
("C-x C-3" split-window-right)
("C-x C-f" helm-find-files)

;; Control Prefix 3
("C-c C-s C-f" helm-swoop-find-files-recursively)
("C-c C-s C-m" helm-multi-swoop)
("C-c C-s C-s" (lambda() (interactive)
                 (helm-swoop :$query "")))
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
("M-*"   mc/mark-all-like-this)
("M-+"   align-regexp)
("M-@"   er/expand-region)
("M-C--" redo)
("M-C-_" redo)
("M-N"   mc/mark-next-like-this)
("M-P"   mc/mark-previous-like-this)
("M-_"   redo)
("M-g"   goto-line)
("M-j"   (lambda() (interactive) (join-line -1)))
("M-k"   kill-this-buffer)
("M-q"   backward-kill-word)
("M-x"   helm-M-x)
))

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

;; Key Chord
(key-chord-define insert-mode-map "xs"
    (lambda() (interactive)
        (if (string-equal (buffer-name) "*Helm Swoop Edit*")
            (helm-swoop--edit-complete))
        (god-mode-enable)
        (save-buffer)))

;; Other Keys
(global-set-key [f1] 'copy-to-clipboard)
(global-set-key [f2] 'paste-from-clipboard)
(global-set-key [f3] 'describe-key)
(global-set-key [f7] 'get-current-buffer-major-mode)

;; Erlang Keys
(defun erlang-keys-hook ()
  (local-set-key (kbd "M-?") 'erlang-get-error)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "M-q") 'backward-kill-word)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
)


;; Helm keys
(define-key helm-map (kbd "C-b") 'nil)
(define-key helm-map (kbd "C-f") 'nil)
(define-key helm-map (kbd "M-b") 'nil)
(define-key helm-map (kbd "M-f") 'helm-select-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "C-a") 'helm-buffers-toggle-show-hidden-buffers)
(define-key helm-swoop-map (kbd "M-e") 'helm-swoop-edit)

;; eShell
(add-hook 'eshell-mode-hook
'(lambda ()
   (define-key eshell-mode-map (kbd "C-i") 'helm-esh-pcomplete)))

;; God mode
(define-key god-local-mode-map (kbd "g") 'god-g)
(define-key god-local-mode-map (kbd "i") 'god-mode-disable)
(define-key god-local-mode-map (kbd "[") (lambda ()
    (interactive) (scroll-down-line 3)))
(define-key god-local-mode-map (kbd "]") (lambda ()
    (interactive) (scroll-up-line 3)))

;; Modes
(define-minor-mode attic-minor-mode
"A minor mode so that my key settings override annoying major modes."

t " attic" 'attic-minor-mode-map)
(defun attic-minibuffer-setup-hook ()
(attic-minor-mode 0))

;; Disable Control keys in insert mode
(define-minor-mode insert-mode
  "Insert mode" nil nil 'insert-mode-map)

(define-key insert-mode-map (kbd "C-g") 'god-mode-enable)
(let ((f (lambda () `(lambda () (interactive)
              (message (concat "Exit insert mode first."))))))
  (dolist (l '(("C-a") ("C-q") ("C-w") ("C-e") ("C-r") ("C-t")
               ("C-y") ("C-u") ("C-o") ("C-p") ("C-a") ("C-s")
               ("C-d") ("C-f") ("C-j") ("C-k") ("C-l") ("C-z")
               ("C-x") ("C-v") ("C-b") ("C-n") ))
    (define-key insert-mode-map
      (read-kbd-macro (car l)) (funcall f))))

;; Other unset keys
(global-unset-key "\C-x\C-z")
(provide 'my-keys)

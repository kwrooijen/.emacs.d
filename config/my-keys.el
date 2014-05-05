(defvar attic-minor-mode-map (make-keymap) "attic-minor-mode keymap.")

; Unset Keys
(dolist (key '("\C-z"))
  (global-unset-key key))

;; Control Keys
(define-key attic-minor-mode-map (kbd "C-u") 'pop-to-mark-command)
(define-key attic-minor-mode-map (kbd "C-.") 'helm-resume)
(define-key attic-minor-mode-map (kbd "C-q") 'backward-delete-char)
(define-key attic-minor-mode-map (kbd "C--") 'undo)

;; Control Prefix Keys
(define-key attic-minor-mode-map (kbd "C-M-_")	 'redo)
(define-key attic-minor-mode-map (kbd "C-M--")	 'redo)
(define-key attic-minor-mode-map (kbd "C-x M-t") 'transpose-paragraphs)
(define-key attic-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key attic-minor-mode-map (kbd "C-c C-v") 'undo-tree-visualize)

;; Meta Keys
(global-set-key (kbd "M-g") 'god-mode-enable)
(define-key attic-minor-mode-map (kbd "M-q") 'backward-kill-word)
(define-key attic-minor-mode-map (kbd "M-o") 'yas/expand)
(define-key attic-minor-mode-map (kbd "M-x") 'execute-extended-command)
(define-key attic-minor-mode-map (kbd "M-+") 'align-regexp)
(define-key attic-minor-mode-map (kbd "M-@") 'er/expand-region)
(define-key attic-minor-mode-map (kbd "M-t") 'transpose-words)
(define-key attic-minor-mode-map (kbd "M-E") 'mc/edit-lines)
(define-key attic-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key attic-minor-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key attic-minor-mode-map (kbd "M-P")'mc/mark-previous-like-this)

(define-key attic-minor-mode-map (kbd "M-N") 'mc/mark-next-like-this)
(define-key attic-minor-mode-map (kbd "M-*") 'mc/mark-all-like-this)
(define-key attic-minor-mode-map (kbd "M-j") (lambda()
                                               (interactive)
                                               (join-line -1)))

;; C Prefix Keys
(define-prefix-command 'c-j-prefix)
(define-key attic-minor-mode-map (kbd "C-c C-o") 'hoogle-search)
(define-key attic-minor-mode-map (kbd "C-c C-t") 'transpose-paragraphs)
(define-key attic-minor-mode-map (kbd "C-l")     'iy-go-up-to-char)
(define-key attic-minor-mode-map (kbd "C-h")     'iy-go-to-char-backward)
(define-key attic-minor-mode-map (kbd "C-z") 'helm-buffers-list)
(define-key attic-minor-mode-map (kbd "C-j")     'ace-jump-mode)
(define-key attic-minor-mode-map (kbd "C-/")     'comment-or-uncomment-region)
(define-key attic-minor-mode-map (kbd "C-c C-v") 'org-cycle-agenda-files)
(define-key attic-minor-mode-map (kbd "C-c C-m") 'magit-status)
(define-key attic-minor-mode-map (kbd "C-c C-q") 'kmacro-start-macro)
(define-key attic-minor-mode-map (kbd "C-c C-e")
    'kmacro-end-or-call-macro-repeat)
(define-key attic-minor-mode-map (kbd "C-c C-z") 'run-make)
(define-key attic-minor-mode-map (kbd "C-c C-w C-e") 'load-attic-workgroups)
(define-key attic-minor-mode-map (kbd "C-c C-w C-w") 'load-attic-workgroups2)
(define-key attic-minor-mode-map (kbd "C-c C-f") 'helm-ls-git-ls)

(define-key attic-minor-mode-map (kbd "C-c C-g C-g") 'helm-do-grep)
(define-key attic-minor-mode-map (kbd "C-c C-g C-s") 'helm-swoop)
(define-key attic-minor-mode-map (kbd "C-c C-g C-m") 'helm-multi-swoop)
(define-key attic-minor-mode-map (kbd "C-c C-g C-r") (lambda () (interactive)
    (let ((current-prefix-arg '(1)))
      (call-interactively 'helm-do-grep))))


(define-key attic-minor-mode-map (kbd "C-c C-b C-n") 'copy-line-to-next-line)
(define-key attic-minor-mode-map (kbd "C-c C-b C-p") 'copy-line-to-previous-line)
(define-key attic-minor-mode-map (kbd "C-c C-b C-a") 'copy-line-to-end-of-line)
(define-key attic-minor-mode-map (kbd "C-c C-b C-e") 'copy-line-to-beginning-of-line)

(define-key attic-minor-mode-map (kbd "<escape>") 'god-mode-enable)

(define-key attic-minor-mode-map (kbd "C-c C-r") (key-binding (kbd "\C-xr")))

(define-key attic-minor-mode-map (kbd "C-c C-1") 'wg-switch-to-index-0)
(define-key attic-minor-mode-map (kbd "C-c C-2") 'wg-switch-to-index-1)
(define-key attic-minor-mode-map (kbd "C-c C-3") 'wg-switch-to-index-2)
(define-key attic-minor-mode-map (kbd "C-c C-4") 'wg-switch-to-index-3)
(define-key attic-minor-mode-map (kbd "C-c C-5") 'wg-switch-to-index-4)
(define-key attic-minor-mode-map (kbd "C-c C-6") 'wg-switch-to-index-5)
(define-key attic-minor-mode-map (kbd "C-c C-7") 'wg-switch-to-index-6)
(define-key attic-minor-mode-map (kbd "C-c C-8") 'wg-switch-to-index-7)
(define-key attic-minor-mode-map (kbd "C-c C-9") 'wg-switch-to-index-8)
(define-key attic-minor-mode-map (kbd "C-c C-0") 'wg-switch-to-index-9)

(define-key attic-minor-mode-map (kbd "C-c 1") 'wg-switch-to-index-0)
(define-key attic-minor-mode-map (kbd "C-c 2") 'wg-switch-to-index-1)
(define-key attic-minor-mode-map (kbd "C-c 3") 'wg-switch-to-index-2)
(define-key attic-minor-mode-map (kbd "C-c 4") 'wg-switch-to-index-3)
(define-key attic-minor-mode-map (kbd "C-c 5") 'wg-switch-to-index-4)
(define-key attic-minor-mode-map (kbd "C-c 6") 'wg-switch-to-index-5)
(define-key attic-minor-mode-map (kbd "C-c 7") 'wg-switch-to-index-6)
(define-key attic-minor-mode-map (kbd "C-c 8") 'wg-switch-to-index-7)
(define-key attic-minor-mode-map (kbd "C-c 9") 'wg-switch-to-index-8)
(define-key attic-minor-mode-map (kbd "C-c 0") 'wg-switch-to-index-9)

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
(define-key helm-buffer-map (kbd "C-a")
  'helm-buffers-toggle-show-hidden-buffers)
(define-prefix-command 'm-g-prefix)
(define-key helm-map (kbd "M-g M-g") 'helm-keyboard-quit)

;; God mode
(define-key god-local-mode-map (kbd "i")   'god-mode-disable)
(define-key god-local-mode-map (kbd "M-g") 'keyboard-escape-quit-mc)
(define-key god-local-mode-map (kbd "[") (lambda ()
    (interactive) (scroll-down-line 3)))
(define-key god-local-mode-map (kbd "]") (lambda ()
    (interactive) (scroll-up-line 3)))


(define-key god-local-mode-map (kbd "C-x C-s") (lambda()
    (interactive)
    (save-buffer)
    (god-mode-enable)))

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

; auto-complete
(define-key ac-complete-mode-map "\r" nil)

; Global Keys
(global-set-key [f7] 'get-current-buffer-major-mode)
(global-set-key [f1] 'copy-to-clipboard)
(global-set-key [f2] 'paste-from-clipboard)

; Web Mode
;(define-key web-mode-map (kbd "C-z") 'zencoding-expand-yas)

; Define mode
(define-minor-mode attic-minor-mode
"A minor mode so that my key settings override annoying major modes."

t " attic" 'attic-minor-mode-map)
(defun attic-minibuffer-setup-hook ()
	(attic-minor-mode 0))

(provide 'my-keys)

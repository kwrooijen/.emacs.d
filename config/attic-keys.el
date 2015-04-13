(defvar attic-minor-mode-map (make-keymap) "attic-minor-mode keymap.")
(define-prefix-command 'semi-colon-map)
(define-prefix-command 'attic-make-map)

(defun attic-key(key function)
  (define-key attic-minor-mode-map (kbd key) function))

(global-set-key (kbd "C-M-]") 'attic-minor-mode)
(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a))) '(

("<escape>" escape-key)

;; Control Keys
("C--" undo)
("C-," helm-resume)
("C-." repeat)
("C-/" my-comment)
("C-=" repeat)
("C-j" iy-go-to-char)
("C-l" helm-register)
("C-o" vim-o)
("C-q" backward-delete-char)
("C-z" helm-mini)

("C-M-q" backward-kill-sexp)
("M-y" (lambda() (interactive)
    (if (or (equal last-command 'yank) (equal last-command 'yank-pop))
        (yank-pop)
        (helm-show-kill-ring))))

;; Control Prefix
("C-c C-e" kmacro-end-or-call-macro-repeat)
("C-c C-f" helm-ls-git-ls)
("C-c C-m" magit-status)
("C-c C-q" kmacro-start-macro)
("C-c C-t" transpose-mark)
("C-c M-t" transpose-paragraphs)
("C-c C-w" kill-rectangle)
("C-c C-v" inc-register)
("C-c C-y" yank-rectangle)
("C-c C-o" switch-to-minibuffer)
("C-c C-p" copy-line-up)
("C-c C-n" copy-line-down)
("C-c C-+" camelcase-word-or-region)
("C-c C-=" camelcase-word-or-region+)
("C-c C--" snakecase-word-or-region)
("C-x C-/" comment-or-uncomment-region)
("C-x C-0" sticky-window-delete-window)
("C-x   0" sticky-window-delete-window)
("C-x C-1" delete-other-windows)
;; ("C-x C-1" sticky-window-delete-other-windows-focus)
;; ("C-x   1" sticky-window-delete-other-windows-focus)
("C-x C-2" split-window-below)
("C-x C-3" split-window-right)
("C-x C-f" helm-find-files)
("C-x C-k" kill-this-buffer)

("C-;" semi-colon-map)

;; Control Prefix 3
("C-c C-s C-a" helm-bookmarks)
("C-c C-s C-d" helm-dash)
("C-c C-s C-f" helm-swoop-find-files-recursively)
("C-c C-s C-m" mu4e)

("C-c C-s C-r" my/grep)
("C-c C-s C-s" helm-multi-swoop)

("C-c C-z" attic-make-map)

;; Meta keys
("M-*" mc/mark-all-like-this)
("M-+" align-regexp)
("M--" redo)
("M-C" capitalize-previous-word)
("M-;" (lambda() (interactive) (ac-stop) (company-abort) (yas/expand)))
("M-@" er/expand-region)
("M-#" align-regexp)
("M-N" mc/mark-next-like-this)
("M-P" mc/mark-previous-like-this)
("M-S" helm-swoop)
("M-_" negative-argument)
("M-g" escape-key)
("M-i" tab-to-tab-stop-line-or-region)
("M-I" (lambda() (interactive) (tab-to-tab-stop-line-or-region t)))
("M-j" iy-go-to-char-backward)
("M-h" (lambda() (interactive) (join-line -1)))
("M-o" (lambda() (interactive) (vim-o 1)))
("M-q" backward-kill-word)
("M-s" (lambda() (interactive) (helm-swoop :$query "")))
("M-x" helm-M-x)
("M-\\" spawn-eshell)
("M-," (lambda() (interactive (winner-undo) (deactivate-mark))))
("M-0" attic-sauron-toggle)
("M-9" attic-neotree-toggle)
("s-w" other-frame)))

(defun set-map-list (a)
  (interactive)
  (let ((map (nth 0 a))
        (list (nth 1 a)))
    (mapcar (lambda(key-val)
              (interactive)
              (let ((key (nth 0 key-val))
                    (func (nth 1 key-val)))
                (define-key map (kbd key) func)
                (define-key map (kbd (format "C-%s" key)) func))) list)))

(mapcar 'set-map-list
        '((semi-colon-map (("n" sauron-select-last-event)
                           ("p" escreen-goto-prev-screen)
                           (";" escreen-goto-last-screen)
                           ("c" escreen-create-screen)
                           ("a" async-shell-command)
                           ("s" shell-command)
                           ("f" helm-ls-git-project)
                           ("y" x-clipboard-yank)
                           ("w" clipboard-kill-region)
                           ("e" eww)
                           ("b" eww-list-bookmarks)
                           ("h" select-line-from-indentation)
                           ("1" escreen-goto-screen-1)
                           ("2" escreen-goto-screen-2)
                           ("3" escreen-goto-screen-3)
                           ("4" escreen-goto-screen-4)
                           ("5" escreen-goto-screen-5)
                           ("6" escreen-goto-screen-6)
                           ("7" escreen-goto-screen-7)
                           ("8" escreen-goto-screen-8)
                           ("9" escreen-goto-screen-9)
                           ("0" xsescreen-goto-screen-0)
                           ("'" helm-org-capture-templates)))
          (attic-make-map (("p" (lambda() (interactive) (run-make "stop"    "[Make Stop]")))
                           ("r" (lambda() (interactive) (run-make "restart" "[Make Restart]")))
                           ("s" (lambda() (interactive) (run-make "start"   "[Make Start]")))
                           ("t" (lambda() (interactive) (run-make "test"    "[Make Test]")))
                           ("o" (lambda() (interactive) (run-make "go"      "[Make Go]")))
                           ("z" (lambda() (interactive) (run-make ""        "[Make]")))
                           ("c" run-make-input)))))

;; Make keys

(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "M-g") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-exit)

;; Other Keys
(global-set-key [f3] 'describe-key)
(global-set-key [f4] 'send-to-pastie)
(global-set-key [f6] 'describe-mode)
(global-set-key [f7] 'get-current-buffer-major-mode)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
(global-set-key [f11] 'screenshot-frame)

;; C Keys
(defun c-keys-hook ()
(define-key c-mode-base-map (kbd "C-c C-l") 'execute-c)
(define-key c-mode-base-map (kbd "C-/") 'my-comment)
(define-key god-local-mode-map (kbd "/") 'my-comment))

;; Dired keys
(define-key dired-mode-map (kbd "c f")   'helm-ls-git-ls)
(define-key dired-mode-map (kbd "z")     'helm-mini)
(define-key dired-mode-map (kbd "c s a") 'helm-bookmarks)
(define-key dired-mode-map (kbd "c s r") 'my/grep)
(define-key dired-mode-map (kbd "c m")   'magit-status)
(define-key dired-mode-map (kbd ";")     'semi-colon-map)
(define-key dired-mode-map (kbd "c z")   'attic-make-map)

;; Key Chord
(key-chord-define-global "xs"
    (lambda() (interactive) (god-mode-enable) (save-buffer)))

(key-chord-define-global ";j"
    (lambda() (interactive) (escape-key) (god-mode-enable)))

(key-chord-define attic-minor-mode-map ";j" (lambda() (interactive) (escape-key) (god-mode-enable)))
(key-chord-define isearch-mode-map ";j" 'isearch-abort)

(define-key mc/keymap (kbd "<return>")  'newline)

;; Grep mode
(define-key grep-mode-map (kbd "n") 'next-line)
(define-key grep-mode-map (kbd "p") 'previous-line)
(define-key grep-mode-map (kbd "TAB") (lambda() (interactive) (error-preview "*grep*")))
(define-key grep-mode-map (kbd "v") 'scroll-up-command)
(define-key grep-mode-map (kbd ";") 'semi-colon-map)

;; God mode
(define-key god-local-mode-map (kbd "g") 'goto-line)
(define-key god-local-mode-map (kbd "h") 'ace-jump-mode)
(define-key god-local-mode-map (kbd "i") 'god-mode-disable)
(define-key god-local-mode-map (kbd "[") 'scroll-down-line)
(define-key god-local-mode-map (kbd "]") 'scroll-up-line)
(define-key god-local-mode-map (kbd ";") 'semi-colon-map)

(require 'eww)
(define-key eww-mode-map (kbd "n") (lambda() (interactive) (scroll-up 1)))
(define-key eww-mode-map (kbd "p") (lambda() (interactive) (scroll-down 1)))
(define-key eww-mode-map (kbd "v") 'scroll-up-command)

;; Magit mode
(define-key magit-status-mode-map (kbd "RET") (lambda () (interactive) (magit-visit-item t)))
(define-key magit-status-mode-map (kbd "g") 'magit-refresh)
(define-key magit-status-mode-map (kbd ";") 'semi-colon-map)
(define-key magit-diff-mode-map   (kbd ";") 'semi-colon-map)
(define-key magit-commit-mode-map (kbd ";") 'semi-colon-map)

;; Package Menu mode
(define-key package-menu-mode-map (kbd ";") 'semi-colon-map)

(require 'doc-view)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)

(define-key help-mode-map (kbd ";") 'semi-colon-map)
(define-key help-mode-map (kbd "z") 'helm-mini)

(define-key grep-mode-map (kbd ";") 'semi-colon-map)
(define-key grep-mode-map (kbd "z") 'helm-mini)

(define-key doc-view-mode-map (kbd ";") 'semi-colon-map)
(define-key doc-view-mode-map (kbd "z") 'helm-mini)

(define-key top-mode-map (kbd ";") 'semi-colon-map)
(define-key top-mode-map (kbd "z") 'helm-mini)

(define-key messages-buffer-mode-map (kbd ";") 'semi-colon-map)
(define-key messages-buffer-mode-map (kbd "z") 'helm-mini)

;; Modes
(define-minor-mode attic-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " attic" 'attic-minor-mode-map)

(defun attic-minibuffer-setup-hook ()
  (attic-minor-mode 0))

;; Other unset keys
(global-unset-key "\C-x\C-z")
(provide 'attic-keys)

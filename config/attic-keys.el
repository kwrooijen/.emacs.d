(define-prefix-command 'semi-colon-map)
(define-prefix-command 'attic-make-map)

(defun attic-key(key function)
  (define-key attic-mode-map (kbd key) function))

(global-set-key (kbd "C-M-]") 'attic-mode)
(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a))) '(

("C-c C-o" switch-to-minibuffer)
("C-;" semi-colon-map)
("C-x C-1" delete-other-windows)
("C-x C-2" split-window-below)
("C-x C-3" split-window-right)

;; ;; Meta keys
("M-N" mc/mark-next-like-this)
("M-P" mc/mark-previous-like-this)
;("<return>" newline mc/keymap)
("M-C" capitalize-previous-word)
("M-i" tab-to-tab-stop-line-or-region)
("M-I" (lambda() (interactive) (tab-to-tab-stop-line-or-region t)))
("M-0" attic-sauron-toggle)
("M-9" attic-neotree-toggle)
("M-y" (lambda() (interactive)
         (if (or (equal last-command 'yank) (equal last-command 'yank-pop))
             (yank-pop)
           (helm-show-kill-ring))))))

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
        '((semi-colon-map (("r" rgrep)
                           ("g" magit-status)
                           ("k" kill-buffer)
                           ("n" sauron-select-last-event)
                           ("p" escreen-goto-prev-screen)
                           ("x" helm-M-x)
                           (";" escreen-goto-last-screen)
                           ("d" (lambda() (interactive) (helm-swoop :$query "")))
                           ("M-d" helm-swoop)
                           ("a" async-shell-command)
                           ("s" shell-command)
                           ("f" helm-ls-git-ls)
                           ("e" eww)
                           ("b" helm-bookmarks)
                           ("[" winner-undo)
                           ("]" winner-redo)
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
                           ("'" helm-org-capture-templates)
                           ("q" attic-make-map)))
          (attic-make-map (("p" (lambda() (interactive) (run-make "stop"    "[Make Stop]")))
                           ("r" (lambda() (interactive) (run-make "restart" "[Make Restart]")))
                           ("s" (lambda() (interactive) (run-make "start"   "[Make Start]")))
                           ("t" (lambda() (interactive) (run-make "test"    "[Make Test]")))
                           ("o" (lambda() (interactive) (run-make "go"      "[Make Go]")))
                           ("q" (lambda() (interactive) (run-make ""        "[Make]")))
                           ("c" run-make-input)))))

(if window-system
    (progn
      (define-key attic-mode-map (kbd "C-S-V") 'x-clipboard-yank)
      (define-key attic-mode-map (kbd "C-S-C") 'clipboard-kill-ring-save)))

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
(define-key c-mode-base-map (kbd "C-/") 'attic/comment))

;; Package Menu mode
(define-key package-menu-mode-map (kbd ";") 'semi-colon-map)

(require 'doc-view)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)

(define-key doc-view-mode-map (kbd ";") 'semi-colon-map)
(define-key doc-view-mode-map (kbd "z") 'helm-buffers-list)

(define-key help-mode-map (kbd ";") 'semi-colon-map)
(define-key help-mode-map (kbd "z") 'helm-buffers-list)

(define-key messages-buffer-mode-map (kbd ";") 'semi-colon-map)
(define-key messages-buffer-mode-map (kbd "z") 'helm-buffers-list)

(defun attic-minibuffer-setup-hook ()
  (attic-mode 0))

;; Other unset keys
(global-unset-key "\C-x\C-z")
(provide 'attic-keys)

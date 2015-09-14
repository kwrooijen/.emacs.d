(define-prefix-command 'semi-colon-map)
(define-prefix-command 'attic-make-map)

(defun attic-key(key function)
  (define-key attic-mode-map (kbd key) function))

(global-set-key (kbd "C-M-]") 'attic-mode)
(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a))) '(

("C-c C-o" switch-to-minibuffer)
("C-;" semi-colon-map)

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

(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "TAB")
  (if (active-minibuffer-window)
      (minibuffer-complete)
    (indent-for-tab-command)))

;; C Keys
(defun c-keys-hook ()
(define-key c-mode-base-map (kbd "C-/") 'my-comment))

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

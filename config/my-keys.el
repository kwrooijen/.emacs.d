(defvar attic-minor-mode-map (make-keymap) "attic-minor-mode keymap.")

; Unset Keys
(dolist (key '("\C-z"))
  (global-unset-key key))

;; Control Keys
(define-key attic-minor-mode-map (kbd "C-u") 'repeat)
(define-key attic-minor-mode-map (kbd "C-.") 'helm-resume)
(define-key attic-minor-mode-map (kbd "C-q") 'backward-delete-char)
(define-key attic-minor-mode-map (kbd "C--") 'undo)
(define-key attic-minor-mode-map (kbd "C-;") 'pop-to-mark-command)

;; Control Prefix Keys
(define-key attic-minor-mode-map (kbd "C-M-_")	 'redo)
(define-key attic-minor-mode-map (kbd "C-M--")	 'redo)
(define-key attic-minor-mode-map (kbd "C-x M-t") 'transpose-paragraphs)
(define-key attic-minor-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key attic-minor-mode-map (kbd "C-c C-v") 'undo-tree-visualize)

;; Meta Keys
(global-set-key (kbd "M-g") 'god-mode-enable)
(define-key attic-minor-mode-map (kbd "M-q") 'backward-kill-word)
(define-key attic-minor-mode-map (kbd "M-o") 'er/expand-region)
(define-key attic-minor-mode-map (kbd "M-x") 'execute-extended-command)
(define-key attic-minor-mode-map (kbd "M-+") 'align-regexp)
(define-key attic-minor-mode-map (kbd "M-@") 'er/expand-region)
(define-key attic-minor-mode-map (kbd "M-t") 'transpose-words)
;(define-key attic-minor-mode-map (kbd "M-T") 'ghc-insert-template)
(define-key attic-minor-mode-map (kbd "M-E") 'mc/edit-lines)
(define-key attic-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key attic-minor-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key attic-minor-mode-map (kbd "M-P")'mc/mark-previous-like-this)

(define-key attic-minor-mode-map (kbd "M-N") 'mc/mark-next-like-this)
(define-key attic-minor-mode-map (kbd "M-*") 'mc/mark-all-like-this)
(define-key attic-minor-mode-map (kbd "M-j") (lambda()
                                               (interactive)
                                               (join-line -1)))

;; J Prefix Keys
(define-prefix-command 'c-j-prefix)
(define-key attic-minor-mode-map (kbd "C-j")     'c-j-prefix)
(define-key attic-minor-mode-map (kbd "C-j C-o") 'hoogle-search)
(define-key attic-minor-mode-map (kbd "C-j C-t") 'transpose-paragraphs)
(define-key attic-minor-mode-map (kbd "C-j C-n") 'iy-go-up-to-char)
(define-key attic-minor-mode-map (kbd "C-j C-p") 'iy-go-to-char-backward)
(define-key attic-minor-mode-map (kbd "C-j C-j") 'helm-buffers-list)
(define-key attic-minor-mode-map (kbd "C-j C-i") 'ace-jump-mode)
(define-key attic-minor-mode-map (kbd "C-j C-f") 'textmate-goto-file)
(define-key attic-minor-mode-map (kbd "C-j C-c") 'comment-or-uncomment-region)
(define-key attic-minor-mode-map (kbd "C-j C-v") 'org-cycle-agenda-files)
(define-key attic-minor-mode-map (kbd "C-j C-m") 'magit-status)
(define-key attic-minor-mode-map (kbd "C-j C-q") 'kmacro-start-macro)
(define-key attic-minor-mode-map (kbd "C-j C-e")
    'kmacro-end-or-call-macro-repeat)
(define-key attic-minor-mode-map (kbd "C-j C-w") 'load-attic-workgroups)
(define-key attic-minor-mode-map (kbd "C-j C-c") 'comment-or-uncomment-region)
(define-key attic-minor-mode-map (kbd "C-j C-f") 'helm-ls-git-ls)
(define-key attic-minor-mode-map (kbd "C-j C-l") 'toggle-linum-gutter)

(define-key attic-minor-mode-map (kbd "C-j C-g C-g") 'helm-do-grep)
(define-key attic-minor-mode-map (kbd "C-j C-g C-s") 'helm-swoop)
(define-key attic-minor-mode-map (kbd "C-j C-g C-m") 'helm-multi-swoop)
(define-key attic-minor-mode-map (kbd "C-j C-g C-r") (lambda () (interactive)
    (let ((current-prefix-arg '(1)))
      (call-interactively 'helm-do-grep))))

(define-key attic-minor-mode-map (kbd "C-j C-r") (key-binding (kbd "\C-xr")))

(define-key attic-minor-mode-map (kbd "C-j C-1") 'wg-switch-to-index-0)
(define-key attic-minor-mode-map (kbd "C-j C-2") 'wg-switch-to-index-1)
(define-key attic-minor-mode-map (kbd "C-j C-3") 'wg-switch-to-index-2)
(define-key attic-minor-mode-map (kbd "C-j C-4") 'wg-switch-to-index-3)
(define-key attic-minor-mode-map (kbd "C-j C-5") 'wg-switch-to-index-4)
(define-key attic-minor-mode-map (kbd "C-j C-6") 'wg-switch-to-index-5)
(define-key attic-minor-mode-map (kbd "C-j C-7") 'wg-switch-to-index-6)
(define-key attic-minor-mode-map (kbd "C-j C-8") 'wg-switch-to-index-7)
(define-key attic-minor-mode-map (kbd "C-j C-9") 'wg-switch-to-index-8)
(define-key attic-minor-mode-map (kbd "C-j C-0") 'wg-switch-to-index-9)

(define-key attic-minor-mode-map (kbd "C-j 1") 'wg-switch-to-index-0)
(define-key attic-minor-mode-map (kbd "C-j 2") 'wg-switch-to-index-1)
(define-key attic-minor-mode-map (kbd "C-j 3") 'wg-switch-to-index-2)
(define-key attic-minor-mode-map (kbd "C-j 4") 'wg-switch-to-index-3)
(define-key attic-minor-mode-map (kbd "C-j 5") 'wg-switch-to-index-4)
(define-key attic-minor-mode-map (kbd "C-j 6") 'wg-switch-to-index-5)
(define-key attic-minor-mode-map (kbd "C-j 7") 'wg-switch-to-index-6)
(define-key attic-minor-mode-map (kbd "C-j 8") 'wg-switch-to-index-7)
(define-key attic-minor-mode-map (kbd "C-j 9") 'wg-switch-to-index-8)
(define-key attic-minor-mode-map (kbd "C-j 0") 'wg-switch-to-index-9)

;; Erlang Keys
(defun erlang-keys-hook ()
  (local-set-key (kbd "M-?") 'erlang-get-error)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "M-q") 'backward-kill-word)
)

;; JS Keys
(defun js2-keys-hook ()
  (define-key js2-mode-map (kbd "M-n") 'js2-next-error)
  (define-key js2-mode-map (kbd "M-p") 'js2-previous-error)
  (define-key js2-mode-map (kbd "M-?") 'js2-display-error-list)

  (define-key attic-minor-mode-map (kbd "C-j C-s C-s") 'slime-eval-buffer)
  (define-key attic-minor-mode-map (kbd "C-j C-s C-r") 'slime-eval-region)
  (define-key attic-minor-mode-map (kbd "C-j C-s C-c") 'slime-connect)
  (define-key attic-minor-mode-map (kbd "C-j C-s C-d") 'slime-disconnect)
  (define-key attic-minor-mode-map (kbd "C-j C-s C-e") 'slime-js-select-remote)

  (define-key js2-mode-map (kbd "C-j C-h C-h") 'hs-toggle-hiding)
  (define-key js2-mode-map (kbd "C-j C-h C-a") 'hs-hide-all)
  (define-key js2-mode-map (kbd "C-j C-h C-s") 'hs-show-all)

  (define-key attic-minor-mode-map (kbd "C-j C-h C-h") 'hs-toggle-hiding)
  (define-key attic-minor-mode-map (kbd "C-j C-h C-a") 'hs-hide-all)
  (define-key attic-minor-mode-map (kbd "C-j C-h C-s") 'hs-show-all)
)

;; Helm keys
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-f") 'helm-select-action)
(define-key helm-map (kbd "M-b") 'nil)
(define-key helm-map (kbd "C-f") 'nil)
(define-key helm-map (kbd "C-b") 'nil)
(define-key helm-map (kbd "C-c C-a")
  'helm-ff-run-toggle-basename)
(define-key helm-buffer-map (kbd "C-a")
  'helm-buffers-toggle-show-hidden-buffers)
(define-prefix-command 'm-g-prefix)
(define-key helm-map (kbd "M-g M-g") 'helm-keyboard-quit)

;; God mode

(define-key god-local-mode-map (kbd "i")   'god-mode-disable)
(define-key god-local-mode-map (kbd "M-g") 'keyboard-escape-quit-mc)
(define-key god-local-mode-map (kbd "C-l") 'forward-to-word)
(define-key god-local-mode-map (kbd "C-h") 'backward-to-word)
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

; Ruby
;; Execution
(define-key attic-minor-mode-map (kbd "C-j C-r C-r") 'rinari-rake)
(define-key attic-minor-mode-map (kbd "C-j C-r C-i") 'rinari-console)
(define-key attic-minor-mode-map (kbd "C-j C-r C-s") 'rinari-web-server)
(define-key attic-minor-mode-map (kbd "C-j C-r C-k") 'rinari-test)
(define-key attic-minor-mode-map (kbd "C-j C-r C-q") 'rinari-sql)
;; Navigation
(define-key attic-minor-mode-map (kbd "C-j C-r C-c") 'helm-rails-controllers)
(define-key attic-minor-mode-map (kbd "C-j C-r C-m") 'helm-rails-models)
(define-key attic-minor-mode-map (kbd "C-j C-r C-v") 'helm-rails-views)
(define-key attic-minor-mode-map (kbd "C-j C-r C-h") 'helm-rails-helpers)
(define-key attic-minor-mode-map (kbd "C-j C-r C-j") 'helm-rails-javascripts)
(define-key attic-minor-mode-map (kbd "C-j C-r C-t") 'helm-rails-specs)
(define-key attic-minor-mode-map (kbd "C-j C-r C-y") 'helm-rails-stylesheets)
(define-key attic-minor-mode-map (kbd "C-j C-r C-l") 'helm-rails-libs)

(define-key attic-minor-mode-map (kbd "C-j C-r c") 'helm-rails-grep-controllers)
(define-key attic-minor-mode-map (kbd "C-j C-r m") 'helm-rails-grep-models)
(define-key attic-minor-mode-map (kbd "C-j C-r v") 'helm-rails-grep-views)
(define-key attic-minor-mode-map (kbd "C-j C-r h") 'helm-rails-grep-helpers)
(define-key attic-minor-mode-map (kbd "C-j C-r j") 'helm-rails-grep-javascripts)
(define-key attic-minor-mode-map (kbd "C-j C-r t") 'helm-rails-grep-specs)
(define-key attic-minor-mode-map (kbd "C-j C-r y") 'helm-rails-grep-stylesheets)
(define-key attic-minor-mode-map (kbd "C-j C-r l") 'helm-rails-grep-libs)

; Todo: add these
;(define-key attic-minor-mode-map (kbd "C-j C-r C-w") 'helm-rails-worker)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-e") 'helm-rails-environment)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-p") 'helm-rails-plugin)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-g") 'helm-rails-migration)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-f") 'helm-rails-cofiguration)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-x") 'helm-rails-fixture)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-u") 'helm-rails-public)
;(define-key attic-minor-mode-map (kbd "C-j C-r C-z") 'helm-rails-script)

;(define-key attic-minor-mode-map (kbd "C-j C-r w") 'helm-rails-grep-worker)
;(define-key attic-minor-mode-map (kbd "C-j C-r e") 'helm-rails-grep-environment)
;(define-key attic-minor-mode-map (kbd "C-j C-r p") 'helm-rails-grep-plugin)
;(define-key attic-minor-mode-map (kbd "C-j C-r g") 'helm-rails-grep-migration)
;(define-key attic-minor-mode-map (kbd "C-j C-r f") 'helm-rails-grep-cofiguration)
;(define-key attic-minor-mode-map (kbd "C-j C-r x") 'helm-rails-grep-fixture)
;(define-key attic-minor-mode-map (kbd "C-j C-r u") 'helm-rails-grep-public)
;(define-key attic-minor-mode-map (kbd "C-j C-r z") 'helm-rails-grep-script)


; Global Keys
(global-set-key [f7] 'get-current-buffer-major-mode)
(global-set-key [f8] 'copy-to-clipboard)
(global-set-key [f9] 'paste-from-clipboard)

; Web Mode
(define-key web-mode-map (kbd "C-z") 'zencoding-expand-yas)

; Define mode
(define-minor-mode attic-minor-mode
"A minor mode so that my key settings override annoying major modes."

t " attic" 'attic-minor-mode-map)
(defun attic-minibuffer-setup-hook ()
	(attic-minor-mode 0))

(provide 'my-keys)

(define-prefix-command 'attic-make-map)

(defun attic-key(key function)
  (define-key attic-mode-map (kbd key) function))

(global-set-key (kbd "C-M-]") 'attic-mode)
(mapcar (lambda(a) (attic-key (nth 0 a) (nth 1 a)))
        '(("C-c C-o" switch-to-minibuffer)
          ("C-c C-=" increment-decimal)
          ("C-c C--" decrement-decimal)
          ("C-;" attic-semi-colon/body)
          ("C-'" helm-M-x)
          ("C-z" helm-buffers-list)
          ("C-j" iy-go-to-char)
          ("M-j" iy-go-to-char-backward)
          ("C-q" backward-delete-char)
          ("M-q" backward-kill-word)
          ("C-S-V" x-clipboard-yank)
          ("C-S-C" clipboard-kill-ring-save)
          ("C-M-q" backward-kill-sexp)
          ("C-x C-f" helm-find-files)
          ("C-x C-1" delete-other-windows)
          ("C-x C-2" split-window-below)
          ("C-x C-3" split-window-right)
          ("C-x C-4" delete-window)
          ("C-x C-8" fill-paragraph)
          ("C-x C-b" helm-buffers-list)
          ("C-x C-k" kill-this-buffer)
          ("C-c C-p" copy-line-up)
          ("C-c C-n" copy-line-down)
          ;; Meta keys
          ("M-+" align-regexp)
          ("M-C" capitalize-previous-word)
          ("M-i" tab-to-tab-stop-line-or-region)
          ("M-I" tab-to-tab-stop-line-or-region-backward)
          ("M-y" yank-pop-or-kill-ring)))

(define-key attic-mode-map
  (kbd "C-;")
  (defhydra attic-semi-colon (:color blue :columns 7)
    "Attic"
    ("'" helm-org-capture-templates nil)
    ("." create-tags "Tag")
    ("0" elscreen-goto-0 nil)
    ("1" elscreen-goto-1 nil)
    ("2" elscreen-goto-2 nil)
    ("3" elscreen-goto-3 nil)
    ("4" elscreen-goto-4 nil)
    ("5" elscreen-goto-5 nil)
    ("6" elscreen-goto-6 nil)
    ("7" elscreen-goto-7 nil)
    ("8" elscreen-goto-8 nil)
    ("9" elscreen-goto-9 nil)
    (";" elscreen-toggle nil)
    ("<SPC>" pop-to-mark-command "Pop Mark")
    ("M-d" helm-swoop nil)
    ("[" winner-undo nil :color red)
    ("]" winner-redo nil :color red)
    ("s" async-shell-command "ASync Shell")
    ("b" helm-bookmarks "Bookmarks")
    ("d" (lambda() (interactive) (helm-swoop :$query "")) "Swoop")
    ("e" eww "Eww")
    ("f" attic-file/body "File")
    ("g" magit-status "Magit")
    ("i" remove-newline-space nil)
    ("j" attic-lock "Lock")
    ("k" kill-buffer "Kill")
    ("m" attic-emms/body "EMMS")
    ("q" attic-make/body "Make")
    ("a" attic-projectile/body "Projectile")
    ("x" helm-M-x "M-x")
    ("r" rgrep "RGrep")
    ("t" transpose-mark nil)
    ("c" attic-macro/body "Macro")))

(defhydra attic-macro (:color blue :columns 4)
  "Attic Macro"
  ("s"    kmacro-start-macro "kmacro-start-macro")
  ("k"    kmacro-end-or-call-macro-repeat "kmacro-end-or-call-macro-repeat")
  ("r"    apply-macro-to-region-lines "apply-macro-to-region-lines")
  ("q"    kbd-macro-query "kbd-macro-query")
  ("C-n"  kmacro-cycle-ring-next "kmacro-cycle-ring-next")
  ("C-p"  kmacro-cycle-ring-previous "kmacro-cycle-ring-previous")
  ("C-v"  kmacro-view-macro-repeat "kmacro-view-macro-repeat")
  ("C-d"  kmacro-delete-ring-head "kmacro-delete-ring-head")
  ("C-t"  kmacro-swap-ring "kmacro-swap-ring")
  ("C-l"  kmacro-call-ring-2nd-repeat "kmacro-call-ring-2nd-repeat")
  ("C-f"  kmacro-set-format "kmacro-set-format")
  ("C-c"  kmacro-set-counter "kmacro-set-counter")
  ("C-i"  kmacro-insert-counter "kmacro-insert-counter")
  ("C-a"  kmacro-add-counter "kmacro-add-counter")
  ("C-e"  kmacro-edit-macro-repeat "kmacro-edit-macro-repeat")
  ("r"    kmacro-edit-macro "kmacro-edit-macro")
  ("e"    edit-kbd-macro "edit-kbd-macro")
  ("l"    kmacro-edit-lossage "kmacro-edit-lossage")
  (" "    kmacro-step-edit-macro "kmacro-step-edit-macro")
  ("b"    kmacro-bind-to-key "kmacro-bind-to-key")
  ("n"    kmacro-name-last-macro "kmacro-name-last-macro")
  ("x"    kmacro-to-register "kmacro-to-register"))

(defhydra attic-make (:color blue :columns 4)
  "[Make]"
  ("p" attic/make-stop    "Stop")
  ("r" attic/make-restart "Restart")
  ("s" attic/make-start   "Start")
  ("t" attic/make-test    "Test")
  ("o" attic/make-go      "Go")
  ("q" attic/make-default "Make")
  ("c" attic/make-custom  "Custom"))

(defhydra attic-file (:color blue :columns 4)
  "Attic File"
  ("f" helm-find-files "helm-find-files")
  ("d" helm-ls-git-ls "helm-ls-git-ls")
  ("b" helm-bookmarks "helm-bookmarks")
  ("j" helm-buffers-list "helm-buffers-list")
  ("h" previous-buffer "previous-buffer" :color red)
  ("l" next-buffer "next-buffer" :color red))

(defhydra attic-projectile (:color blue :columns 4)
  "Helm Projectile"
  ("a" helm-projectile "helm-projectile")
  ("b" helm-projectile-switch-to-buffer "helm-projectile-switch-to-buffer")
  ("c" helm-projectile-ack "helm-projectile-ack")
  ("d" helm-projectile-find-dir "helm-projectile-find-dir")
  ("e" helm-projectile-switch-to-eshell "helm-projectile-switch-to-eshell")
  ("f" helm-projectile-find-file "helm-projectile-find-file")
  ("g" helm-projectile-grep "helm-projectile-grep")
  ("i" helm-projectile-find-files-eshell-command-on-file-action "helm-projectile-find-files-eshell-command-on-file-action")
  ("k" helm-projectile-find-file-in-known-projects "helm-projectile-find-file-in-known-projects")
  ("o" helm-projectile-find-other-file "helm-projectile-find-other-file")
  ("r" helm-projectile-recentf "helm-projectile-recentf")
  ("s" helm-projectile-switch-project "helm-projectile-switch-project")
  ("t" helm-projectile-ff-etags-select-action "helm-projectile-ff-etags-select-action")
  ("w" helm-projectile-find-file-dwim "helm-projectile-find-file-dwim"))

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
(define-key package-menu-mode-map (kbd ";") 'attic-semi-colon/body)

(require 'doc-view)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)

(define-key doc-view-mode-map (kbd ";") 'attic-semi-colon/body)
(define-key doc-view-mode-map (kbd "z") 'helm-buffers-list)

(define-key help-mode-map (kbd ";") 'attic-semi-colon/body)
(define-key help-mode-map (kbd "z") 'helm-buffers-list)

(define-key messages-buffer-mode-map (kbd ";") 'attic-semi-colon/body)
(define-key messages-buffer-mode-map (kbd "z") 'helm-buffers-list)

(defun attic-minibuffer-setup-hook ()
  (attic-mode 0))

;; Other unset keys
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-z")
(provide 'attic-keys)

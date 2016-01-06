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
    ("<tab>" buffer-toggle "Buffer Toggle")
    ("`" elscreen-toggle "Elscreen Toggle")
    ("RET" nil)
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
    ("<SPC>" pop-to-mark-command "Pop Mark" :color red)
    ("M-d" helm-swoop nil)
    ("s" async-shell-command "ASync Shell")
    ("b" helm-bookmarks "Bookmarks")
    ("d" (lambda() (interactive) (helm-swoop :$query "")) "Swoop")
    ("e" eww "Eww")
    ("f" attic-file/body "File")
    ("w" attic-window/body "Window")
    ("n" attic-mc/body "Multi Cursor")
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

(defhydra attic-window (:color red :columns 4)
  "Attic Window"
  ("q" nil "Quit" :color blue)
  ("RET" nil :color blue)
  ("[" winner-undo "Winner Undo")
  ("]" winner-redo "Winner Redo")
  ("h" shrink-window-horizontally "Shrink Horizontally")
  ("l" enlarge-window-horizontally "Enlarge Horizontally")
  ("j" enlarge-window "Shrink")
  ("k" shrink-window "Enlarge"))

(defhydra attic-mc (:color red :columns 4)
  "Attic MC"
  ("q" nil "Quit")
  ("e" attic-mc-edit/body "Edit" :color blue)
  ("l" attic-mc-like-this/body "Like this" :color blue)
  ("r" attic-mc-region/body "Region" :color blue)
  ("m" attic-mc-mmlte/body "Mmlte" :color blue)
  ("w" attic-mc-word/body "Word" :color blue)
  ("s" attic-mc-symbol/body "Symbol" :color blue)
  ("d" attic-mc-defun/body "Defun" :color blue)
  ("o" mc/mark-pop "mark-pop")
  ("@" mc/mark-all-dwim "mark-all-dwim")
  ("k" mc/keyboard-quit "keyboard-quit")
  ("f" mc/cycle-forward "cycle-forward")
  ("b" mc/cycle-backward "cycle-backward")
  ("va" mc/vertical-align "vertical-align")
  ("vs" mc/vertical-align-with-space "vertical-align-with-space")
  ;; ("n" mc/mark-next-lines "mark-next-lines")
  ;; ("p" mc/mark-previous-lines "mark-previous-lines")
  ;; ("" mc/mark-sgml-tag-pair "mark-sgml-tag-pair")
  ("@" mc/mark-all-like-this "mark-all-like-this")
  ("n" mc/mark-next-like-this "mark-next-like-this")
  ("p" mc/mark-previous-like-this "mark-previous-like-this")
  ("N" mc/skip-to-next-like-this "skip-to-next-like-this")
  ("P" mc/skip-to-previous-like-this "skip-to-previous-like-this"))

(defhydra attic-mc-edit (:color red :columns 4)
  ("q" nil "Quit")
  ("<tab>" attic-mc/body "Back" :color blue)
  ("l" mc/edit-lines "edit-lines")
  ("b" mc/edit-beginnings-of-lines "edit-beginnings-of-lines")
  ("e" mc/edit-ends-of-lines "edit-ends-of-lines")
  ("i" mc/insert-numbers "insert-numbers")
  )
(defhydra attic-mc-like-this (:color red :columns 4)
  ("q" nil "Quit")
  ("<tab>" attic-mc/body "Back" :color blue)
  ("un" mc/unmark-next-like-this "unmark-next-like-this")
  ("d@" mc/mark-all-like-this-dwim "mark-all-like-this-dwim")
  ("up" mc/unmark-previous-like-this "unmark-previous-like-this")
  ("m" mc/mark-more-like-this-extended "mark-more-like-this-extended"))

(defhydra attic-mc-region (:color red :columns 4)
  ("q" nil "Quit")
  ("<tab>" attic-mc/body "Back" :color blue)
  ("s" mc/sort-regions "sort-regions")
  ("r" mc/reverse-regions "reverse-regions")
  ("@" mc/mark-all-in-region "mark-all-in-region")
  ("x" mc/mark-all-in-region-regexp "mark-all-in-region-regexp"))

(defhydra attic-mc-mmlte (:color red :columns 4)
  ("q" nil "Quit")
  ("<tab>" attic-mc/body "Back" :color blue)
  ("k" mc/mmlte--up "mmlte--up")
  ("h" mc/mmlte--left "mmlte--left")
  ("j" mc/mmlte--down "mmlte--down")
  ("l" mc/mmlte--right "mmlte--right"))

(defhydra attic-mc-word (:color red :columns 4)
  ("q" nil "Quit")
  ("<tab>" attic-mc/body "Back" :color blue)
  ("n" mc/mark-next-word-like-this "mark-next-word-like-this")
  ("@" mc/mark-all-words-like-this "mark-all-words-like-this")
  ("p" mc/mark-previous-word-like-this "mark-previous-word-like-this"))

(defhydra attic-mc-symbol (:color red :columns 4)
  ("q" nil "Quit")
  ("<tab>" attic-mc/body "Back" :color blue)
  ("n" mc/mark-next-symbol-like-this "mark-next-symbol-like-this")
  ("p" mc/mark-previous-symbol-like-this "mark-previous-symbol-like-this")
  ("@" mc/mark-all-symbols-like-this "mark-all-symbols-like-this"))

(defhydra attic-mc-defun (:color red :columns 4)
  ("t" mc/mark-all-like-this-in-defun "mark-all-like-this-in-defun")
  ("s" mc/mark-all-symbols-like-this-in-defun "mark-all-symbols-like-this-in-defun")
  ("w" mc/mark-all-words-like-this-in-defun "mark-all-words-like-this-in-defun"))

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
  ("s" helm-projectile-find-file "helm-projectile-find-file")
  ("g" helm-projectile-grep "helm-projectile-grep")
  ("i" helm-projectile-find-files-eshell-command-on-file-action "helm-projectile-find-files-eshell-command-on-file-action")
  ("k" helm-projectile-find-file-in-known-projects "helm-projectile-find-file-in-known-projects")
  ("o" helm-projectile-find-other-file "helm-projectile-find-other-file")
  ("r" helm-projectile-recentf "helm-projectile-recentf")
  ("f" helm-projectile-switch-project "helm-projectile-switch-project")
  ("t" helm-projectile-ff-etags-select-action "helm-projectile-ff-etags-select-action")
  ("w" helm-projectile-find-file-dwim "helm-projectile-find-file-dwim"))

;; Make keys
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)
(define-key isearch-mode-map (kbd "M-g") 'isearch-abort)
(define-key isearch-mode-map (kbd "TAB") 'isearch-exit)

;; Other Keys
(global-set-key [f3] 'describe-key)
(global-set-key [f4] 'send-to-gist)
(global-set-key [f6] 'describe-mode)
(global-set-key [f7] 'get-current-buffer-major-mode)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
(global-set-key [f11] 'screenshot-frame)

;; C Keys
(defun c-keys-hook ()
  (define-key c-mode-base-map (kbd "C-/") 'attic/comment))

(require 'doc-view)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)


(defun macro-add-key (m)
  `(progn (define-key ,m (kbd ";") 'attic-semi-colon/body)))

(defmacro add-semi-colon-to-modes (&rest modes)
  (let ((forms (mapcar 'macro-add-key modes)))
    `(progn ,@forms)))

(add-semi-colon-to-modes messages-buffer-mode-map
                         help-mode-map
                         doc-view-mode-map
                         package-menu-mode-map
                         dired-mode-map
                         elfeed-show-mode-map
                         elfeed-search-mode-map
                         grep-mode-map
                         grep-mode-map
                         magit-status-mode-map
                         magit-revision-mode-map
                         twittering-mode-map
                         flyspell-mode-map
                         mu4e-main-mode-map
                         mu4e-headers-mode-map
                         mu4e-view-mode-map)

(defun attic-minibuffer-setup-hook ()
  (attic-mode 0))

;; Other unset keys
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-z")
(provide 'attic-keys)

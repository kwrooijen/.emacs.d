;;==============================================================================
;;== Notes
;;==============================================================================

;; For this module to work properly you need to have the package window-numbering
;; installed and activated. neotree tries to take focus when toggling / changing
;; directory and I use the window-numbering package to return to the previous
;; window.

;;==============================================================================
;;== Macros
;;==============================================================================

(defmacro neotree-root-hook (function-name)
  `(defadvice ,function-name (after ,function-name activate)
    (if neotree-active
        (set-neo-root-project))))

;;==============================================================================
;;== Keys
;;==============================================================================

(define-key neotree-mode-map (kbd "RET") 'neotree-enter)
(define-key neotree-mode-map (kbd "c s a") 'helm-bookmarks)
(define-key neotree-mode-map (kbd "z") 'helm-mini)
(define-key neotree-mode-map (kbd ";") 'semi-colon-map)

;;==============================================================================
;;== Options
;;==============================================================================

(setq neo-theme 'ascii)
(setq neo-window-width 30)
;; Made up options
(setq neotree-active nil)
(setq neotree-ignore-list
      '(erc-mode
        sauron-mode
        help-mode
        eww-mode
        doc-view-mode
        top-mode
        Custom-mode
        message-mode
        gnus-group-mode
        gnus-summary-mode
        gnus-article-mode
        twittering-mode
        alchemist-iex-mode
        shell-mode))

;;==============================================================================
;;== Advice
;;==============================================================================

(neotree-root-hook dired-find-file)
(neotree-root-hook select-window-1)
(neotree-root-hook select-window-2)
(neotree-root-hook select-window-3)
(neotree-root-hook select-window-4)
(neotree-root-hook select-window-5)
(neotree-root-hook select-window-6)
(neotree-root-hook select-window-7)
(neotree-root-hook select-window-8)
(neotree-root-hook select-window-9)
(neotree-root-hook select-window-0)
(neotree-root-hook magit-visit-item)
(neotree-root-hook neotree-enter)
(neotree-root-hook helm-find-files)
(neotree-root-hook helm-ls-git-ls)
(neotree-root-hook helm-mini)
(neotree-root-hook kill-this-buffer)
(neotree-root-hook ido-kill-buffer)

;;==============================================================================
;;== Functions
;;==============================================================================

(defface neotree-overlay-face
  '((t :background "#696969"))
  "" :group 'neotree)

(setq neotree-overlay nil)

(defun set-neo-root-project ()
  (interactive)
  (unless (or (member major-mode neotree-ignore-list)
              (is-tramp-mode)
              (equal (buffer-name) " *NeoTree*"))
    (let ((previous-window (window-numbering-get-number)))
      (neotree-dir (or (magit-get-top-dir) default-directory))
      (select-window-by-number previous-window)
      (neotree-find)
      (if neotree-overlay (delete-overlay neotree-overlay))
      (setq neotree-overlay (make-overlay (point) (progn (end-of-line) (point))))
      (overlay-put neotree-overlay 'face 'neotree-overlay-face)
      (select-window-by-number previous-window))))

(defun attic-neotree-toggle ()
  (interactive)
  (setq neotree-active (not (get-buffer-window " *NeoTree*")))
  (neotree-toggle)
  (if neotree-active
      (let ((previous-window (window-numbering-get-number)))
        (select-window-by-number (+ 1 previous-window))
        (set-neo-root-project))))

(provide 'attic-neotree)

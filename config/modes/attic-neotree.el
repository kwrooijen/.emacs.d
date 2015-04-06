;;==============================================================================
;;== Notes
;;==============================================================================

;; For this module to work properly you need to have the package window-numbering
;; installed and activated. neotree tries to take focus when toggling / changing
;; directory and I use the window-numbering package to return to the previous
;; window.

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
        shell-mode))

;;==============================================================================
;;== Advice
;;==============================================================================

(defadvice dired-find-file (after dired-find-file activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-1 (after select-window-1 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-2 (after select-window-2 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-3 (after select-window-3 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-4 (after select-window-4 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-5 (after select-window-5 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice magit-visit-item (after magit-visit-item activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice neotree-enter (after neotree-enter activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice helm-ls-git-ls (before helm-ls-git-ls activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice helm-find-files (before helm-find-files activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice helm-find-files (after helm-find-files activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice helm-ls-git-ls (before helm-ls-git-ls activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice helm-ls-git-ls (after helm-ls-git-ls activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice helm-mini (after helm-mini activate)
  (if neotree-active
      (set-neo-root-project)))

;;==============================================================================
;;== Functions
;;==============================================================================

(defface neotree-overlay-face
  '((t :background "#696969"))
  "" :group 'neotree)

(setq neotree-overlay nil)

(defun set-neo-root-project ()
  (interactive)
  (unless (member major-mode neotree-ignore-list)
    (let ((previous-window (window-numbering-get-number)))
      (unless (equal (buffer-name) " *NeoTree*")
        (neotree-dir (or (magit-get-top-dir) default-directory))
        (select-window-by-number previous-window)
        (neotree-find)
        (if neotree-overlay (delete-overlay neotree-overlay))
        (setq neotree-overlay (make-overlay (point) (progn (end-of-line) (point))))
        (overlay-put neotree-overlay 'face 'neotree-overlay-face)
        (select-window-by-number previous-window)))))

(defun attic-neotree-toggle ()
  (interactive)
  (setq neotree-active (not (get-buffer-window " *NeoTree*")))
  (let ((previous-window (window-numbering-get-number)))
    (neotree-toggle)
    (if neotree-active (select-window-by-number (+ 1 previous-window)))))

(provide 'attic-neotree)


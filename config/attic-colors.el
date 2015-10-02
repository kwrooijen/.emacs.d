(defun set-foreground-background (face foreground &optional background)
  (set-face-foreground face foreground)
  (if background (set-face-background face background)))

(defun sfb (l)
  (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

;; (load-theme 'sanityinc-tomorrow-night t)
(load-theme 'material t)

(set-face-attribute 'highlight-symbol-face nil :inherit 'default)

(set-face-attribute 'neo-file-link-face nil :inherit 'helm-ff-file)
(set-face-attribute 'neo-button-face nil :inherit 'helm-ff-directory)

(set-face-attribute 'linum nil :inherit 'default)
(set-face-background 'linum nil)

(set-face-attribute 'ac-candidate-face nil :inherit 'company-tooltip-common)
(set-face-attribute 'ac-completion-face nil :inherit 'company-preview-common)
(set-face-attribute 'ac-selection-face nil :inherit 'company-tooltip-common-selection)

(set-face-background 'fringe 'unspecified)
(require 'whitespace)
(sfb '(
       ;; (git-gutter+-added           "green" unspecified)
       ;; (git-gutter+-deleted         "red" unspecified)
       ;; (git-gutter+-modified        "magenta" unspecified)
       ;; (git-gutter+-unchanged       unspecified "#383838")
       (font-lock-string-face       "cadet blue" unspecified)
       (highlight-symbol-face       "#fff" "gray20" )
       (neo-file-link-face          unspecified unspecified unspecified)
       (neo-button-face             unspecified unspecified unspecified)
       (ac-completion-face           unspecified unspecified)
       ;; (hl-line                     unspecified "gray14")
       ;; (fringe                      unspecified "gray14")
       ))

(defun daytime ()
  (interactive)
  (load-theme 'sanityinc-tomorrow-day t)
  (set-face-background 'hl-line nil)
  (set-face-attribute 'highlight-symbol-face nil :inherit 'highlight))

(setq whitespace-style
      '(face tabs spaces trailing
             space-before-tab indentation
             space-after-tab space-mark tab-mark))

;; Modeline

(defun modeline-region-counter ()
  (if (region-active-p)
    (format "%sC|%sW|%sL "
            (- (region-end) (region-beginning))
            (count-words (region-beginning) (region-end))
            (count-lines (region-beginning) (region-end)))
    ""))

;; (setq attic-mode-line-format
;;       '(" " (:eval (concat "[" (number-to-string (escreen-get-current-screen-number)) "]")) " "
;;         (:eval erc-modified-channels-object)
;;         "%*" "_"
;;         mode-line-remote " "
;;         (:eval (modeline-region-counter))
;;         "%3lL:%2cC "
;;         (:eval (format-time-string "%-I:%M%p")) " | "
;;         mode-line-buffer-identification " | "
;;         mode-name " |"
;;         (vc-mode vc-mode) " | "
;;         battery-mode-line-string))

(defun set-theme-white ()
  (interactive)
  (fringe-mode 0)
  (setq-default global-font-lock-mode nil)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil)
  (set-face-background 'default "#fff")
  (set-face-foreground 'default "#000"))

(defun set-theme-black ()
  (interactive)
  (fringe-mode 0)
  (setq-default global-font-lock-mode nil)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil)
  (set-face-background 'default "#000")
  (set-face-foreground 'default "#fff"))

(defun set-theme-hackergreen ()
  (interactive)
  (fringe-mode 0)
  (setq-default global-font-lock-mode nil)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil)
  (global-font-lock-mode -1)
  (font-lock-mode -1)
  (set-face-background 'default "#000")
  (set-face-foreground 'default "#0DB804"))

(defun set-theme-default ()
  (interactive)
  (fringe-mode)
  (setq-default global-font-lock-mode t)
  (setq-default mode-line-format attic-mode-line-format)
  (setq mode-line-format attic-mode-line-format)
  (global-font-lock-mode t)
  (font-lock-mode t)
  (load-file "~/.emacs.d/config/attic-colors.el"))

(setq powerline-default-separator 'wave)
(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (powerline-selected-window-active))
                        (mode-line (if active (quote mode-line) (quote mode-line-inactive)))
                        (face1 (if active (quote powerline-active1) (quote powerline-inactive1)))
                        (face2 (if active (quote powerline-active2) (quote powerline-inactive2)))
                        (separator-left (intern (format "powerline-%s-%s"
                                                        (powerline-current-separator)
                                                        (car powerline-default-separator-dir))))
                        (separator-right (intern (format "powerline-%s-%s"
                                                         (powerline-current-separator)
                                                         (cdr powerline-default-separator-dir))))
                        (lhs (list (powerline-raw "%*" nil (quote l))
                                   (powerline-buffer-size nil (quote l))
                                   (powerline-buffer-id nil (quote l))
                                   (powerline-raw " ")
                                   (powerline-raw (modeline-region-counter))
                                   (funcall separator-left mode-line face1)
                                   (powerline-narrow face1 (quote l))
                                   (powerline-vc face1)))
                        (rhs (list (powerline-raw global-mode-string face1 (quote r))
                                   (powerline-raw "%4l" face1 (quote r))
                                   (powerline-raw ":" face1)
                                   (powerline-raw "%3c" face1 (quote r))
                                   (funcall separator-right face1 mode-line)
                                   (powerline-raw " ")
                                   (powerline-raw "%6p" nil (quote r))
                                   (format-time-string "%-I:%M%p")
                                   (powerline-raw " ")
                                   (powerline-hud face2 face1)))
                        (center (list (powerline-raw " " face1)
                                      (funcall separator-left face1 face2)
                                      (when (and (boundp (quote erc-track-minor-mode))
                                                 erc-track-minor-mode)
                                        (powerline-raw erc-modified-channels-object face2 (quote l)))
                                      (powerline-major-mode face2 (quote l))
                                      (powerline-process face2)
                                      (powerline-raw " " face2)
                                      (funcall separator-right face2 face1))))
                   (concat (powerline-render lhs)
                           (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                           (powerline-render center)
                           (powerline-fill face1 (powerline-width rhs))
                           (powerline-render rhs))))))

(provide 'attic-colors)

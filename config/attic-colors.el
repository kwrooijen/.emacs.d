(defun set-foreground-background (face foreground &optional background)
  (set-face-foreground face foreground)
  (if background (set-face-background face background)))

(defun sfb(l)
  (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

;; (load-theme 'sanityinc-tomorrow-night t)

(set-face-attribute 'magit-diff-none nil :inherit nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)

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
    (git-gutter+-added          "green" unspecified)
    (git-gutter+-deleted        "red" unspecified)
    (git-gutter+-modified       "magenta" unspecified)
    (git-gutter+-unchanged  nil "#383838")
    (magit-diff-add             "black" "green")
    (magit-diff-del             "white" "red")
    (font-lock-string-face      "cadet blue" unspecified)
    (highlight-symbol-face      "#fff" "gray20" )
    (neo-file-link-face         unspecified unspecified unspecified)
    (neo-button-face            unspecified unspecified unspecified)
    (whitespace-empty            "gray16" "#1d1f21")
    (whitespace-hspace           "gray16" "#1d1f21")
    (whitespace-indentation      "gray16" "#1d1f21")
    (whitespace-line             "gray16" "#1d1f21")
    (whitespace-newline          "gray16" "#1d1f21")
    (whitespace-space            "gray16" "#1d1f21")
    (whitespace-space-after-tab  "gray16" "#1d1f21")
    (whitespace-space-before-tab "gray16" "#1d1f21")
    (whitespace-tab              "gray16" "#1d1f21")
    (whitespace-trailing         "gray16" "#1d1f21")
    (ac-completion-face           unspecified unspecified)
    ;; (hl-line                     unspecified "gray14")
    (fringe                      unspecified "gray14")
    (magit-item-highlight       nil "dim grey" unspecified)))

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

(defun god-mode-bar ()
  (if (member major-mode god-exempt-major-modes)
      ""
    (if mark-active
        (propertize "[VISUAL]" 'face 'bg:erc-color-face8)
      (if (and (boundp 'god-local-mode) god-local-mode)
          (propertize "[NORMAL]" 'face 'font-lock-constant-face)
        (propertize "[INSERT]" 'face 'bg:erc-color-face3)))))

(setq attic-mode-line-format
      '(" " (:eval (concat "[" (number-to-string (escreen-get-current-screen-number)) "]")) " "
        (:eval (butlast (cdr (gnus-mst-notify-modeline-form)))) " "
        (:eval erc-modified-channels-object)
        "%*" "_"
        mode-line-remote " "
        (:eval (modeline-region-counter))
        "%3lL:%2cC "
        (:eval (format-time-string "%-I:%M%p")) " | "
        mode-line-buffer-identification " | "
        mode-name " |"
        (vc-mode vc-mode) " | "
        battery-mode-line-string " | "
        (:eval (god-mode-bar))))

(provide 'attic-colors)

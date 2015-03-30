(defun set-foreground-background (face foreground &optional background)
    (set-face-foreground face foreground)
    (if background (set-face-background face background)))

(defun sfb(l)
    (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

(load-theme 'tango)
(sfb '(
    (git-gutter+-added          "green" "#383838")
    (git-gutter+-deleted        "red" "#383838")
    (git-gutter+-modified       "magenta" "#383838")
    (git-gutter+-unchanged  nil "#383838")
    (magit-diff-add             "black" "green")
    (magit-diff-del             "white" "red")
    (helm-selection             "white" "royalblue")
    (helm-ff-directory          "deep sky blue" unspecified)
    (company-scrollbar-bg   nil "gainsboro")
    (company-template-field     "black" "gainsboro")
    (company-tooltip            "black" "gainsboro")
    (font-lock-string-face      "cadet blue" unspecified)
    (mode-line                  "black" "grey" unspecified)
    (magit-item-highlight       nil "dim grey" unspecified)))

(set-face-attribute 'magit-diff-none nil :inherit nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)
(load-theme 'sanityinc-tomorrow-night t)

(if window-system
    (progn)
  (progn))

(provide 'my-colors)

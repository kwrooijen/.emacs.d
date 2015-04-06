(defun set-foreground-background (face foreground &optional background)
  (set-face-foreground face foreground)
  (if background (set-face-background face background)))

(defun sfb(l)
  (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

(load-theme 'sanityinc-tomorrow-night t)

(set-face-attribute 'magit-diff-none nil :inherit nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)

(set-face-attribute 'highlight-symbol-face nil :inherit 'default)

(set-face-attribute 'neo-file-link-face nil :inherit 'helm-ff-file)
(set-face-attribute 'neo-button-face nil :inherit 'helm-ff-directory)

(set-face-attribute 'linum nil :inherit 'default)
(set-face-background 'linum nil)

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
    (hl-line         unspecified "gray14")
    (fringe         unspecified "gray14")
    (magit-item-highlight       nil "dim grey" unspecified)))

(setq whitespace-style
      '(face tabs spaces trailing
             space-before-tab indentation
             space-after-tab space-mark tab-mark))
(provide 'my-colors)


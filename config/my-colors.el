; Enable my powerline Theme
(my-powerline-theme)

(defun set-foreground-background (face foreground &optional background)
    (set-face-foreground face foreground)
    (if background (set-face-background face background)))

(defun sfb(l)
    (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

(sfb '(
    (git-gutter+-added "green" "#383838" )
    (git-gutter+-deleted "red" "#383838" )
    (git-gutter+-modified "magenta" "#383838" )
    (git-gutter+-unchanged nil "#383838")
    (helm-selection "white" "royalblue")
    (hl-line nil "gray18")
    (helm-ff-directory "deep sky blue" unspecified)
    (company-scrollbar-bg   nil "gainsboro")
    (company-template-field "black" "gainsboro")
    (company-tooltip        "black" "gainsboro")
))
(load-theme 'underwater t)

(set-face-underline 'hl-line nil)


(provide 'my-colors)


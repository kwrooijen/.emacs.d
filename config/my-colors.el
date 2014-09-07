; Enable my powerline Theme
(my-powerline-theme)

(defun set-foreground-background (face foreground &optional background)
    (set-face-foreground face foreground)
    (if background (set-face-background face background)))
(defun sfb(l)
    (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

;; Enable Zenburn Theme
(progn (load-theme 'zenburn t) (setq zenburn t))
;; (progn (load-theme 'anti-zenburn t) (setq anti-zenburn t))

(if (boundp 'zenburn)
 (sfb '(
 (default "#e5e3e3" "#1c1c1c")
 (ac-candidate-face "#8FB28F" "#2B2B2B")
 (ac-selection-face "#6F6F6F" "#8FB28F")
 (ac-yasnippet-candidate-face "#F0DFAF" "#2B2B2B" )
 (ac-yasnippet-selection-face "#F0DFAF" "#6F6F6F")
 (dired-ignored "#8f8a8a")
 (flymake-errline "#ff0000")
 (flymake-warnline "#ffeb08")
 (font-lock-comment-delimiter-face  "#707070")
 (font-lock-comment-face "#707070")
 (font-lock-constant-face "#dc1e6a")
 (font-lock-doc-face "#bc853d")
 (font-lock-string-face "#CC9393")
 (font-lock-type-face "#9d4fcf")
 (git-gutter+-added "green" "#383838" )
 (git-gutter+-deleted "red" "#383838" )
 (git-gutter+-modified "magenta" "#383838" )
 (git-gutter+-unchanged nil "#383838")
 (helm-ff-directory "cyan" "#121212")
 (helm-ff-executable "#9FC59F" "#1c1c1c")
 (helm-ff-file "#DCDCCC" "#1c1c1c")
 (helm-swoop-target-line-block-face "#fff" "#585858")
 (helm-swoop-target-line-face "#fff" "#585858")
 (helm-swoop-target-word-face "#fff" "#7700ff")
 (hl-line nil "#303030")
 (linum "#8FB28F" "#383838")
 (magit-branch nil "#111111")
 (magit-diff-add nil nil)
 (magit-diff-del nil nil)
 (magit-diff-none "#ababab")
 (magit-item-highlight nil nil)
 (region "#fff" "#585858")
 (show-paren-match "#7CB8BB" "#4e4e4e")
 (vertical-border "#383838" "#383838")
 (web-mode-block-attr-name-face "#808080")
 (web-mode-html-attr-custom-face "#b2b2b2")
 (web-mode-html-attr-equal-face "#b2b2b2")
 (web-mode-html-attr-name-face "#b2b2b2")
 (web-mode-html-tag-bracket-face "#808080")
 (web-mode-html-tag-face "#808080")
 (web-mode-symbol-face "#2a55d4")
)))

(if (boundp 'anti-zenburn)
 (sfb '(
 (default "#000" "#f1f1f1")
 (ac-candidate-face "#8FB28F" "#2B2B2B")
 (ac-selection-face "#6F6F6F" "#8FB28F")
 (ac-yasnippet-candidate-face "#F0DFAF" "#2B2B2B" )
 (ac-yasnippet-selection-face "#F0DFAF" "#6F6F6F")
 (dired-ignored "#8f8a8a")
 (flymake-errline "#ff0000")
 (flymake-warnline "#ffeb08")
 (font-lock-comment-delimiter-face  "#707070")
 (font-lock-comment-face "#707070")
 (font-lock-constant-face "#dc1e6a")
 (font-lock-doc-face "#bc853d")
 (font-lock-type-face "#000")
 (font-lock-string-face "#CC9393")
 (helm-ff-directory "#6c1f1c"  "#f1f1f1")
 (helm-ff-executable "#603a60" "#f1f1f1")
 (helm-ff-file "#232333" "#f1f1f1")
 (helm-swoop-target-line-block-face "#222222" "#cccc00")
 (helm-swoop-target-line-face "#222222" "#e3e300")
 (helm-swoop-target-word-face "#f1f1f1" "#7700ff")
 (hl-line nil "#c7c7c7")
 (linum "#603a60" "#f1f1f1" )
 (magit-branch nil nil)
 (magit-diff-none "#ababab")
 (magit-diff-add "#2d965c" nil)
 (magit-diff-del "#dc0000" nil)
 (magit-item-highlight nil "#e1e1e1")
 (region "#f1f1f1" "#585858")
 (show-paren-match "#7CB8BB" "#4e4e4e")
 (vertical-border "#383838" "#383838")
 (web-mode-block-attr-name-face "#808080")
 (web-mode-html-attr-custom-face "#b2b2b2")
 (web-mode-html-attr-equal-face "#b2b2b2")
 (web-mode-html-attr-name-face "#b2b2b2")
 (web-mode-html-tag-bracket-face "#808080")
 (web-mode-html-tag-face "#808080")
 (web-mode-symbol-face "#2a55d4")
)))

(provide 'my-colors)


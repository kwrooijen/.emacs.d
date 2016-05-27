(defun set-foreground-background (face foreground &optional background)
  (set-face-foreground face foreground)
  (if background (set-face-background face background)))

(defun sfb (l)
  (mapcar (lambda(list) (set-foreground-background (nth 0 list)(nth 1 list)(nth 2 list) )) l))

(load-theme 'jazz t)
(set-face-background 'default "#2f2922")
(set-face-background 'fringe "#25201b")
(set-face-foreground 'fringe "#c6a57b")
(set-fringe-mode '(1 . 0))

(set-face-background 'font-lock-comment-delimiter-face "#2a251e")
(set-face-background 'font-lock-comment-face "#2a251e")

(set-face-attribute 'highlight-symbol-face nil :inherit 'default)
(set-face-attribute 'highlight-numbers-number nil :foreground "#ff7f00")

(set-face-attribute 'helm-ff-directory nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'dired-directory)

(set-face-attribute 'helm-ff-file nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'default)

(set-face-attribute 'helm-ff-symlink nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'dired-symlink)

(set-face-attribute 'helm-buffer-directory nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'dired-directory)

(set-face-attribute 'helm-buffer-file nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'default)

(set-face-attribute 'helm-buffer-process nil
                    :foreground "#cd8500"
                    :background 'unspecified)

(set-face-attribute 'linum nil :inherit 'default)
(set-face-background 'linum nil)

(set-face-attribute 'term-color-blue nil :background "#385e6b" :foreground "#385e6b")

(set-face-attribute 'ac-candidate-face nil :inherit 'company-tooltip-common)
(set-face-attribute 'ac-completion-face nil :inherit 'company-preview-common)
(set-face-attribute 'ac-selection-face nil :inherit 'company-tooltip-common-selection)

(require 'whitespace)
(sfb '((highlight-symbol-face "#fff" "gray20" )
       (ac-completion-face unspecified unspecified)))

(set-face-attribute 'whitespace-space nil
                    :foreground "#505050"
                    :background 'unspecified
                    :inherit 'default)

(setq whitespace-style
      '(face tabs spaces trailing
             space-before-tab indentation
             space-after-tab space-mark tab-mark))

(set-face-attribute 'mode-line-buffer-id nil :foreground "#cd853f")
(set-face-attribute 'mode-line-inactive nil :foreground "grey")
(set-face-attribute 'vertical-border     nil :foreground "#25201b" :inherit 'fringe)
(set-face-attribute 'elscreen-tab-background-face     nil :background "#25201b" :inherit 'fringe)
(set-face-attribute 'elscreen-tab-current-screen-face nil :foreground nil :background nil :inherit 'fringe)
(set-face-attribute 'elscreen-tab-current-screen-face nil :background "#2f2922" :foreground "#c6a57b")
(set-face-attribute 'elscreen-tab-other-screen-face   nil :background "#4b4238" :foreground "#25201b")

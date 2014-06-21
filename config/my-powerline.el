(defun dash-string (d s)
    "Execute dash expression with a string. Converts string to a
    list, applies the dash function and wraps it back to a string again"
    (let ((result (funcall d (-drop 1 (split-string s "")))))
        (if result (list-to-string result) "")))

(defun string/reverse (str)
      "Reverse the str where str is a string"
      (apply #'string
	     (reverse
	      (string-to-list str))))

(defpowerline god-mode-bar
 (if mark-active
     (format "[VISUAL]")
     (if (and (boundp 'god-local-mode) god-local-mode)
              (format "[NORMAL]")
              (format "[INSERT]"))))

(defpowerline my-garak-notify ;; TODO Fix colors that are lost
    (string/reverse (dash-string (lambda(x)
        (--drop-while (not (equal it "]")) x))
        (string/reverse (powerline-raw mode-line-modes)))))

(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
    '("%e"
      (:eval
       (let* ((active (powerline-selected-window-active))
              (mode-line (if active 'mode-line 'mode-line-inactive))
              (face1 (if active 'powerline-active1 'powerline-inactive1))
              (face2 (if active 'powerline-active2 'powerline-inactive2))
              (separator-left (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (car powerline-default-separator-dir))))
              (separator-right (intern (format "powerline-%s-%s"
                                               powerline-default-separator
                                               (cdr powerline-default-separator-dir))))

              (lhs (list
                         (powerline-raw "%*" nil 'l)
                         (if (fboundp 'total-unread) (format " %i" total-unread))
                         (my-garak-notify nil 'l)
                         (powerline-raw mode-line-mule-info nil 'l)
                         (powerline-buffer-id nil 'l)
                         (god-mode-bar nil 'l)
                         (when (and (boundp 'which-func-mode) which-func-mode)
                           (powerline-raw which-func-format nil 'l))
                         (powerline-raw " ")
                         (funcall separator-left mode-line face1)
                         (when (boundp 'erc-modified-channels-object)
                           (powerline-raw erc-modified-channels-object face1 'l))
                         (powerline-major-mode face1 'l)
                         (powerline-process face1)
                         (powerline-narrow face1 'l)
                         (powerline-raw " " face1)
                         (funcall separator-left face1 face2)
                         (powerline-vc face2 'r)
))
              (rhs (list (funcall separator-right face2 face1)
                         (powerline-raw "%3l:%2c " face1 'l)
                         (funcall separator-right face1 mode-line)
                         (powerline-raw " ")
                         (powerline-raw "%7p" nil 'r)
                         (powerline-hud face2 face1)
)))
         (concat (powerline-render lhs)
                 (powerline-fill face2 (powerline-width rhs))
                 (powerline-render rhs)
))))))

(provide 'my-powerline)

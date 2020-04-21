;;; custom/custom-functions.el -*- lexical-binding: t; -*-

(defmacro add-hook* (mode &rest body)
  `(add-hook ,mode (lambda () ,@body)))

(defun capitalize-previous-word ()
  (interactive)
  (save-excursion
    (backward-word)
    (capitalize-word 1)))

(defun select-minibuffer ()
 "Make the active minibuffer the selected window."
 (interactive)
 (when (active-minibuffer-window)
   (select-window (active-minibuffer-window))))

(provide 'custom-functions)

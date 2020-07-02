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

(defun change-language ()
  (interactive)
  (save-excursion
    (call-interactively 'ispell-change-dictionary)
    (flyspell-buffer)))

(defun kwrooijen/recenter (&rest x)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun get-beginning-of-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (point)))

(defun get-end-of-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (point)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun upgrade-packages ()
  "Upgrades all packages to their latest version (using Straight.el) and updates the lockfile."
  (interactive)
  (straight-pull-all)
  (straight-freeze-versions))

(defun rollback-packages ()
  "Drops all changes to the lockfile, resulting in a rollback of all packages."
  (interactive)
  (shell-command "git -C $HOME/.emacs.d checkout -- emacs/straight/versions/default.el"))

(provide 'custom-functions)

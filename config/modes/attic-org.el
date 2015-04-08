;;==============================================================================
;;== Org
;;==============================================================================

(defun org-keys-hook ()
    (define-prefix-command 'org-mode-custom-map)
    (define-key org-mode-map (kbd "C-c C-o") 'org-mode-custom-map)
    (define-key org-mode-custom-map (kbd "C-l") 'browse-url-at-point)
    (define-key org-mode-custom-map (kbd "C-t") 'org-todo))

;;==============================================================================
;;== Options
;;==============================================================================

;; Log time
(setq org-log-done 'time)
(setq org-capture-templates '())
;; Add Capture Templates
(setq org-capture-templates
      '(("1" "Done" entry
         (file+headline "~/Documents/notes/Org/Done.org" "Done")
         (file "~/.emacs.d/Templates/Done.orgtpl"))
        ("2" "Retro" entry
         (file+headline "~/Documents/notes/Org/Retro.org" "Done")
         (file "~/.emacs.d/Templates/Retro.orgtpl"))
        ("3" "Todo" entry
         (file+headline "~/Documents/notes/Org/Todo.org" "Todo")
         (file "~/.emacs.d/Templates/Todo.orgtpl"))))

(add-my-todos-to-org
 (directory-files
  (expand-file-name "~/Documents/notes/Org/Todo")
  nil
  "^\\([^#|^.]\\|\\.[^.]\\|\\.\\..\\)"))

;;==============================================================================
;;== Hooks
;;==============================================================================

(add-hook 'org-mode-hook 'org-keys-hook)

;;==============================================================================
;;== Functions
;;==============================================================================

(defun add-my-todos-to-org (list)
  "Adds All the files in Todo directory to my list of todo subjects."
  (let ((c 0)
        (r '()))
    (while (nth c list)
      (let ((key (char-to-string (+ c 97)))
            (val (nth c list)))
        (add-to-list 'org-capture-templates `(,key ,val entry
         (file+headline ,(concat "~/Documents/notes/Org/Todo/" val) ,val)
         (file "~/.emacs.d/Templates/GenericTodo.orgtpl")))
        (setq c (+ c 1))))))

(provide 'attic-org)

(fset 'copy-line-to-previous-line
   (lambda (&optional arg) "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("  e awoy;;;p" 0 "%d")) arg)))

(fset 'copy-line-to-next-line
   (lambda (&optional arg) "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("  a ewy;;;n" 0 "%d")) arg)))

(fset 'copy-line-to-end-of-line
   (lambda (&optional arg) "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("  m ewy;;;" 0 "%d")) arg)))

(fset 'copy-line-to-beginning-of-line
   (lambda (&optional arg) "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item (quote ("  e mwy;;;" 0 "%d")) arg)))

(provide 'my-macros)

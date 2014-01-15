(fset 'copy-line-to-previous-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("  e awoy;;;p" 0 "%d")) arg)))

(fset 'copy-line-to-next-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("  a ewy;;;n" 0 "%d")) arg)))

(provide 'my-macros)

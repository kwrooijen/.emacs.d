
(fset 'copy-line-to-next-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("  a ewy;;;n" 0 "%d")) arg)))

(provide 'my-macros)

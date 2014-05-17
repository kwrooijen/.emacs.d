(setq all-keyboard-keys '(
    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d"
    "f" "g" "h" "j" "k" "l" "z" "x" "c" "v" "b" "n" "m"
    "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "A" "S" "D"
    "F" "G" "H" "J" "K" "L" "Z" "X" "C" "V" "B" "N" "M"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-"
    "=" "\\" "`" "[" "]" ";" "'" "," "." "/"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_"
    "+" "|" "~" "{" "}" ":" "\"" "<" ">" "?"
    ))

(setq meta-functions
  (mapcar (lambda(key)
    (lookup-key (current-global-map) (kbd (format "M-%s" key))))
      all-keyboard-keys))

(setq meta-key-storage
  (mapcar* #'cons all-keyboard-keys meta-functions))


(defun call-meta-by-char (char)
  (call-interactively (cdr (assoc (string char) meta-key-storage))))

(defun call-control-by-char (char)
  (call-interactively (key-binding (kbd (format "%s" (string char))))))

(defun god-g (_ char)
  (interactive "p\nc ")
  (call-meta-by-char char)
  (setq god-repeat-char char)
  (call-interactively 'god-g-repeat)
)
(defun god-g-repeat (_ char)
  (interactive "p\nc ")
  (if (equal char god-repeat-char)
      (progn
        (call-meta-by-char char)
        (call-interactively 'god-g-repeat))
      (if god-local-mode
          (call-control-by-char char)
          (insert char))
      ))

(define-key god-local-mode-map (kbd "g") 'god-g)

(define-key attic-minor-mode-map (kbd "ESC") 'god-mode-enable)
(provide 'god-tty)

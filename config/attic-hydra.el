(defun private--set-hydra-function (var)
  `(defhydra ,(make-symbol (concat "hydra-god-repeater-" var))
     (god-local-mode-map "g")
     (,var (lambda() (interactive) (call-interactively (key-binding (kbd ,(concat "M-" var))))))))

(defmacro set-hydra-meta-repeat (&rest vars)
  (let ((forms (mapcar 'private--set-hydra-function vars)))
    `(progn ,@forms)))

(set-hydra-meta-repeat "q" "w" "e" "r" "t" "y" "u" "i" "o"
                       "p" "a" "s" "d" "f" "g" "h" "j" "k"
                       "l" "z" "x" "c" "v" "b" "n" "m"
                       "1" "2" "3" "4" "5" "6" "7" "8" "9"
                       "0" "!" "@" "#" "$" "%" "^" "&" "*"
                       "(" ")" "_" "+" "{" "}" "|" ":" "\""
                       "<" ">" "?" "-" "=" "[" "]" ";" "'"
                       "\\" "," "." "/" "`" "~")

;; Special cases
(defhydra hydra-god-repeater-g (god-local-mode-map "g") ("g" goto-line))
(defhydra hydra-god-repeater-G (god-local-mode-map "g") ("G" goto-line))

(defhydra buffer-move (semi-colon-map "m")
  "buffer-move"
  ("n" buf-move-down)
  ("p" buf-move-up)
  ("f" buf-move-right)
  ("b" buf-move-left)

  ("l" buf-move-down)
  ("o" buf-move-up)
  ("p" buf-move-right)
  ("k" buf-move-left))

(defhydra transpose-mark-end (semi-colon-map "t")
  "Move transpose-mark-region-end."
  ("f" tmr-end--forward-char "forward-char")
  ("b" tmr-end--backward-char "backward-char")
  ("M-f" tmr-end--forward-word "forward-word")
  ("M-b" tmr-end--backward-word "backward-word")
  ("q" hydra-keyboard-quit "quit" :color blue))

(defhydra transpose-mark-start (semi-colon-map "r")
  "Move transpose-mark-region-start."
  ("f" tmr-start--forward-char "forward-char")
  ("b" tmr-start--backward-char "backward-char")
  ("M-f" tmr-start--forward-word "forward-word")
  ("M-b" tmr-start--backward-word "backward-word")
  ("q" hydra-keyboard-quit "quit" :color blue))


(defhydra hydra-move-line ()
  "Move lines up or down"
  ("M-n" move-line-down)
  ("M-p" move-line-up))

(provide 'attic-hydra)

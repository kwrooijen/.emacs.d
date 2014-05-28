;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;

(defun what-line-int ()
    "Print the current line number (in the buffer) of point."
    (interactive)
    (save-restriction
        (widen)
        (save-excursion
            (beginning-of-line)
            (1+ (count-lines 1 (point))))))

(defun dash-string (d s)
    "Execute dash expression with a string. Converts string to a
    list, applies the dash function and wraps it back to a string again"
    (let ((result (funcall d (-drop 1 (split-string s "")))))
        (if result (list-to-string result) "")))

(defun get-indent (value)
    "Gets the amount of spaces at the start of a string"
    (length (dash-string (lambda(x) (--take-while (or (equal it " ") (equal it "")) x)) value)))

(defun trim-starting-whitespace (value)
    "Removes all whitespace from the start of a string"
    (dash-string (lambda(x) (--drop-while (or (equal it "") (equal it " ")) x)) value))

(defun point-is (p &rest xs)
    "Check if the given point is one of multiple characters"
    (--any? (equal t it) (-map (lambda(x) (equal (char-at-point p) x)) xs)))

(defun char-at-point (p)
    "Gets the character at the given point"
    (string (char-after p)))

(defun list-to-string (xs)
    "Converts a list to a string"
    (--reduce (format "%s%s" acc it) xs))

(defun get-current-line ()
    "Get the current line as a string"
    (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun get-prev-line (&optional recursive)
    "Get the previous line as a string"
    (let ((r (- (point-at-bol) 2)) (result ""))
        (if (<= r 0) "***beginning-of-file***" (progn
            (if recursive (while (point-is r "\n" " ") (setq r (- r 1))))
            (while (and (/= r 0) (not (equal (char-at-point r) "\n")))
                (setq result (concat (char-at-point r) result))
                (setq r (- r 1)))
            result))))

(defun get-next-line (&optional recursive)
    "Get the next line as a string"
    (let ((p (+ (point-at-eol) 1)) (result ""))
        (setq p (recur p))
          (while (not (equal (char-at-point p) "\n"))
            (setq result (concat result (char-at-point p)))
        (setq p (+ p 1))) p
        result))

(defun recur (p)
    "A helper function to recursively look for the next valid string"
    (if recursive (while (point-is p "\n") (setq p (+ p 1)))) p
    (let ((is-line p))
        (if recursive (while (point-is is-line " ") (setq is-line (+ is-line 1))))
        (if (point-is is-line "\n") (recur is-line) p)))

(defun take-to-column (col)
    "Move line to column number and adjust cursor position accordingly"
    (let (
        (old-column (current-column))
        (old-indent (current 'indent))
    )
    (beginning-of-line)
    (just-one-space 0)
    (-dotimes col (lambda (n) (insert " ")))

    (let ((new-pos (+ old-column (- (current 'indent) old-indent))))
        (if (>= new-pos 0) (move-to-column new-pos) (move-to-column 0)))))

(defun mode-doom-rules ()
    "Get the indent rules of the current major mode as well as the default 'all' rules"
    (let ((result (cdr (assoc (with-current-buffer (buffer-name) major-mode) my-doom)))
         (all (cdr (assoc 'all my-doom))))
    (append (if result result '()) all)
))

(defun calc-tab ()
    "Check which ident rule is true and evaluate the requested tab number.
    Custom indent rules can be added inside the my-doom variable"
    (eval (car (last (car (--drop-while (not (eval (car it))) (append (mode-doom-rules) (IoD-standard))))))))

;;;;;;;;;;;;;;;;;;;;;;; DSL ;;;;;;;;;;;;;;;;;;;;;;;

;; @DOC
;; The DSL can be used as follows:
;;; First you have a target; prev, next or current
;;; Next you choose a situation; ends-on, starts-with, indent, indent-char and indent-char-is
;;; Lastly you can add an comparison, which is a string (however indent takes an int and indent-char doesn't take anything)
;;;; Example:
;;; (prev 'starts-with ";;;; Ex")
;;; Executing the previous line (C-x C-e) will return t. Because the line before that starts with the given string.
;;; (next 'ends-on "!!!")
;;; Executing the previous line will also return t because the this line has 3 exclamation marks at the end !!!
    ;;; (current 'indent)
        ;;; Executing the previous line will return 4 because it has an indentation of 4 spaces.
    ;;; (prev 'indent 2)
;;; And lastly executing the previous line will result in 8 + (* tab-width 2).

(defun prev (function &rest values)
    "Previous line as a target"
    (funcall function (get-prev-line t) values))

(defun next (function &rest values)
    "Next line as a target"
    (funcall function (get-next-line t) values))

(defun current (function &rest values)
    "Current line as a target"
    (funcall function (get-current-line) values))

(defun ends-on (value xs)
    "Check if the line ends on one of the following strings"
    (--any? (equal (if (>= (length value) (length it))
        (substring value (* (length it) -1) nil) nil)
            it) xs))

(defun starts-with (value xs)
    "Check if the line starts with one of the following strings"
    (let ((new-value (trim-starting-whitespace value)))
    (--any? (equal (if (>= (length new-value) (length it))
        (substring new-value 0 (length it)) nil)
        it) xs)))

(defun indent (value tab)
    "Get the indentation of a line in spaces"
    (let ((result (get-indent value)))
        (if tab (+ (* tab-width (car tab)) result) result)))

(defun indent-char (value _)
    "Get the indentation character of a line"
    (substring (trim-starting-whitespace value) 0 1))

(defun indent-char-is (value xs)
    "Check if the indentation character of a line
    is equal to one of the given characters"
    (equal (substring (trim-starting-whitespace value) 0 1) (car xs)))


;;;;;;;;;;;;;;;;; Default Config ;;;;;;;;;;;;;;;;;;

(defvar tab-of-doom-mode-map (make-keymap) "tab-of-doom-mode keymap.")

(define-minor-mode tab-of-doom-mode
    "One tabbing mode to rule them all"
    nil " ToD" 'tab-of-doom-mode-map)

(define-key tab-of-doom-mode-map (kbd "TAB") 'tab-of-doom)

(setq my-doom '())

(defun IoD-standard ()
    "These are the standard tabbing rules. If the variable
    doom-use-standard is not nil they will be automatically used."
    (if doom-use-standard '(
        ((< (current 'indent) (prev 'indent -1)) (prev 'indent -1) )
        ((and (>= (current 'indent) (prev 'indent -1)) (< (current 'indent) (prev 'indent))) (prev 'indent))
        ((and (>= (current 'indent) (prev 'indent)) (< (current 'indent) (prev 'indent 1))) (prev 'indent 1))
    (t 0)) '() ))

;;;;;;;;;;;;;;;;;; End Functions ;;;;;;;;;;;;;;;;;;

(defun tab-of-doom ()
    "Tab of doom initial function"
    (interactive)
    (if mark-active (tab-of-doom-region) (tab-of-doom-line)))

(defun tab-of-doom-region ()
    "Tab of Doom for region selection"
    (let ((old-line   (what-line-int))
          (old-col    (current-column))
          (begin      (region-beginning))
          (end        (region-end))
          (begin-line 0)
          (end-line   0))
    (goto-char end)
    (setq end-line (what-line-int))
    (goto-char begin)
    (unless (equal (what-line-int) end-line) (tab-of-doom-line))
    (while (/= (what-line-int) end-line )
         (next-line)
         (tab-of-doom-line))
     (goto-line old-line)
     (move-to-column old-col)
     (error "") ;; Throw error to keep region active
))

(defun tab-of-doom-line ()
    "Tab of Doom for current line"
    (if (calc-tab) (take-to-column (calc-tab))
        (indent-for-tab-command)))

(provide 'tab-of-doom)

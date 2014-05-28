;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;

(defun dash-string (d s)
    (let ((result (funcall d (-drop 1 (split-string s "")))))
      (if result (list-to-string result) "")))

(defun get-indent (value)
    (length (dash-string (lambda(x) (--take-while (or (equal it " ") (equal it "")) x)) value)))

(defun trim-starting-whitespace (value)
    (dash-string (lambda(x) (--drop-while (or (equal it "") (equal it " ")) x)) value))

(defun get-current-line ()
    (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun point-is (p &rest xs)
    (--any? (equal t it) (-map (lambda(x) (equal (char-at-point p) x)) xs)))

(defun char-at-point (p) (string (char-after p)))

(defun list-to-string (xs)
    (--reduce (format "%s%s" acc it) xs))

(defun get-prev-line (&optional recursive)
    (let ((r (- (point-at-bol) 2)) (result ""))
        (if recursive (while (point-is r "\n" " ") (setq r (- r 1))))
        (while (and (/= r 0) (not (equal (char-at-point r) "\n")))
            (setq result (concat (char-at-point r) result))
            (setq r (- r 1)))
        result))

(defun get-next-line (&optional recursive)
    (let ((p (+ (point-at-eol) 1)) (result ""))
        (setq p (recur p))
          (while (not (equal (char-at-point p) "\n"))
            (setq result (concat result (char-at-point p)))
        (setq p (+ p 1))) p
        result))

(defun recur (p)
    (if recursive (while (point-is p "\n") (setq p (+ p 1)))) p
    (let ((is-line p))
        (if recursive (while (point-is is-line " ") (setq is-line (+ is-line 1))))
        (if (point-is is-line "\n") (recur is-line) p)))

(defun take-to-column (col)
    "Move line to column number"
    (let (
        (old-column (current-column))
        (old-indent (current 'indent))
    )
    (beginning-of-line)
    (just-one-space 0)
    (-dotimes col (lambda (n) (insert " ")))

    (let ((new-pos (+ old-column (- (current 'indent) old-indent))))
        (if (>= new-pos 0) (move-to-column new-pos) (move-to-column 0)))))

(defun calc-tab ()
  (eval (car (last (car (--drop-while (not (eval (car it))) (append my-doom (IoD-standard))))))))

;;;;;;;;;;;;;;;;;;;;;;; DSL ;;;;;;;;;;;;;;;;;;;;;;;

(defun prev (function &rest values)
    (funcall function (get-prev-line t) values))

(defun next (function &rest values)
    (funcall function (get-next-line t) values))

(defun current (function &rest values)
    (funcall function (get-current-line) values))

(defun ends-on (value xs)
    (--any? (equal (if (>= (length value) (length it))
        (substring value (* (length it) -1) nil) nil)
            it) xs))

(defun starts-with (value xs)
    (let ((new-value (trim-starting-whitespace value)))
    (--any? (equal (if (>= (length new-value) (length it))
        (substring new-value 0 (length it)) nil)
        it) xs)))

(defun indent (value tab)
    (let ((result (get-indent value)))
        (if tab (+ (* tab-width (car tab)) result) result)))

(defun indent-char (value _)
    (substring (trim-starting-whitespace value) 0 1))

(defun indent-char-is (value xs)
    (equal (substring (trim-starting-whitespace value) 0 1) (car xs)))

;;;;;;;;;;;;;;;;; Default Config ;;;;;;;;;;;;;;;;;;

(defvar tab-of-doom-mode-map (make-keymap) "tab-of-doom-mode keymap.")

(define-minor-mode tab-of-doom-mode
    "One tabbing mode to rule them all"
    nil " ToD" 'tab-of-doom-mode-map)

(define-key tab-of-doom-mode-map (kbd "TAB") 'tab-of-doom)

(setq my-doom '())

(defun IoD-standard ()
    (if doom-use-standard '(
        ((< (current 'indent) (prev 'indent -1)) (prev 'indent -1) )
        ((and (>= (current 'indent) (prev 'indent -1)) (< (current 'indent) (prev 'indent))) (prev 'indent))
        ((and (>= (current 'indent) (prev 'indent)) (< (current 'indent) (prev 'indent 1))) (prev 'indent 1))
    (t 0)) '() ))

;;;;;;;;;;;;;;;;;; End Functions ;;;;;;;;;;;;;;;;;;

(defun tab-of-doom ()
    "Tab of doom initial function"
    (interactive)
    (if mark-active (tab-of-doom-region))
    (if multiple-cursors-mode (tab-of-doom-mc))
    (tab-of-doom-line))

(defun tab-of-doom-region ()
    "Tab of Doom for region selection"
    (return))

(defun tab-of-doom-mc ()
    "Tab of Doom for multiple cursors"
    (return))

(defun tab-of-doom-line ()
    "Tab of Doom for current line"
    (if (calc-tab) (take-to-column (calc-tab))
        (indent-for-tab-command)))

(provide 'tab-of-doom)

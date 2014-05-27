;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;
(defun get-current-line ()
    (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun point-is (p &rest xs)
    (--any? (equal t it) (-map (lambda(x) (equal (char-at-point p) x)) xs)))

(defun char-at-point (p) (string (char-after p)))

(defun get-prev-line (&optional recursive)
    (let ((r (- (point-at-bol) 2)) (result ""))
        (if recursive (while (point-is r "\n" " ") (setq r (- r 1))))
        (while (not (equal (char-at-point r) "\n"))
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

(defun dash-string (d s)
    (funcall d (-drop 1 (split-string s ""))))

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

;;;;;;;;;;;;;;;;;;;; Utils END ;;;;;;;;;;;;;;;;;;;;

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
    (--any? (equal (if (>= (length value) (length it))
        (substring value 0 (length it)) nil)
            it) xs))

(defun indent (value _)
    (length (dash-string (lambda(x) (--take-while (equal it " ") x)) value)))

(defun indent-char (value _)
    (car (dash-string (lambda(x) (--drop-while (equal it " ") x)) value)))

(defun indent-char-is (value xs)
    (equal (car (dash-string (lambda(x) (--drop-while (equal it " ") x)) value)) (car xs)))

;;;;;;;;;;;;;;;;;;;;; DSL END ;;;;;;;;;;;;;;;;;;;;;

(defun tab-of-doom-region ()
   "Tab of Doom for region selection"
   (return))
(defun tab-of-doom-mc ()
   "Tab of Doom for multiple cursors"
   (return))

(defun tab-of-doom-line ()
    "Tab of Doom for current line"
    (let (
        (prev1       (prev 'indent))
        (minus-prev  (- (prev 'indent) tab-width))
        (plus-prev   (+ (prev 'indent) tab-width))
        (curr-ind    (current 'indent))
        (curr        (current-column))
        )
        (take-to-column
            (if (prev 'ends-on ",") (prev 'indent)
            (if (prev 'ends-on "[" "{") (+ (prev 'indent) tab-width)
            (if (current 'indent-char-is "]") (- (prev 'indent) tab-width)
            (if (< curr-ind minus-prev) minus-prev
            (if (and (>= curr-ind minus-prev) (< curr-ind prev1)) prev1
            (if (and (>= curr-ind prev1) (< curr-ind plus-prev)) plus-prev 0
            )))))))
        ))

(defun tab-of-doom ()
    "Tab of doom initial function"
    (interactive)
    (if mark-active (tab-of-doom-region))
    (if multiple-cursors-mode (tab-of-doom-mc))
    (tab-of-doom-line))

(defvar tab-of-doom-mode-map (make-keymap) "tab-of-doom-mode keymap.")

(define-key tab-of-doom-mode-map (kbd "TAB") 'tab-of-doom)

(define-minor-mode tab-of-doom-mode
    "One tabbing mode to rule them all"
    nil " ToD" 'tab-of-doom-mode-map)

(add-hook 'minibuffer-setup-hook (lambda() (tab-of-doom-mode 0)))
(tab-of-doom-mode 0)

(provide 'tab-of-doom)

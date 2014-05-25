(defvar tab-of-doom-mode-map (make-keymap) "tab-of-doom-mode keymap.")

(define-key tab-of-doom-mode-map (kbd "TAB") 'tab-of-doom)

(define-minor-mode tab-of-doom-mode
    "One tabbing mode to rule them all"
    nil " ToD" 'tab-of-doom-mode-map)

(defun line-length ()
    "This function will return the length of a line"
    (let ((current (current-column)))
        (end-of-line) (current-column)
        (let ((end (current-column)))
            (move-to-column current)
            end)))

(defun get-previous-indent ()
    "Get the column number of the previous line's indent point"
    (let ((previous (point)))
        (previous-line)
        (end-of-line)
        (while (equal (current-column) 0)
            (progn
                (previous-line)
                (end-of-line)
            ))
        (back-to-indentation)
        (let ((result (current-column)))
            (goto-char previous)
            result)))

(defun get-current-indent ()
    "Get the column number of the current line's indent point"
    (let ((previous (current-column)))
        (back-to-indentation)
        (let ((result (current-column)))
            (move-to-column previous)
            result)))

(defun take-to-column (col)
    "Move line to column number"
    (interactive)
    (beginning-of-line)
    (just-one-space 0)
    (let (c) (-dotimes col (lambda (n) (insert " "))) c))

(defun tab-of-doom-region ()
    "Tab of Doom for region selection"
    (return))

(defun tab-of-doom-mc ()
    "Tab of Doom for multiple cursors"
    (return))

(defun tab-of-doom-line ()
    "Tab of Doom for current line"
    (let (
        (prev        (get-previous-indent))
        (minus-prev  (- (get-previous-indent) tab-width))
        (plus-prev   (+ (get-previous-indent) tab-width))
        (current     (current-column))
        (curr-ind    (get-current-indent))
        (prev-length (line-length)))
        (take-to-column
            (if (< curr-ind minus-prev) minus-prev
            (if (and (>= curr-ind minus-prev) (< curr-ind prev)) prev
            (if (and (>= curr-ind prev) (< curr-ind plus-prev)) plus-prev 0))))
        (let ((new-pos (+ current (- (get-current-indent) curr-ind))))
            (if (>= new-pos  0)
            (move-to-column new-pos)
            (move-to-column 0)))))

(defun tab-of-doom ()
    "Tab of doom initial function"
    (interactive)
    (if mark-active (tab-of-doom-region))
    (if multiple-cursors-mode (tab-of-doom-mc))
    (tab-of-doom-line))

(add-hook 'minibuffer-setup-hook (lambda() (tab-of-doom-mode 0)))
(tab-of-doom-mode 0)
(provide 'tab-of-doom)

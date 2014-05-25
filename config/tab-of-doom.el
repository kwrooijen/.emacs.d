(defvar tab-of-doom-mode-map (make-keymap) "tab-of-doom-mode keymap.")

(define-key tab-of-doom-mode-map (kbd "TAB") 'tab-of-doom)

(define-minor-mode tab-of-doom-mode
    "One tabbing mode to rule them all"
    nil " ToD" 'tab-of-doom-mode-map)

(defun line-length () (interactive)
    (let ((current (current-column)))
        (end-of-line) (current-column)
        (let ((end (current-column)))
            (move-to-column current)
            end
        )))

(defun get-previous-indent ()
    (let ((previous (current-column)))
        (previous-line)
        (back-to-indentation)
        (let ((result (current-column)))
            (next-line)
            (move-to-column previous)
            result
        )))

(defun get-current-indent ()
    (let ((previous (current-column)))
        (back-to-indentation)
        (let ((result (current-column)))
            (move-to-column previous)
            result
        )))

(defun take-to-column (col)
    (interactive)
    (beginning-of-line)
    (just-one-space 0)
    (let (c) (-dotimes col (lambda (n) (insert " "))) c))

(defun is-prev-comma ()
  (previous-line)
  (end-of-line))

(defun tab-of-doom () (interactive)
  (let ((prev (get-previous-indent))
        (current (current-column))
        (current-indent (get-current-indent))
        (prev-length (line-length)))
    (if (< current-indent prev)
        (take-to-column  prev)
      (if (equal current-indent prev)
          (take-to-column (+ prev tab-width))
        (if (and (>= current-indent prev) (< current-indent (+ prev tab-width)))
            (take-to-column (- prev tab-width))
          (if (>= prev tab-width)
              (take-to-column (- prev tab-width))
            (take-to-column 0)))))
        (let ((new-pos (+ current (- (get-current-indent) current-indent))))
            (if (>= new-pos  0)
            (move-to-column new-pos)
            (move-to-column 0)
        ))))

(add-hook 'minibuffer-setup-hook (lambda() (tab-of-doom-mode 0)))
(tab-of-doom-mode 0)
(provide 'tab-of-doom)

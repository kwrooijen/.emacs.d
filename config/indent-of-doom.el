;;; indent-of-doom.el ---
;;
;; Filename: indent-of-doom.el
;; Description: A customizable general purpose indenting mode
;; Author: Kevin van Rooijen
;; Maintainer: Kevin van Rooijen
;; Created: May 25 2014
;; Version: 0.1.0
;; Package-Requires: ((dash "2.6.0"))
;; URL:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; The DSL can be used as follows:
;; First you have a target; prev, next or current.
;; Next you choose a situation; ends-on, starts-with, indent, indent-char and
;; indent-char-is.
;; Lastly you can add an comparison, which is a string (however indent takes an
;; int and indent-char doesn't take anything)
;; Example: ---------------------------------------------------------------------
;; (prev 'starts-with ";; Ex")
;; Executing the previous line (C-x C-e) will return t. Because the line before
;; that starts with the given string.
;; (next 'ends-on "!!!")
;; Executing the previous line will also return t !!!
    ;;; (current 'indent)
        ;;; Executing the previous line will return 4 because it has an indentation of 4 spaces.
    ;;; (prev 'indent 2)
;; And lastly executing the previous line will result in 8 + (* tab-width 2).
;;
;; You can add custom rules for your indentation.
;; Example: ---------------------------------------------------------------------
;;(setq my-doom '(
;;    (all . (
;;        ((current 'starts-with "]" "}") (prev 'indent -1))
;;        ((prev 'ends-on "[" "{")        (prev 'indent 1))
;;        ((prev 'ends-on ",")            (prev 'indent))
;;    ))
;;))
;;
;; Saving the my-doom varaible will add these rules to all major-modes which
;; have indent-of-doom-mode active. It reads as follows from up to down:
;;
;; Line 36:
;; If the current line starts with the characters "]" or "}" then
;; the current line should have the same indentation as the
;; previous line MINUS 1 tab-width.
;;
;; Line 37:
;; If the previous line ends with the characters "[" or "{" then
;; the current line should have the same indentation as the
;; previous line PLUS 1 tab-width.
;;
;; Line 38:
;; If the previous line ends with the character "," then
;; the current line should have the same indentation as the
;; previous line.
;;
;; These rules will indent these lines in the following matter automatically:
;;
;; myVariableList = [
;;     valueOne,
;;     valueTwo,
;;     valueThree
;; ]
;;
;; Whereas in certain modes this will be indented like this:
;;
;; myVariableList = [
;;                   valueOne,
;;                   valueTwo,
;;                   valueThree
;;                  ]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;; Config ;;;;;;;;;;;;;;;;;;;;;

(defgroup indent-of-doom ()
  "Customize group for indent-of-doom.el"
  :prefix "indent-of-doom-"
  :group 'indent)

(defcustom doom-use-tab-cycle nil
    "Use tab of doom to cycle through 3
    indentations depending on previous line."
    :type 'boolean
    :group 'indent-of-doom)

(defcustom doom-tab-cycle-zero nil
    "When cycling through the 3 indentation you
    also cycle to the beginning of the line."
    :type 'boolean
    :group 'indent-of-doom)

(defcustom doom-region t
    "Use indentation of doom while region is active."
    :type 'boolean
    :group 'indent-of-doom)

(defcustom doom-region-cycle nil
    "Use the 3 indentation cycle while region is active.
    Turning this off won't affect your custom indentation settings."
    :type 'boolean
    :group 'indent-of-doom)

(defcustom doom-indent-fallback t
    "When no indentation rules are found, fallback to the original tab."
    :type 'boolean
    :group 'indent-of-doom)

(defcustom doom-indent-region-fallback t
    "When no indentation rules are found while region is active,
    fallback to the original tab."
    :type 'boolean
    :group 'indent-of-doom)

(defcustom doom-indent-key "TAB"
    "The key to run tab-of-doom."
    :type '(string)
    :group 'indent-of-doom)

;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;

(defun what-line-int ()
    "Get the current line number as an int"
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
        (if (<= r 0) "***beginning-of-buffer***" (progn
            (if recursive (while (and (> r 0) (point-is r "\n" " ") ) (setq r (- r 1))))
            (while (and (> r 0) (not (equal (char-at-point r) "\n")))
                (setq result (concat (char-at-point r) result))
                (setq r (- r 1)))
            result))))

(defun recur (p &optional r)
    "A helper function to recursively look for the next valid string"
    (if r (while (point-is p "\n") (setq p (+ p 1)))) p
    (let ((is-line p))
        (if r (while (point-is is-line " ") (setq is-line (+ is-line 1))))
        (if (point-is is-line "\n") (recur is-line r) p)))

(defun get-next-line (&optional recursive)
    "Get the next line as a string"
    (let ((p (+ (point-at-eol) 1)) (result ""))
        (setq p (recur p recursive))
          (while (not (equal (char-at-point p) "\n"))
            (setq result (concat result (char-at-point p)))
        (setq p (+ p 1))) p
        result))

(defun take-to-column (col)
    "Move line to column number and adjust cursor position accordingly"
    (let (
        (old-column (current-column))
        (old-indent (current 'indent))
    )
    (unless (equal col old-column)
        (beginning-of-line)
        (just-one-space 0)
        (-dotimes col (lambda (n) (insert " ")))
        (let ((new-pos (+ old-column (- (current 'indent) old-indent))))
            (if (>= new-pos 0) (move-to-column new-pos) (move-to-column 0))))))

(defun doom-mode-rules ()
    "Get the indent rules of the current major mode as well as the default 'all' rules"
    (let ((result (cdr (assoc (with-current-buffer (buffer-name) major-mode) my-doom)))
         (all (cdr (assoc 'all my-doom))))
    (append (if result result '()) all)))

(defun calc-tab (&optional region-true)
    "Check which ident rule is true and evaluate the requested tab number.
    Custom indent rules can be added inside the my-doom variable"
    (eval (car (last (car (--drop-while (not (eval (car it)))
        (append (doom-mode-rules) (IoD-standard region-true))))))))

;;;;;;;;;;;;;;;;;;;;;;; DSL ;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;; End Functions ;;;;;;;;;;;;;;;;;;

(defun indent-of-doom ()
    "Indent of Doom initial function"
    (interactive)
    (if mark-active
        (if doom-region (indent-of-doom-region)
        (indent-for-tab-command)))
    (unless mark-active (indent-of-doom-line)))

(defun indent-of-doom-region ()
    "Indent of Doom for region selection"
    (let ((old-line   (what-line-int))
          (old-col    (current-column))
          (begin      (region-beginning))
          (end        (region-end))
          (end-line   0))
    (goto-char end)
    (setq end-line (what-line-int))
    (goto-char begin)
    (set-mark nil)
    (unless (equal (what-line-int) end-line) (indent-of-doom-line t))
    (while (/= (what-line-int) end-line )
        (call-interactively 'next-line)
        (indent-of-doom-line t))
    (let ((current-prefix-arg old-line))
        (call-interactively 'goto-line))
    (move-to-column old-col)
    (if (> (point) begin) (set-mark begin) (set-mark end))))

(defun indent-of-doom-line (&optional region-true)
    "Indent of Doom for current line"
    (if (calc-tab region-true) (take-to-column (calc-tab region-true))
        (if (or doom-indent-fallback
            (if region-true doom-indent-region-fallback))
        (indent-for-tab-command))))

;;;;;;;;;;;;;;;;; Default Config ;;;;;;;;;;;;;;;;;;

(defun IoD-standard (&optional region-true)
    "These are the standard tabbing rules. If the variable
    doom-use-tab-cycle is not nil they will be automatically used."
    (if (and doom-use-tab-cycle (if region-true doom-region-cycle t)) '(
        ((< (current 'indent) (prev 'indent -1)) (prev 'indent -1) )
        ((and (>= (current 'indent) (prev 'indent -1)) (< (current 'indent) (prev 'indent))) (prev 'indent))
        ((and (>= (current 'indent) (prev 'indent)) (< (current 'indent) (prev 'indent 1))) (prev 'indent 1))
    (t (if doom-tab-cycle-zero 0 (prev 'indent -1)))) '() ))

(defvar indent-of-doom-mode-map (make-keymap) "indent-of-doom-mode keymap.")

(define-minor-mode indent-of-doom-mode
    "One indentation mode to rule them all"
    nil " ToD" 'indent-of-doom-mode-map)

(define-key indent-of-doom-mode-map (kbd doom-indent-key) 'indent-of-doom)

(setq my-doom '())

(provide 'indent-of-doom)

;;; indent-of-doom.el ends here

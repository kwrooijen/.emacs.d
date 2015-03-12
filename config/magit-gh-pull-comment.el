(require 'thingatpt)
(require 'gh-pull-comments)

(defun magit-gh-pull-comment-line ()
  (interactive)
  (magit-section-action visit (info parent-info)
    (hunk
     (post-comment (read-string "Comment: ") parent-info))))

(defun post-comment (body path)
  (let ((api (gh-pull-comments-api "api" :sync nil :cache nil :num-retries 1))
        (pull-id (get-pull-id))
        (comment (make-instance 'gh-pull-comments-comment
                                :body body
                                :path path
                                :position (get-diff-position)
                                :commit-id (get-commit-id))))
    (gh-pull-comments-new api (magit-get-user) (magit-get-repo) pull-id comment)))

(defun magit-get-user ()
  (car (magit-gh-pulls-guess-repo)))

(defun magit-get-repo ()
  (cdr (magit-gh-pulls-guess-repo)))

(defun get-pull-id ()
  (let ((repo (magit-gh-pulls-guess-repo)))
    (let ((api (magit-gh-pulls-get-api))
          (user (car repo))
          (proj (cdr repo)))
      (format "%s" (oref (car
                          (funcall
                           magit-gh-pulls-maybe-filter-pulls
                           (oref (gh-pulls-list api user proj) :data))) :number)))))

(defun get-diff-position ()
  (save-excursion
    (let ((cur-line (what-line-int)))
      (magit-goto-previous-section)
      (- cur-line (what-line-int)))))

(defun get-commit-id ()
  (save-excursion
    (beginning-of-buffer)(forward-char)
    (thing-at-point 'word)))

(provide 'magit-gh-pull-comment)


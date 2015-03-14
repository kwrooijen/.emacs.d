(require 'gh-pull-comments)
(setq gnus-use-cache t)

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
                                :commit-id current-commit-id)))
    (gh-pull-comments-new api (magit-get-user) (magit-get-repo) pull-id comment)))

(defun magit-gh-pulls-compare-with-master ()
  (interactive)
  (setq current-commit-id (get-commit-id))
  ;; TODO get remote repository of PR (origin / other)
  (magit-diff (format "origin/master..origin/%s" (get-branch-name))))

(defun magit-get-user ()
  (car (magit-gh-pulls-guess-repo)))

(defun magit-get-repo ()
  (cdr (magit-gh-pulls-guess-repo)))

(defun get-pull-id ()
  (oref (get-data) :number))

(defun get-pull-branch ()
  (oref (get-data) :commits))

(defun get-data ()
  (let ((repo (magit-gh-pulls-guess-repo)))
    (let ((api (magit-gh-pulls-get-api))
          (user (car repo))
          (proj (cdr repo)))
      (car
       (funcall
        magit-gh-pulls-maybe-filter-pulls
        (oref (gh-pulls-list api user proj) :data))))))

(defun get-gh-pulls-request()
  (car (let ((repo (magit-gh-pulls-guess-repo)))
  (let ((api (magit-gh-pulls-get-api))
            (user (car repo))
            (proj (cdr repo)))
  (funcall magit-gh-pulls-maybe-filter-pulls
           (oref (gh-pulls-list api user proj) :data))))))

(defun get-gh-repos-ref()
  (oref (get-gh-pulls-request) :head))

(defun get-branch-name()
  (message (oref (get-gh-repos-ref) :ref)))

;; TODO This only works when comparing the entire PR to master
;; Would be nice if that wasn't necessary
;; This is also a bit of a dirty hack
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

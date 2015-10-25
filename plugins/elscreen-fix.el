(defun elscreen-window-history-supported-p ()
  (and (fboundp 'window-prev-buffers)
       (fboundp 'window-next-buffers)
       (fboundp 'set-window-prev-buffers)
       (fboundp 'set-window-next-buffers)))

(defun elscreen-get-all-window-history-alist ()
  (when (elscreen-window-history-supported-p)
    (mapcar (lambda (window)
              (let ((prevs (window-prev-buffers window))
                    (nexts (window-next-buffers window)))
                (cons window (cons prevs nexts))))
            (window-list))))

(defun elscreen-restore-all-window-history-alist (history-alist)
  (when (elscreen-window-history-supported-p)
    (mapc (lambda (entry)
            (let* ((window (car entry))
                   (histories (cdr entry))
                   (prevs (car histories))
                   (nexts (cdr histories)))
              (when (window-valid-p window)
                (set-window-prev-buffers window prevs)
                (set-window-next-buffers window nexts))))
          history-alist)))

(defmacro elscreen-save-screen-excursion (&rest body)
  "Execute BODY, preserving ElScreen meta data.
 Return the value of the last form in BODY."
  `(let ((original-buffer-list (buffer-list))
         (original-buffer-live-p nil)
         (original-elscreen-window-configuration
          (elscreen-current-window-configuration))
         (original-frame-confs (elscreen-copy-tree elscreen-frame-confs))
         (original-window-histories (elscreen-get-all-window-history-alist)))
     (unwind-protect
         (save-window-excursion ,@body)
       (setq elscreen-frame-confs original-frame-confs)
       (elscreen-apply-window-configuration
        original-elscreen-window-configuration)
       (mapc
        (lambda (buffer)
          (when (buffer-live-p buffer)
            (bury-buffer buffer)
            (setq original-buffer-live-p t)))
        original-buffer-list)
       (when original-buffer-live-p
         (while (not (memq (car (buffer-list)) original-buffer-list))
           (bury-buffer (car (buffer-list)))))
       (elscreen-restore-all-window-history-alist original-window-histories))))



(defun elscreen-get-screen-to-name-alist (&optional truncate-length padding)
  (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
            screen-name screen-to-name-alist nickname-type-map)
       (elscreen-save-screen-excursion
        (mapc
         (lambda (screen)
           ;; If nickname exists, use it.
           (setq screen-name (elscreen-get-screen-nickname screen))
           ;; Nickname does not exist, so examine major-mode and buffer-name.
           (when (null screen-name)
             (elscreen-goto-internal screen)

             (setq nickname-type-map
                   (mapcar
                    (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (or (elscreen-get-alist-to-nickname
                             elscreen-mode-to-nickname-alist-internal
                             'string-match (symbol-name major-mode))
                            (elscreen-get-alist-to-nickname
                             elscreen-buffer-to-nickname-alist-internal
                             'string-match (buffer-name))
                            (cons 'buffer-name (buffer-name)))))
                    (window-list)))
             (let (nickname-list)
               (while (> (length nickname-type-map) 0)
                 (let ((type (caar nickname-type-map))
                       (name (cdar nickname-type-map)))
                   (when name
                     (setq nickname-list (cons name nickname-list)))
                   (setq nickname-type-map
                         (if (eq type 'nickname)
                             (delete (car nickname-type-map) nickname-type-map)
                           (cdr nickname-type-map)))))
               (setq screen-name
                     (mapconcat 'identity (reverse nickname-list) ":"))))

           (elscreen--set-alist 'screen-to-name-alist screen screen-name))
         screen-list))

       (elscreen-set-screen-to-name-alist-cache screen-to-name-alist))))


  ;; Arguments of truncate-length and padding are deprecated.
  (if truncate-length
      (let ((screen-to-name-alist
             (copy-alist (elscreen-get-screen-to-name-alist-cache))))
        (elscreen-message "Arguments for `elscreen-get-screen-to-name-alist' are deprecated.  Use elscreen-truncate-screen-name for each screen-name.")
        (mapc
         (lambda (screen-to-name)
           (setcdr screen-to-name
                   (elscreen-truncate-screen-name (cdr screen-to-name)
                                                  truncate-length padding)))
         screen-to-name-alist)
        screen-to-name-alist)
    (elscreen-get-screen-to-name-alist-cache)))


(defun elscreen-get-screen-to-name-alist (&optional truncate-length padding)
  (when (elscreen-screen-modified-p 'elscreen-get-screen-to-name-alist)
    (elscreen-notify-screen-modification-suppress
     (elscreen-set-window-configuration (elscreen-get-current-screen)
                                        (elscreen-current-window-configuration))
     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
            screen-name screen-to-name-alist nickname-type-map)
       (elscreen-save-screen-excursion
        (mapc
         (lambda (screen)
           ;; If nickname exists, use it.
           (setq screen-name (elscreen-get-screen-nickname screen))
           ;; Nickname does not exist, so examine major-mode and buffer-name.
           (when (null screen-name)
             (elscreen-goto-internal screen)

             (setq nickname-type-map
                   (mapcar
                    (lambda (window)
                      (with-current-buffer (window-buffer window)
                        (or (elscreen-get-alist-to-nickname
                             elscreen-mode-to-nickname-alist-internal
                             'string-match (symbol-name major-mode))
                            (elscreen-get-alist-to-nickname
                             elscreen-buffer-to-nickname-alist-internal
                             'string-match (buffer-name))
                            (cons 'buffer-name (buffer-name)))))
                    (window-list)))

             (let (nickname-list)
               (while (> (length nickname-type-map) 0)
                 (let ((type (caar nickname-type-map))
                       (name (cdar nickname-type-map)))
                   (when name
                     (setq nickname-list (cons name nickname-list)))
                   (setq nickname-type-map
                         (if (eq type 'nickname)
                             (delete (car nickname-type-map) nickname-type-map)
                           (cdr nickname-type-map)))))
               (setq screen-name
                     (mapconcat 'identity (reverse nickname-list) ":"))))

           (elscreen--set-alist 'screen-to-name-alist screen screen-name))
         screen-list))

       (elscreen-set-screen-to-name-alist-cache screen-to-name-alist))))

  ;; Arguments of truncate-length and padding are deprecated.
  (if truncate-length
      (let ((screen-to-name-alist
             (copy-alist (elscreen-get-screen-to-name-alist-cache))))
        (elscreen-message "Arguments for `elscreen-get-screen-to-name-alist' are deprecated.  Use elscreen-truncate-screen-name for each screen-name.")
        (mapc
         (lambda (screen-to-name)
           (setcdr screen-to-name
                   (elscreen-truncate-screen-name (cdr screen-to-name)
                                                  truncate-length padding)))
         screen-to-name-alist)
        screen-to-name-alist)
    (elscreen-get-screen-to-name-alist-cache)))



  (provide 'elscreen-fix)

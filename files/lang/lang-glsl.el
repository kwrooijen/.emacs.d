;; Uses: https://github.com/patriciogonzalezvivo/glslViewer

(require 'glsl-mode)

(defun kill-glsl-buffer ()
  (when (get-buffer "*GLSL*")
    (let ((kill-buffer-query-functions nil))
      (kill-buffer "*GLSL*"))))

(defun glsl-run-current-file ()
  (interactive)
  (kill-glsl-buffer)
  (let ((cmd (format "glslViewer -l %s" (buffer-file-name)))
        (output-buffer (generate-new-buffer "*GLSL*"))
        (error-buffer  (generate-new-buffer "*GLSL*")))
    (async-shell-command cmd output-buffer error-buffer)))

(mode-leader-def
  'normal glsl-mode-map
  "'" 'glsl-run-current-file)

(provide 'lang-glsl)

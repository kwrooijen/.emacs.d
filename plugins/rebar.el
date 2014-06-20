;;; rebar.el --- emacs wrapper for the erlang rebar tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.4.0
;; Keywords: erlang, tools, processes
;; Created: 2013-12-19

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run rebar easily in emacs.
;;
;; Documentation: https://github.com/rebar/rebar/wiki

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'compile))
(require 'erlext nil t)       ;part of distel, `erlext-binary-to-term'

(eval-and-compile
  (or (fboundp 'user-error) (defalias 'user-error 'error)))
(autoload 'vc-responsible-backend "vc")

(defgroup rebar nil
  "An Erlang build tool."
  :link '(url-link "https://github.com/rebar/rebar")
  :group 'tools)

(defcustom rebar-config-url
  "https://raw.github.com/rebar/rebar/master/rebar.config.sample"
  "URL to the rebar configuration."
  :type 'string
  :group 'rebar)

(defcustom rebar-use-vc-root '(Git SVN Hg)
  "List of vc backends used to determine rebar project root.
If t use all backends in `vc-handled-backends'."
  :type '(choice (const :tag "All" t) (repeat symbol))
  :group 'rebar)

(defcustom rebar-templates '(("simplesrv" "srvid")
                             ("simplenode" "nodeid")
                             ("simplemod" "modid")
                             ("simplelib" "libid")
                             ("simplefsm" "fsmid")
                             ("simpleapp" "appid")
                             ("ctsuite" "testmod")
                             ("basicnif" "module"))
  "List of templates."
  :type '(repeat (repeat string))
  :group 'rebar)

(defvar rebar-cmds
  '("clean" "compile" "escriptize" "create" "create-app" "create-lib"
    "create-node" "list-templates" "doc" "check-deps" "get-deps" "update-deps"
    "delete-deps" "list-deps" "generate" "overlay" "generate-upgrade"
    "generate-appups" "eunit" "ct" "qc" "xref" "help" "version")
  "List of rebar commands.")

(defun rebar-cmds ()
  (with-temp-buffer
    (call-process "rebar" nil t nil "-c")
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^[-[:alnum:]]+" nil t)
             collect (match-string 0))))

(defun rebar-strip-string (s)
  (let* ((re "[ \t\n\r\f\v]+")
         (from (if (string-match (concat "\\`" re) s) (match-end 0) 0))
         (to (and (string-match (concat re "\\'") s) (match-beginning 0))))
    (substring s from to)))

(defun rebar-read-string (prompt &optional initial-input history default-value
                                 inherit-input-method)
  "Like `read-string' but automatically interpolate DEFAULT in PROMPT."
  (let ((prompt (if (and default-value
                         (not (string-match-p "(default .*?): *\\'" prompt)))
                    (format "%s (default %s): "
                            (substring prompt 0 (string-match-p ": *\\'" prompt))
                            (or (car-safe default-value) default-value))
                  prompt)))
    (read-string prompt initial-input history default-value
                 inherit-input-method)))

(defun rebar-bin ()
  (if (file-executable-p "rebar") "./rebar" "rebar"))

(defvar rebar-find-project-function #'rebar-find-project-default
  "Function used to find rebar project root.")

(defun rebar-find-project-default ()
  (or (locate-dominating-file default-directory #'rebar-directory-p)
      (rebar-vc-root default-directory)
      (read-directory-name "Directory: " nil nil t)))

;;;###autoload
(defun rebar-directory-p (dir)
  (or (file-exists-p (expand-file-name "rebar.config" dir))
      (file-exists-p (expand-file-name "rebar.config.script" dir))
      (file-regular-p (expand-file-name "rebar" dir))))

(defun rebar-vc-root (&optional file-or-directory)
  (when rebar-use-vc-root
    (ignore-errors
      (let* ((vc-handled-backends (if (listp rebar-use-vc-root)
                                      rebar-use-vc-root
                                    vc-handled-backends))
             (file (or file-or-directory buffer-file-name default-directory))
             (backend (vc-responsible-backend file)))
        (and backend (vc-call-backend backend 'root file))))))

(defvar-local rebar-project-root nil "Internal")

;;;###autoload
(defun rebar-project-root ()
  (or rebar-project-root
      (setq rebar-project-root (funcall rebar-find-project-function))))

(defmacro rebar-ensure-directory (&rest body)
  `(let ((default-directory (file-name-as-directory (rebar-project-root))))
     ,@body))

(defvar rebar-compilation-finish-functions nil)

(define-compilation-mode rebar-compilation-mode "Rebar" nil
  (rebar-mode 1))

(defun rebar-start (&rest cmds)
  (rebar-ensure-directory
   (let* ((cmd (concat (rebar-bin) " "
                       (mapconcat 'identity (cl-remove-if #'null cmds) " ")))
          (cmd (if (string-match-p "skip_deps\\|help\\|-deps" cmd)
                   cmd
                 (concat cmd " skip_deps=true"))))
     (compilation-start cmd 'rebar-compilation-mode))))

;;;###autoload
(defun rebar-install-config ()
  (interactive)
  (rebar-ensure-directory
   (and (file-exists-p "rebar.config.sample")
        (error "File rebar.config.sample already exists"))
   (with-temp-buffer
     (url-insert-file-contents rebar-config-url)
     (write-region (point-min) (point-max) "rebar.config.sample"
                   nil nil nil 'ask))))

;;;###autoload
(defun rebar (cmd)
  "Run rebar CMD."
  (interactive (list (completing-read "Cmd: " rebar-cmds nil t)))
  (let ((fn (intern-soft (format "rebar-%s" cmd))))
    (cond
     ((commandp fn) (call-interactively fn))
     ((equal cmd "version")
      (message "%s" (rebar-strip-string
                     (shell-command-to-string "rebar version"))))
     (t (rebar-start cmd)))))

(defun rebar-help (cmd)
  (interactive (list (completing-read "Help on cmd: " rebar-cmds)))
  ;; To avoid asking for project root.
  (let ((rebar-project-root default-directory)
        (compilation-scroll-output nil))
    (rebar-start "help" cmd)))

(defun rebar-create--read-id (prompt)
  (rebar-ensure-directory
   (let ((default (file-name-nondirectory
                    (directory-file-name default-directory))))
     (rebar-read-string prompt nil nil default))))

;;;###autoload
(defun rebar-create (template &rest vars)
  (interactive
   (let* ((template-id (completing-read "Template: " rebar-templates))
          (variables (cdr (assoc template-id rebar-templates)))
          (answers (mapcar
                    (lambda (var)
                      (let ((ans (rebar-create--read-id (capitalize var))))
                        (when (and ans (not (equal ans "")))
                          (format "%s=%s" var ans))))
                    variables)))
     `(,template-id ,@answers)))
  (apply #'rebar-start "create" (concat "template=" template) vars))

;;;###autoload
(defun rebar-create-app (appid)
  (interactive (list (rebar-create--read-id "Appid")))
  (rebar-start "create-app" (concat "appid=" appid)))

;;;###autoload
(defun rebar-create-lib (libid)
  (interactive (list (rebar-create--read-id "Libdir")))
  (rebar-start "create-lib" (concat "libid=" libid)))

;;;###autoload
(defun rebar-create-node (nodeid)
  (interactive (list (rebar-create--read-id "Nodeid")))
  (rebar-start "create-node" (concat "nodeid=" nodeid)))

;;;###autoload
(defun rebar-compile (&optional clean)
  (interactive "P")
  (rebar-start (and clean "clean") "compile"))

(defvar rebar-build-command "get-deps update-deps compile skip_deps=false")

;;;###autoload
(defun rebar-build (&optional edit)
  "Build the project."
  (interactive "P")
  (when edit
    (let ((new-build (read-string "Build command: " rebar-build-command)))
      (if (equal new-build "")
          (message "Empty value ignored; build command unchanged")
        (setq rebar-build-command new-build))))
  (rebar-start rebar-build-command))

(defun rebar-read-term ()
  (let* ((to-term (lambda (beg len)
                    (prog1 (erlext-binary-to-term
                            (buffer-substring beg (+ beg len)))
                      (forward-char len))))
         (size (prog1 (get-byte) (forward-char 1)))
         (term (funcall to-term (point) size)))
    ;; Could use mcase but it introduces too many compiler warnings.
    (if (and (vectorp term) (eq (aref term 0) '$size))
        (funcall to-term (point) (aref term 1))
      term)))

(defvar rebar-coverdata nil)

;; See cover:do_import_to_table for details of the return value.
(defun rebar-read-coverdata (coverdata)
  (or (fboundp 'erl-binary-to-term)
      (error "Library `erlext' required to parse cover data"))
  (when (file-exists-p coverdata)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents coverdata)
      (setq rebar-coverdata
            (cl-loop while (not (eobp))
                     collect (rebar-read-term))))))

(defun rebar-eunit-outdir ()
  (let ((dir (locate-dominating-file default-directory ".eunit")))
    (and dir (expand-file-name ".eunit" dir))))

(defun rebar-load-coverdata ()
  (rebar-read-coverdata (expand-file-name "eunit.coverdata" (rebar-eunit-outdir))))

(defun rebar-covered-lines (module)
  ;; Return a list of (LINE HITS).
  (when module
    (cl-flet ((aref-safe (object idx)
                         (ignore-errors (aref object idx))))
      (cl-loop for term in rebar-coverdata
               when (and (eq (aref-safe (aref-safe term 0) 0) 'bump)
                         (eq (aref-safe (aref-safe term 0) 1) module))
               collect (list (aref-safe (aref-safe term 0) 5)
                             (aref-safe term 1))))))

(defun rebar-cover-browse (&optional view-index)
  (interactive "P")
  (let* ((outdir (or (rebar-eunit-outdir) (user-error "No .eunit directory")))
         (html (expand-file-name (if (or (not buffer-file-name) view-index)
                                     "index.html"
                                   (concat (file-name-sans-extension
                                            (file-name-nondirectory buffer-file-name))
                                           ".COVER.html"))
                                 outdir)))
    (if (file-exists-p html)
        (browse-url html)
      (user-error "File `%s' does not exist" html))))

(unless (fringe-bitmap-p 'centered-vertical-bar)
  (define-fringe-bitmap 'centered-vertical-bar [24] nil nil '(top t)))

(defun rebar-cover-annotate ()
  "Annotate current buffer with coverage data from eunit.

Needs these entries in rebar.config:
  {cover_enabled,true}.
  {cover_export_enabled,true}."
  (interactive (ignore (or rebar-coverdata (rebar-load-coverdata))))
  (or rebar-coverdata (user-error "Cover data not available"))
  (if (eq (next-single-char-property-change (point-min) 'rebar-cover)
          (point-max))
      (let* ((module (and buffer-file-name
                          (intern-soft
                           (file-name-sans-extension
                            (file-name-nondirectory buffer-file-name)))))
             (data (or (rebar-covered-lines module)
                       (user-error "No coverage information for `%s'" module)))
             (count (cl-count-if-not #'zerop data :key #'cadr)))
        (save-excursion
          (save-restriction
            (widen)
            (dolist (d data)
              (pcase d
                (`(,(and line (guard (< 0 line))) ,hits)
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (let ((face (if (zerop hits) 'error 'success))
                       (o (make-overlay (line-beginning-position)
                                        (line-end-position))))
                   (overlay-put o 'rebar-cover t)
                   (unless (zerop hits)
                     (overlay-put o 'help-echo (format "Hits: %d" hits)))
                   (overlay-put o 'before-string
                                (apply #'propertize "|"
                                       (if (zerop (car (window-fringes)))
                                           ;; No left fringe
                                           `(face ,face)
                                         `(display (left-fringe
                                                    centered-vertical-bar
                                                    ,face)))))))))))
        (message "%d%% covered" (/ (* count 100) (length data))))
    (remove-overlays nil nil 'rebar-cover t)))

(defvar-local rebar-test-suite nil)

(defun rebar-set-test-suite ()
  (let* ((suite (when buffer-file-name
                  (file-name-nondirectory
                   (if (string-match "\\(?:_tests\\|_SUITE\\).erl" buffer-file-name)
                       (substring buffer-file-name 0 (match-beginning 0))
                     (file-name-sans-extension buffer-file-name)))))
         (suites (rebar-read-string "Test suites: " nil nil suite)))
    (setq rebar-test-suite
          (unless (equal suites "")
            (format "suites=%S" (substring-no-properties suites))))))

;;;###autoload
(defun rebar-eunit (&optional test-suite)
  (interactive "P")
  (and test-suite (rebar-set-test-suite))
  (let ((rebar-compilation-finish-functions rebar-compilation-finish-functions))
    (when (fboundp 'erlext-binary-to-term)
      (add-hook 'rebar-compilation-finish-functions
                (lambda (_buf _msg)
                  (when (save-excursion
                          (goto-char (point-max))
                          (re-search-backward "^Coverdata export: ?\\(.*\\)$" nil t))
                    (rebar-read-coverdata (match-string 1))
                    (message "%s" (concat (current-message)
                                          "; rebar cover data updated"))))))
    (rebar-start "eunit" rebar-test-suite)))

;;;###autoload
(defun rebar-ct (&optional test-suite)
  (interactive "P")
  (and test-suite (rebar-set-test-suite))
  (rebar-start "ct" rebar-test-suite))

(defvar rebar-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-A" 'rebar-cover-annotate)
    (define-key m "\M-B" 'rebar-build)
    (define-key m "\M-H" 'rebar-help)
    (define-key m "\M-K" 'rebar-compile)
    (define-key m "\M-N" 'rebar-create)
    (define-key m "\M-R" 'rebar)
    (define-key m "\M-T" 'rebar-ct)
    (define-key m "\M-U" 'rebar-eunit)
    (define-key m "\M-W" 'rebar-cover-browse)
    m))

;;;###autoload
(define-minor-mode rebar-mode nil
  :lighter (:eval (if global-rebar-mode "" " RB")))

;;;###autoload
(define-minor-mode global-rebar-mode nil
  :lighter " GRB" :global t :keymap rebar-mode-map
  (if global-rebar-mode
      (add-hook 'minibuffer-setup-hook #'rebar-minibuffer-setup-function)
    (remove-hook 'minibuffer-setup-hook #'rebar-minibuffer-setup-function)))

(defun rebar-minibuffer-setup-function ()
  ;; Disable the keymap in minibuffer.
  (set (make-local-variable 'global-rebar-mode) nil))

(defvar rebar-menu-items '(("Rebar"
                            (("Run Command" rebar)
                             ("Build" rebar-build)
                             ("Compile" rebar-compile)
                             ("Create Application" rebar-create-app)
                             ("Create" rebar-create)
                             ("EUnit" rebar-eunit)
                             ("Common Test" rebar-ct)
                             ("Cover annotate buffer" rebar-cover-annotate)
                             ("Browse .COVER.html file" rebar-cover-browse)
                             ("Command help" rebar-help))))
  "See `erlang-menu-base-items' for documentation.")

(defun rebar-install-erlang-menu ()
  (with-no-warnings
    (setq erlang-menu-items
          (erlang-menu-add-above 'rebar-menu-items
                                 'erlang-menu-compile-items
                                 erlang-menu-items))
    (erlang-menu-init))
  (remove-hook 'erlang-load-hook 'rebar-install-erlang-menu))

(if (featurep 'erlang)
    (rebar-install-erlang-menu)
  (add-hook 'erlang-load-hook 'rebar-install-erlang-menu))

(provide 'rebar)
;;; rebar.el ends here

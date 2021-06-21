(use-package flycheck-clj-kondo
  :straight t
  :ensure t)

(use-package clj-refactor
  :straight t
  :config
  (setq cljr-warn-on-eval nil))

(use-package cider
  :straight t
  :init
  (setq cider-auto-jump-to-error nil
        cider-auto-select-error-buffer nil
        cider-enhanced-cljs-completion-p nil
        nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-print-length 100
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup)
        cider-repl-pop-to-buffer-on-connect 'display-only)
  :config
  (add-hook 'cider-repl-mode-hook #'electric-pair-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(use-package clojure-mode
  :straight t
  :mode ("\\.cljg\\'")
  :bind (("C-x e" . cider-pprint-eval-last-sexp-to-comment))
  :config
  (require 'flycheck-clj-kondo)
  (define-clojure-indent
    (render 1)
    (match 1)
    (s/fdef 1)
    (dom/div 1)
    (let-if 1))
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'flycheck-mode)

  (setq clj-refactor--key-binding-prefixes
        '(("mr" . "refactor")
          ("mra" . "add")
          ("mrc" . "cycle/clean/convert")
          ("mrd" . "destructure")
          ("mre" . "extract/expand")
          ("mrf" . "find/function")
          ("mrh" . "hotload")
          ("mri" . "introduce/inline")
          ("mrm" . "move")
          ("mrp" . "project/promote")
          ("mrr" . "remove/rename/replace")
          ("mrs" . "show/sort/stop")
          ("mrt" . "thread")
          ("mru" . "unwind/update")))

  (defun cider-connect-godot ()
    (interactive)
    (cider-connect '(:host "localhost" :port 3722)))

  (defun cider-restart-godot ()
    (interactive)
    (cider-interactive-eval "(-> (Godot.Engine/GetMainLoop) .Root .GetTree .ReloadCurrentScene)")
    (cider-interactive-eval "(arcadia.internal.variables/set-variables-edn!)")
    (cider-interactive-eval "(arcadia.internal.variables/generate-variables!)")
    (cider-interactive-eval "(arcadia.internal.variables/connect-variables!)"))

  (mode-leader-def
    'normal clojure-mode-map

    "'" 'cider-jack-in-clj
    ";" 'cider-jack-in-bb
    "\"" 'cider-jack-in-cljs

    "h" '(:ignore t :which-key "Help")
    "ha" 'cider-apropos
    "hc" 'cider-cheatsheet
    "hd" 'cider-clojuredocs
    "hh" 'cider-doc
    "hj" 'cider-javadoc
    "hn" 'cider-browse-ns
    "hN" 'cider-browse-ns-all
    "hs" 'cider-browse-spec
    "hS" 'cider-browse-spec-all

    "hp" 'hiccup-paste--paste-as-hiccup

    "e" '(:ignore t :which-key "Evaluate")
    "e;" 'cider-eval-defun-to-comment
    "eb" 'cider-eval-buffer
    "ee" 'cider-eval-last-sexp
    "ef" 'cider-eval-defun-at-point
    "ei" 'cider-interrupt
    "em" 'cider-macroexpand-1
    "eM" 'cider-macroexpand-all
    "en" 'cider-ns-refresh
    "eN" 'cider-ns-reload
    "er" 'cider-eval-region
    "eu" 'cider-undef
    "ev" 'cider-eval-sexp-at-point
    "eV" 'cider-eval-sexp-up-to-point
    "ew" 'cider-eval-last-sexp-and-replace

    "ep" '(:ignore t :which-key "pprint")
    "ep;" 'cider-pprint-eval-defun-to-comment
    "ep:" 'cider-pprint-eval-last-sexp-to-comment
    "epf" 'cider-pprint-eval-defun-at-point
    "epe" 'cider-pprint-eval-last-sexp

    "=" '(:ignore t :which-key "Format")
    "==" 'cider-format-buffer
    "=f" 'cider-format-defun
    "=r" 'cider-format-region

    "=e" '(:ignore t :which-key "Edn")
    "=eb" 'cider-format-edn-buffer
    "=ee" 'cider-format-edn-last-sexp
    "=er" 'cider-format-edn-region

    "g" '(:ignore t :which-key "Goto")
    "gb" 'cider-pop-back
    "gc" 'cider-classpath
    "ge" 'cider-jump-to-compilation-error
    "gn" 'cider-find-ns
    "gs" 'cider-browse-spec
    "gS" 'cider-browse-spec-all
    "gr" 'cider-restart-godot
    "gg" 'cider-connect-godot

    "m" '(:ignore t :which-key "Manage Cider")
    "mj" 'cider-connect-sibling-clj
    "ms" 'cider-connect-sibling-cljs

    "s" '(:ignore t :which-key "Send Code")
    "sb" 'cider-load-buffer
    "sL" 'cider-find-and-clear-repl-output
    "so" 'cider-repl-switch-to-other
    "su" 'cider-repl-require-repl-utils


    "sc" '(:ignore t :which-key "Connect")
    "scj" 'cider-connect-clj
    "scm" 'cider-connect-clj&cljs
    "scs" 'cider-connect-cljs


    "sj" '(:ignore t :which-key "Jack in")
    "sjj" 'cider-jack-in-clj
    "sjm" 'cider-jack-in-clj&cljs
    "sjs" 'cider-jack-in-cljs

    "sq" '(:ignore t :which-key "Quit / Reload")
    "sqq" 'cider-quit
    "sqr" 'cider-restart
    "sqn" 'cider-ns-reload
    "sqN" 'cider-ns-reload-all

    "T" '(:ignore t :which-key "Toggle")
    "Te" 'cider-enlighten-mode
    "Tt" 'cider-auto-test-mode

    "t" '(:ignore t :which-key "Cider Tests")
    "tb" 'cider-test-show-report
    "tt" 'cider-test-run-project-tests

    "d" '(:ignore t :which-key "Cider Debug")
    "db" 'cider-debug-defun-at-point
    "dv" 'cider-inspect

    "p" '(:ignore t :which-key "Cider Profile")
    "p+" 'cider-profile-samples
    "pc" 'cider-profile-clear
    "pn" 'cider-profile-ns-toggle
    "ps" 'cider-profile-var-summary
    "pS" 'cider-profile-summary
    "pt" 'cider-profile-toggle
    "pv" 'cider-profile-var-profiled-p

    "r" '(:ignore t :which-key "Refactor")
    "r?"  #'cljr-describe-refactoring

    "ra" '(:ignore t :which-key "Add")
    "rad"  #'cljr-add-declaration
    "rai"  #'cljr-add-import-to-ns
    "ram"  #'cljr-add-missing-libspec
    "rap"  #'cljr-add-project-dependency
    "rar"  #'cljr-add-require-to-ns
    "ras"  #'cljr-add-stubs
    "rau"  #'cljr-add-use-to-ns

    "rc" '(:ignore t :which-key "Cycle")
    "rci"  #'clojure-cycle-if
    "rcn"  #'lsp-clojure-clean-ns
    "rcp"  #'clojure-cycle-privacy
    "rcs"  #'cljr-change-function-signature
    "rct"  #'cljr-cycle-thread

    "rdk"  #'cljr-destructure-keys

    "re" '(:ignore t :which-key "Add")
    "rec"  #'cljr-extract-constant
    "red"  #'cljr-extract-def
    "ref"  #'cljr-extract-function
    "rel"  #'lsp-clojure-expand-let

    "rf" '(:ignore t :which-key "Fn")
    "rfe"  #'cljr-create-fn-from-example
    "rfu"  #'cljr-find-usages

    "rhd"  #'cljr-hotload-dependency

    "ri" '(:ignore t :which-key "Inline")
    "ril"  #'cljr-introduce-let
    "ris"  #'cljr-inline-symbol

    "rm" '(:ignore t :which-key "Move")
    "rmf"  #'cljr-move-form
    "rml"  #'lsp-clojure-move-to-let

    "rp" '(:ignore t :which-key "Project")
    "rpc"  #'cljr-project-clean
    "rpf"  #'cljr-promote-function

    "rr" '(:ignore t :which-key "Rename")
    "rrf"  #'cljr-rename-file-or-dir
    "rrl"  #'cljr-remove-let
    "rrm"  #'cljr-require-macro
    "rrs"  #'cljr-rename-symbol

    "l" '(:ignore t :which-key "Lsp")
    "ld" #'lsp-find-definition
    "lr" #'lsp-find-references
    "lr" #'lsp-ui-peek-find-references

    "rs" '(:ignore t :which-key "Show")
    "rsc"  #'cljr-show-changelog
    "rsp"  #'cljr-sort-project-dependencies
    "rsr"  #'cljr-stop-referring

    "rt" '(:ignore t :which-key "Thread")
    "rtf"  #'clojure-thread-first-all
    "rth"  #'clojure-thread
    "rtl"  #'clojure-thread-last-all

    "ru" '(:ignore t :which-key "Unwind")
    "rua"  #'lsp-clojure-unwind-all
    "rup"  #'cljr-update-project-dependencies
    "ruw"  #'clojure-unwind
    "bb" #'cider-switch-to-repl-buffer))

;; Babashka

(defvar bb-repl nil)

(defun bb-repl-sentinel (process event)
  (when (equal event "hangup\n")
    (setq bb-repl nil)))

(defun cider-jack-in-bb ()
  "Test."
  (interactive)
  (when (not bb-repl)
    (let* ((deps-location (locate-dominating-file default-directory "deps.edn"))
           (default-directory (or deps-location default-directory))
           (proc (start-process "BB Repl" "*bb-repl*" "bb" "--nrepl-server" "--classpath" (shell-command-to-string "clojure -Spath"))))
      (set-process-sentinel proc 'bb-repl-sentinel)
      (setq bb-repl proc)))
  (cider-connect-clj '(:host "127.0.0.1" :port 1667)))

;; TODO remove once on Melpa

(defgroup hiccup-cli nil
  "Hiccup-cli group."
  :prefix "hiccup-cli-"
  :group 'tools)

(defcustom hiccup-cli-custom-path-to-bin
  (or (executable-find "hiccup-cli")
      "hiccup-cli")
  "Custom path to the hiccup-cli executable."
  :type 'file
  :group 'hiccup-cli)

(defcustom hiccup-cli-custom-path-to-tmp-file
  "/tmp/hiccup-cli"
  "Custom path to the hiccup-cli tmp file."
  :type 'file
  :group 'hiccup-cli)

(defun hiccup-cli--clipboard-string ()
  "Return the currency value of the clipboard as a string."
  (let ((clipboard-text (gui--selection-value-internal 'CLIPBOARD))
	(select-enable-clipboard t))
    (if (and clipboard-text (> (length clipboard-text) 0))
	(kill-new clipboard-text))
    (car kill-ring)))

(defun hiccup-cli--write-to-tmp-file (string)
  "Write STRING to `hiccup-cli--custom-path-to-tmp-file`."
  (write-region string nil hiccup-cli-custom-path-to-tmp-file))

(defun hiccup-cli--insert ()
  "Insert converted Hiccup from `hiccup-cli--custom-path-to-tmp-file` into buffer."
  (save-excursion
    (insert
     (shell-command-to-string
      (format "%s --html-file %s"
              hiccup-cli-custom-path-to-bin
              hiccup-cli-custom-path-to-tmp-file)))))

;;;###autoload
(defun hiccup-cli-paste-as-hiccup ()
  "Paste the HTML in your clipboard as Hiccup syntax."
  (interactive)
  (hiccup-cli--write-to-tmp-file (hiccup-cli--clipboard-string))
  (hiccup-cli--insert))

;;;###autoload
(defun hiccup-cli-region-as-hiccup (start end)
  "Replace the HTML in your selected START END region with Hiccup syntax."
  (interactive "r")
  (if (use-region-p)
      (let ((region-str (buffer-substring start end)))
        (kill-region start end)
        (hiccup-cli--write-to-tmp-file region-str)
        (hiccup-cli--insert))))

;;;###autoload
(defun hiccup-cli-yank-as-hiccup ()
  "Paste the HTML in your `kill-ring` as Hiccup syntax."
  (interactive)
  (hiccup-cli--write-to-tmp-file (substring-no-properties (car kill-ring)))
  (hiccup-cli--insert))

(provide 'lang-clojure)

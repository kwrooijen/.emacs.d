;;==============================================================================
;;== Keys
;;==============================================================================

(defun rust-keys-hook ()
  (define-prefix-command 'c-map)
  (define-key rust-mode-map (kbd "C-c") 'c-map)
  (define-key c-map (kbd "C-c C-t")
    (lambda()
      (interactive)
      (async-shell-command "cargo test" "[Cargo test]")))
  (define-key c-map (kbd "C-c C-s")
    (lambda()
      (interactive)
      (async-shell-command "cargo test" "[Cargo test]")))
  (define-key c-map (kbd "C-c C-b")
    (lambda()
      (interactive)
      (async-shell-command "cargo bench" "[Cargo bench]")))
  (define-key c-map (kbd "C-c C-l")
    (lambda()
      (interactive)
      (async-shell-command "cargo clean" "[Cargo clean]")))
  (define-key c-map (kbd "C-c C-u")
    (lambda()
      (interactive)
      (async-shell-command "cargo update" "[Cargo update]")))
  (define-key c-map (kbd "C-c C-c")
    (lambda()
      (interactive)
      (async-shell-command "cargo run" "[Cargo run]")))
  (define-key rust-mode-map (kbd "C-c C-l")
    (lambda()
      (interactive)
      (async-shell-command "cargo run" "[Cargo Run]"))))

;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'rust-mode-hook (lambda ()
    (rainbow-delimiters-mode)
    (setq-local tab-width 4)
    (rust-keys-hook)
    (setq-local helm-dash-docsets '("Rust"))))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun execute-rust ()
  (interactive)
  (if (buffer-file-name)
      (let ((result (shell-command-to-string (format "rustc %s ; echo $?" (buffer-file-name))))
            (origin (buffer-name)))
        (if (equal (get-return-code result) "0")
            (async-shell-command (format " ./%s" (file-name-sans-extension (buffer-name))) "[Rust Compile]")
          (progn
            (async-shell-command "" "[Rust Compile]")
            (switch-to-buffer "[Rust Compile]")
            (insert result)
            (switch-to-buffer origin))))))

(defun test-rust ()
  (interactive)
  (if (buffer-file-name)
      (let ((result (shell-command-to-string (format "rustc --test %s ; echo $?" (buffer-file-name))))
            (origin (buffer-name)))
        (if (equal (get-return-code result) "0")
            (async-shell-command (format " ./%s" (file-name-sans-extension (buffer-name))) "[Rust Compile]")
          (progn
            (async-shell-command "" "[Rust Compile]")
            (switch-to-buffer "[Rust Compile]")
            (insert result)
            (switch-to-buffer origin))))))

(provide 'attic-rust)

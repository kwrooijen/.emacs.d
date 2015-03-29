;;==============================================================================
;;== Hook
;;==============================================================================

(add-hook 'haskell-mode-hook (lambda ()
    (fix-tabs 4)
    (rainbow-delimiters-mode)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indentation)
    (setq-local doom-indent-fallback t)
    (setq-local helm-dash-docsets '("Haskell"))))

;;==============================================================================
;;== Functions
;;==============================================================================

(defun run-haskell-test ()
    (interactive)
    (my-up-to-script "*.cabal" "cabal build ; cabal test --log=/dev/stdout" "[Haskell Tests]")) 

(defun hoogle-search (query)
    "Search with hoogle commandline"
    (interactive "sHoogle query: ")
    (if (get-buffer "*Hoogle*") (kill-buffer "*Hoogle*"))
    ; get the version of hoogle so I don't have to manually adjust it for each update
    (shell-command (format "version=`hoogle --version | head -n 1 | awk '{print $2}' |
        cut -c 2- | rev | cut -c 2- | rev`;
        data=\"/databases\";
        two=$version$data;
        hoogle \"%s\" --data=$HOME/.lazyVault/sandboxes/hoogle/cabal/share/hoogle-$two" query))
    (switch-to-buffer "*Shell Command Output*")
    (rename-buffer "*Hoogle*")
    (haskell-mode)
    (linum-mode 0)
    (previous-buffer))

(provide 'my-haskell)

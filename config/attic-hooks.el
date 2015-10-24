;; Hooks

(defun fix-tabs (x)
  (indy-mode t)
  (setq-local tab-width x))

(add-hook 'sauron-mode-hook 'no-split)

(add-hook 'isearch-mode-hook (lambda()
                               (key-chord-mode 1)))

(setq clean-on-save t)
(add-hook 'before-save-hook (lambda ()
                              (when clean-on-save (whitespace-cleanup))))
(add-hook 'c-initialization-hook 'c-keys-hook)
(add-hook 'dired-mode-hook 'ensure-buffer-name-begins-with-exl)
(add-hook 'message-mode-hook (lambda () (electric-pair-mode 0)))

(add-hook 'racket-repl-mode-hook (lambda ()
    (define-key ac-complete-mode-map (kbd "<return>")
      (lambda() (interactive) (ac-stop) (racket-repl-eval-or-newline-and-indent)))))

;; Dear god what was I thinking? I'm supposed to be writing Lisp!!!!!!!
(setq indy-rules '(
    (all . (
        ((and (indy--prev 'indy--ends-on "[") (indy--current 'indy--starts-with "]")) (indy--prev-tab))
        ((and (indy--prev 'indy--ends-on "{") (indy--current 'indy--starts-with "}")) (indy--prev-tab))
        ((and (indy--prev 'indy--ends-on "(") (indy--current 'indy--starts-with ")")) (indy--prev-tab))

        ((indy--current 'indy--starts-with "]" "}" ")") (indy--prev-tab -1))
        ((indy--prev 'indy--ends-on "[" "{" "(")  (indy--prev-tab 1))
        ((indy--prev 'indy--ends-on ",")          (indy--prev-tab))
    ))
    (erlang-mode . (
        ((indy--prev 'indy--ends-on "->" "fun" "of" "begin") (indy--prev-tab 1))
        ((indy--prev 'indy--ends-on ";") (indy--prev-tab -1))
        ((and (indy--prev 'indy--ends-on "end") (indy--current 'indy--starts-with "end")) (indy--prev-tab -1))
        ((indy--current 'indy--ends-on "end") (indy--prev-tab -1))
    ))
    (haskell-mode . (
        ((indy--prev 'indy--starts-with ",") (indy--prev-tab))
        ((indy--prev 'indy--starts-with "[") (indy--prev-tab))
        ((indy--prev 'indy--ends-on "=" "= do" "=do") (indy--prev-tab 1))
    ))
    (elm-mode . (
        ((indy--prev 'indy--starts-with ",") (indy--prev-tab))
        ((and (indy--current 'indy--starts-with ", ") (indy--prev 'indy--starts-with "[")) (indy--prev-tab))
        ((indy--prev 'indy--ends-on "=") (indy--prev-tab 1))
        ((and (indy--current 'indy--starts-with ", ") (or (indy--prev 'indy--starts-with ", ") (indy--prev 'indy--starts-with "[ ") )) (indy--prev-tab))
        ((indy--current 'indy--starts-with "|> ") (indy--prev-tab))
        ((indy--prev 'indy--ends-on "if") (indy--prev-tab 1))
        ((and (indy--prev 'indy--starts-with "{") (indy--current 'indy--starts-with "," "}")) (indy--prev-tab))
        ((and (indy--prev 'indy--starts-with "[") (indy--current 'indy--starts-with "," "]")) (indy--prev-tab))
    ))
    (elixir-mode . (
        ((and (indy--prev 'indy--ends-on ") ->") (indy--current 'indy--starts-with "end")) (indy--prev-tab))
        ((indy--prev 'indy--ends-on ") ->") (indy--prev-tab 1))
    ))
))

;; Load mode on certain file extensions
(setq auto-mode-alist (append '(
    ("\\.less\\'"    . css-mode)
    ("\\.scss\\'"    . css-mode)
    ("Gemfile$"      . ruby-mode)
    ("Rakefile$"     . ruby-mode)
    ("\\.gemspec$"   . ruby-mode)
    ("\\.rake$"      . ruby-mode)
    ("\\.rb$"        . ruby-mode)
    ("\\.ru$"        . ruby-mode)
    ("\\.app.src\\'" . erlang-mode)
    ("rebar.config"  . erlang-mode)
    ("\\.js\\'"      . js2-mode)
    ("\\.dtl\\'"     . web-mode)
    ("\\.eex\\'"     . web-mode)
    ("\\.erb\\'"     . web-mode)
    ("\\.tpl\\'"     . web-mode)
    ) auto-mode-alist))

(provide 'attic-hooks)

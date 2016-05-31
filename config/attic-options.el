;; Font for X
(setq default-frame-alist
      '((font . "-CTDB-Fira Mono-bold-normal-normal-*-14-*-*-*-m-0-iso10646-1")
        (vertical-scroll-bars . nil)
        (line-spacing . 0)))

;; Disable Tool bar
(tool-bar-mode 0)
;; Disable Menu bar
(menu-bar-mode 0)
;; Disable Scroll bar
(scroll-bar-mode 0)
;; Disable Blinking cursor
(blink-cursor-mode 0)

;; Don't copy to clipboard using X please.
(setq x-select-enable-clipboard nil)

;; Personal Info
(setq user-full-name "Kevin W. van Rooijen")
(setq user-mail-address "kevin.van.rooijen@attichacker.com")


;; y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)
;; Allow upcase-region and downcase-region functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Scroll all the way to the bottom with C-v
(setq scroll-error-top-bottom t)

;; Show nothing in *scratch* when started
(setq initial-scratch-message nil)
;; Hide startup message
(setq inhibit-startup-message t)

;; C SOURCE
;; Default alignment should be 80
(setq fill-column 80)
;; Smooth Scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-conservatively 10000
      scroll-step 1)
(setq auto-window-vscroll nil)
;; Let C-v M-v brings back where you were.
(setq scroll-preserve-screen-position t)
;; Don't use tabs
(setq-default indent-tabs-mode nil)
;; Emacs temp directory
(setq temporary-file-directory "~/.emacs.d/tmp/")
;; Set default tab width to 4
(setq tab-width 4)



(provide 'attic-options)

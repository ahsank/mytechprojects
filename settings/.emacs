(setq column-number-mode t)

;; Turn on red highlighting for characters outside of the 80/100 char limit
(add-hook 'c++-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'java-mode-hook
          '(lambda () (font-lock-set-up-width-warning 100)))
(add-hook 'js-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'python-mode-hook
            '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'sawzall-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'sh-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))

(add-hook 'markdown-mode-hook
          '(lambda () (font-lock-set-up-width-warning 80)))

(global-visual-line-mode 1)
;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(defun copy-to-clipboard ()
  (interactive)
  (if (display-graphic-p)
    (progn
      (message "Yanked region to x-clipboard!")
      (call-interactively 'clipboard-kill-ring-save)
      )
    (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
    (progn
      (clipboard-yank)
      (message "graphics active")
      )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 81))

;; (global-set-key "\C-x~" 'set-80-columns)

(define-key global-map (kbd "C-x O") 'previous-multiframe-window)
;;(define-key global-map (kbd "M-n") 'other-window)

(add-to-list 'auto-mode-alist '("\\.javascript\\'" . javascript-mode))

;; Make the buffer attached to the window
(defun pin-buffer-to-window ()
  (interactive)
  (set-window-dedicated-p (selected-window) 1))

(global-auto-revert-mode t)
(show-paren-mode t)
(setq split-height-threshold 160)
(setq split-width-threshold 160)
(setq auto-save-default nil) 
;; Disable autmoatic scroll when cursor past top or bottom of the window.
(setq scroll-step 1)
(setq auto-window-vscroll nil)
;;(add-to-list 'custom-theme-load-path "~/src/emacs-color-theme-solarized")
;;(add-to-list 'custom-theme-load-path "~/src/replace-colorthemes")
;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (load-theme 'feng-shui t)
;; (require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
;;(color-theme-vim-colors)
;;(color-theme-dark-laptop)

;; Reverse colors for the border to have nicer line
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))

;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))

(global-set-key "\C-cg" 'goto-line)

(defun google-trim-trailing-whitespace ()
  "Remove trailing whitespace from buffer lines" 
  (interactive)
  (delete-trailing-whitespace))

;; (require 'color-theme-vim-colors)
;; (color-theme-vim-colors)

;; Set transparency of emacs
 (defun set-transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(setq make-backup-files nil)

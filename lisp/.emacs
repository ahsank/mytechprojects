(setq column-number-mode t)

;;  Hack for sbcl working with current version of emacs
;; Create a folder ~/.emacs.d/cl-lib and copy cl-lib.el from
;; https://raw.github.com/emacsmirror/cl-lib/master/cl-lib.el into this folder
(add-to-list 'load-path "~/.emacs.d/cl-lib")
(require 'cl-lib)

(push "/path-to-slime-folder/slime" load-path)

(setq slime-lisp-implementations
            `((sbcl ("/path-to-sbcl/bin/sbcl"))))
(setq slime-net-coding-system 'utf-8-unix)
(require 'slime-autoloads)
(slime-setup '(slime-asdf slime-fancy
                          slime-sprof slime-tramp inferior-slime
                          slime-banner))

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

(global-visual-line-mode 1)
;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;(slime-setup '(slime-asdf slime-fancy slime-highlight-edits
;                                                   slime-sprof slime-tramp inferior-slime
;                                                                            slime-fontifying-fu slime-banner))
(setq lisp-indent-hook 'common-lisp-indent-hook)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(defun faces_x ()
;; these are used when in X
(custom-set-faces
'(default ((t (:foreground "wheat" :background "black"))))
'(flyspell-duplicate ((t (:foreground "Gold3" :underline t :weight normal))))
'(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t :weight normal))))
'(font-lock-comment-face ((t (:foreground "SteelBlue1"))))
'(font-lock-function-name-face ((t (:foreground "gold"))))
'(font-lock-keyword-face ((t (:foreground "springgreen"))))
'(font-lock-type-face ((t (:foreground "PaleGreen"))))
'(font-lock-variable-name-face ((t (:foreground "Coral"))))
'(menu ((((type x-toolkit)) (:background "light slate gray" :foreground "wheat" :box (:line-width 2 :color "grey75" :style released-button)))))
'(mode-line ((t (:foreground "black" :background "light slate gray"))))
'(tool-bar ((((type x w32 mac) (class color)) (:background "midnight blue" :foreground "wheat" :box (:line-width 1 :style released-button))))))
(set-cursor-color "deep sky blue")
(set-foreground-color "wheat")
(set-background-color "black")
(set-face-foreground 'default "wheat")
(set-face-background 'default "black"))
(defun faces_nox ()
;; these are used when in terminal
(custom-set-faces
'(default ((t (:foreground "white" :background "black"))))
'(font-lock-comment-face ((t (:foreground "magenta"))))
'(font-lock-function-name-face ((t (:foreground "red"))))
'(font-lock-keyword-face ((t (:foreground "green"))))
'(font-lock-type-face ((t (:foreground "blue"))))
'(font-lock-string-face ((t (:foreground "cyan"))))
'(font-lock-variable-name-face ((t (:foreground "blue"))))
'(menu ((((type x-toolkit)) (:background "white" :foreground "black" :box (:line-width 2 :color "grey75" :style released-button)))))
'(modeline ((t (:foreground "blue" :background "white")))))
(set-cursor-color "blue")
(set-foreground-color "white")
(set-background-color "black")
(set-face-foreground 'default "white")
(set-face-background 'default "black"))

;;(if window-system
;;(faces_x)
;;(faces_nox))

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
  (set-window-width 80))

;; (global-set-key "\C-x~" 'set-80-columns)

(define-key global-map (kbd "M-p") 'previous-multiframe-window)
(define-key global-map (kbd "M-n") 'other-window)

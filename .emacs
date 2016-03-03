(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "apple" :family "Monaco")))))

;; Apply the PATH environment variable to Emacs and set the exec-path
;; http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-
;; path-when-it-runs-a-compile-command
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
      (replace-regexp-in-string "[[:space:]\n]*$" ""
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(setq swapping-buffer nil)
(setq swapping-window nil)
(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))
(global-set-key (kbd "C-c p") 'swap-buffers-in-windows)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(column-number-mode)
(setq ring-bell-function 'ignore)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'buffer-move)

(tool-bar-mode -1)

(desktop-save-mode 1)
(require 'desktop-menu)
(global-set-key "\C-xp" 'desktop-menu)

(put 'downcase-region 'disabled nil)

(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

; (require 'ess-site)
(setq ess-fancy-comments nil) ; just indent comments with code, thanks
(setq ess-default-style 'RStudio)

(global-set-key (kbd "<M-up>") 'scroll-up-line)
(global-set-key (kbd "<M-down>") 'scroll-down-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffers-menu-max-size 30)
 '(compilation-scroll-output (quote first-error))
 '(compile-command "cd ~/Products/upc-clang; make -j5 -k ")
 '(desktop-save-mode t)
 '(fill-column 100)
 '(indent-tabs-mode nil)
 '(magit-push-always-verify nil)
 '(session-use-package t nil (session))
 '(show-trailing-whitespace t))

(defun issue-link (numb)
  (interactive "sIssue number: ")
  (insert "[" numb "]" "(https://github.com/dc-js/dc.js/issues/" numb ")"))

(defun pr-link (numb)
  (interactive "sPR number: ")
  (insert "[" numb "]" "(https://github.com/dc-js/dc.js/pull/" numb ")"))

(global-set-key (kbd "M-I") 'issue-link)

(global-set-key (kbd "M-P") 'pr-link)

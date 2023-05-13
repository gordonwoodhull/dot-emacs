(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil))))

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

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)
(package-initialize)

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

(add-hook 'js2-mode-hook
      (lambda ()
        (when (string-match-p "metagraph" (buffer-file-name))
          (setq js2-additional-externs '("metagraph" "mg" "as_array" "as_keyvalue" "build_map")))))

(column-number-mode)
(setq ring-bell-function 'ignore)
(desktop-save-mode 0)


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

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(require 'buffer-move)

(tool-bar-mode -1)

(desktop-save-mode 1)
(require 'desktop-menu)
(global-set-key "\C-xp" 'desktop-menu)

(put 'downcase-region 'disabled nil)

(define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

(require 'ess-site)
(setq ess-fancy-comments nil) ; just indent comments with code, thanks
(setq ess-default-style 'RStudio)

(global-set-key (kbd "<M-up>") 'scroll-up-line)
(global-set-key (kbd "<M-down>") 'scroll-down-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)

(setq magit-last-seen-setup-instructions "1.4.0")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(buffers-menu-max-size 30)
 '(compilation-scroll-output (quote first-error))
 '(compile-command "cd ~/Products/upc-clang; make -j5 -k ")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(desktop-save-mode t)
 '(fill-column 100)
 '(indent-tabs-mode nil)
 '(magit-item-highlight-face nil)
 '(magit-push-always-verify nil)
 '(package-selected-packages
   (quote
    (elpy graphviz-dot-mode tuareg magit-gitflow poly-markdown jekyll-modes yaml-mode rjsx-mode web-mode js-doc typescript-mode json-mode markdown-mode js2-mode session multi-web-mode magit ess buffer-move)))
 '(session-use-package t nil (session))
 '(show-trailing-whitespace t))

(add-to-list 'session-mode-disable-list 'git-commit-mode)
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

(elpy-enable)
(setq elpy-rpc-python-command "python3")

;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here


(defun issue-link (numb)
  (interactive "sIssue number: ")
  (insert "[#" numb "]"
          (format "(https://github.com/%s/issues/%s)"
                  (replace-regexp-in-string
                   "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                   (magit-get "remote" (magit-get-remote) "url"))
                  numb)))


(defun pr-link (numb)
  (interactive "sPR number: ")
  (insert "[#" numb "]"
          (format "(https://github.com/%s/pull/%s)"
                  (replace-regexp-in-string
                   "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                   (magit-get "remote" (magit-get-remote) "url"))
                  numb)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-I") 'issue-link)
(global-set-key (kbd "M-P") 'pr-link)

(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)

; cancel minibuffer command when it loses focus
; http://stackoverflow.com/a/3024055/676195
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(setq-default c-basic-offset 4)

(setq js-doc-mail-address "gordon@woodhull.com"
       js-doc-author (format "Gordon Woodhull <%s>" js-doc-mail-address)
       js-doc-url "nononononononon"
       js-doc-license "who knows")

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/tomorrow-theme/GNU Emacs")

;; set font
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 12"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "royal blue"))))
 '(diff-changed ((t (:weight bold))))
 '(diff-refine-change ((t (:background "grey20"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red3"))))
 '(hl-line ((t (:background "#222"))))
 '(magit-diff-add ((t (:inherit diff-added))))
 '(magit-diff-del ((t (:inherit diff-removed))))
 '(magit-item-highlight ((t nil))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes (quote ("ca2d69f5dd853dbf6fbcf5d0f1759ec357fda19c481915431015417ec9c1fbd8" default)))
 '(dabbrev-case-replace nil)
 '(diff-switches "-u")
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(flyspell-auto-correct-binding [(control 39)])
 '(frame-background-mode (quote dark))
 '(global-auto-revert-mode t)
 '(global-ede-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(history-length 500)
 '(ibuffer-expert t)
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-prospects 128)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(lazy-highlight-initial-delay 0)
 '(recentf-max-saved-items 250)
 '(recentf-mode t)
 '(save-place t nil (saveplace))
 '(semantic-default-submodes (quote (global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode)))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-separator ":"))

(setq ido-ignore-buffers (cons "^\\*.*\\*$" ido-ignore-buffers))

;; misc configuration
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings 'meta)    ; enable windmove
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ring-bell-function 'ignore
      resize-mini-windows t)
;; enable disabled features
(put 'set-goal-column 'disabled nil)    ; C-x C-n
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l
(put 'narrow-to-region 'disabled nil)   ; C-x n n
(put 'scroll-left 'disabled nil)        ; C-x <

;; mappings
(defun revert-buffer-no-confirmation ()
  "Invoke `revert-buffer' without the confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " buffer-file-name)))
(defun kill-buffer-other-window ()
  "Kill buffer in other window"
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window -1))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirmation)
(global-set-key (kbd "<f6>") 'ack-and-a-half)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f11>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-!") 'kill-this-buffer)
(global-set-key (kbd "C-M-!") 'kill-buffer-other-window)
(global-set-key (kbd "S-SPC") 'dabbrev-expand)

;;;;;;;;;;;;;
;;; modes ;;;
;;;;;;;;;;;;;

;; dired
(setq dired-isearch-filenames t)
(add-hook 'dired-mode-hook (lambda ()
                             (dired-omit-mode 1)))

;; org-mode
(setq org-replace-disputed-keys t
      org-startup-indented t)
(add-hook 'org-mode-hook (lambda ()
                           (make-local-variable 'show-paren-mode)
                           (setq show-paren-mode nil)
                           (flyspell-mode)))

;; rst-mode
(add-hook 'rst-mode-hook (lambda () (flyspell-mode)))

(modify-syntax-entry ?_ "w" c-mode-syntax-table)
;;(modify-syntax-entry ?_ "w" python-mode-syntax-table)

;; Programming
(add-hook 'prog-mode-hook (lambda ()
                            (semantic-mode t)
                            (subword-mode t)
                            (flyspell-prog-mode)
                            (setq show-trailing-whitespace t)
                            (font-lock-add-keywords
                             nil
                             '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\)\\>" 1 font-lock-warning-face t)))
                            (global-set-key (kbd "C-<delete>") 'subword-kill)
                            (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)))

;; C/C++
(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(setq c-default-style "bsd"
      c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.x\\'" . c++-mode))

;; Python
(add-hook 'python-mode-hook (lambda () (flymake-mode)))
(require 'flymake)
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init))

(add-to-list 'auto-mode-alist '("SCons\\(truct\\|cript\\)\\'" . python-mode))

(global-set-key (kbd "S-<f7>") (lambda ()
                                 (interactive)
                                 (flymake-goto-prev-error)
                                 (message "%s"
                                          (flymake-ler-text (caar (flymake-find-err-info
                                                                   flymake-err-info
                                                                   (flymake-current-line-no)))))))
(global-set-key (kbd "S-<f8>") (lambda ()
                                 (interactive)
                                 (flymake-goto-next-error)
                                 (message "%s"
                                          (flymake-ler-text (caar (flymake-find-err-info
                                                                   flymake-err-info
                                                                   (flymake-current-line-no)))))))

;; ido-recentf
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(add-to-list 'load-path "~/.emacs.d/packages/iedit")
(autoload 'iedit-mode "iedit" nil t)
(global-set-key (kbd "C-;") 'iedit-mode)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups '(("default"
                                     ("Dired" (mode . dired-mode))
                                     ("C/C++" (or
                                               (mode . c-mode)
                                               (mode . c++-mode)))
                                     ("Python" (mode . python-mode))
                                     ("Elisp" (mode . emacs-lisp-mode))
                                     ("Docs" (or
                                              (mode . org-mode)
                                              (mode . rst-mode)))
                                     ("Misc" (name . "^\\*"))
                                     )))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; external packages ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; smex
(defun load-smex (original-function)
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/packages/smex")
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (funcall original-function))
(global-set-key (kbd "M-x") (lambda () (interactive) (load-smex 'smex)))
(global-set-key (kbd "M-X") (lambda () (interactive) (load-smex 'smex-major-mode-commands)))

;; anything
(add-to-list 'load-path "~/.emacs.d/packages/anything-config")
(setq anything-input-idle-delay 0)
(require 'anything-match-plugin)
(require 'anything-config)
(defun custom-anything ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+
     anything-c-source-recentf
     anything-c-source-files-in-current-dir+
     anything-c-source-occur
     anything-c-source-imenu
     anything-c-source-semantic
     anything-c-source-emacs-commands
     )
   " *custom-anything*"))
(global-set-key (kbd "C-x a") 'custom-anything)
(global-set-key (kbd "M-i") 'anything-imenu)
(global-set-key (kbd "M-s o") 'anything-occur)
(global-set-key (kbd "C-x f") 'anything-find-files)

;; drag-stuff
(add-to-list 'load-path "~/.emacs.d/packages/drag-stuff")
(setq drag-stuff-modifier '(meta shift))
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/packages/ace-jump-mode")
(autoload 'ace-jump-char-mode "ace-jump-mode" nil t)
(global-set-key (kbd "C-#") 'ace-jump-char-mode)

;; magit
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(autoload 'magit-status "magit" nil t)

;; minimap
(add-to-list 'load-path "~/.emacs.d/packages/minimap")
(autoload 'minimap-create "minimap" nil t)
(setq minimap-update-delay 0.1
      minimap-width-fraction 0.1)

;; YASnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; markdown-mode
(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; zencoding
(add-to-list 'load-path "~/.emacs.d/packages/zencoding")
(autoload 'zencoding-mode "zencoding-mode" nil t)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; ack-and-a-half
(add-to-list 'load-path "~/.emacs.d/packages/ack-and-a-half")
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)

;; expand-region
(add-to-list 'load-path "~/.emacs.d/packages/expand-region")
(autoload 'er/expand-region "expand-region" nil t)
(global-set-key (kbd "C-=") 'er/expand-region)

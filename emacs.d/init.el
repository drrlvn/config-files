;;; init.el --- emacs config
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'cl-macs)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'config-defuns-autoloads)

(setq package-check-signature nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(my/install-packages
 'anzu
 'atom-one-dark-theme
 'auto-compile
 'avy
 'bind-key
 'cmake-font-lock
 'cmake-mode
 'company
 'counsel
 'diff-hl
 'discover-my-major
 'drag-stuff
 'easy-kill
 'emmet-mode
 'expand-region
 'flycheck
 'git-messenger
 'git-timemachine
 'go-mode
 'guide-key
 'helm
 'highlight-symbol
 'iedit
 'magit
 'markdown-mode
 'multiple-cursors
 'paredit
 'popwin
 'powerline
 'projectile
 'protobuf-mode
 'rainbow-delimiters
 'restclient
 'rust-mode
 'spaceline
 'swiper
 'undo-tree
 'use-package
 'web-mode
 'whitespace-cleanup-mode
 'window-numbering
 'wrap-region
 'yaml-mode
 'yasnippet
 )

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'config-looks)

(use-package misc
  :commands (zap-to-char zap-up-to-char copy-from-above-command))

(bind-key "C-x r q" 'save-buffers-kill-emacs)
(unbind-key "C-x C-c")
(bind-key "<home>" 'my/smart-beginning-of-line)
(bind-key "<end>" 'end-of-line)
(bind-key "<escape>" 'keyboard-escape-quit)
(bind-key "<f5>" 'my/revert-buffer-no-confirmation)
(bind-key "<f6>" 'ag-project-at-point)
(bind-key "<f7>" 'previous-error)
(bind-key "<f8>" 'next-error)
(bind-key "<f11>" 'my/cleanup-buffer)
(bind-key "S-<f11>" 'whitespace-cleanup)
(bind-key "S-<f12>" (lambda () (interactive) (find-file user-init-file)))

(bind-key "M-<return>" 'my/open-line-below)
(bind-key "M-S-<return>" 'my/open-line-above)
(bind-key "C-<delete>" 'kill-word)
(bind-key "M-SPC" 'cycle-spacing)
(bind-key [remap kill-ring-save] 'easy-kill)

(bind-key "C-<tab>" 'next-buffer)
(bind-key "C-S-<iso-lefttab>" 'previous-buffer)

(bind-key "C-n" (lambda (n) (interactive "p") (scroll-up n)))
(bind-key "C-p" (lambda (n) (interactive "p") (scroll-down n)))
(bind-key "M-n" (lambda (n) (interactive "p") (scroll-other-window n)))
(bind-key "M-p" (lambda (n) (interactive "p") (scroll-other-window (- n))))

(bind-key "C-z" 'repeat)
(bind-key "C-!" 'kill-this-buffer)
(bind-key "C-M-!" 'my/kill-buffer-other-window)
(bind-key "C-#" 'quick-calc)
(bind-key "C-'" 'highlight-symbol-at-point)
(bind-key "C-," 'highlight-symbol-prev)
(bind-key "C-." 'highlight-symbol-next)
(bind-key "M-s o" 'highlight-symbol-occur)

(bind-key "C-c C-<return>" 'delete-blank-lines)
(bind-key "C-c n" 'my/cleanup-buffer)
(bind-key "C-c d" 'my/diff-current-buffer-with-file)
(bind-key "C-c r" 'my/rotate-windows)
(bind-key "C-c C-;" 'my/toggle-comment-line-or-region)
(bind-key "M-s s" 'sort-lines)
(bind-key "M-s M-s" 'sort-lines)
(bind-key "M-s O" 'occur)
(bind-key "M-z" 'zap-up-to-char)
(bind-key "M-Z" 'zap-to-char)
(bind-key "C-%" 'my/filter-buffer)

(bind-key "C-+" 'my/increment-number-at-point)
(bind-key "C-M-+" 'my/decrement-number-at-point)
(bind-key "C-$" 'copy-from-above-command)

(bind-key "C-h C-f" 'find-function)

(bind-key (kbd "C-*") 'my/isearch-current-region-or-word isearch-mode-map)

(bind-key [remap goto-line] 'my/goto-line-with-feedback)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

(windmove-default-keybindings 'super)   ; enable windmove

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      comment-padding nil
      diff-switches "-u"
      disabled-command-function nil     ; enable all disabled commands
      history-length 500
      indicate-buffer-boundaries 'left
      indicate-empty-lines t
      inhibit-startup-echo-area-message (user-login-name)
      inhibit-startup-screen t
      initial-scratch-message nil
      kill-whole-line t
      lazy-highlight-initial-delay 0
      mouse-wheel-progressive-speed nil
      resize-mini-windows t
      ring-bell-function 'ignore
      scroll-conservatively 10000
      scroll-margin 5
      scroll-preserve-screen-position t
      visual-order-cursor-movement t
      compilation-scroll-output 'first-error
      compilation-read-command nil
      )

(setq-default fill-column 100
              indent-tabs-mode nil
              tab-width 4)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 1)
(winner-mode 1)

(electric-layout-mode 1)
(global-ede-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plinkx")
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
  (add-to-list 'exec-path "C:/Go/bin"))

(defun my/balance-windows (&rest args)
  (balance-windows))
(advice-add 'split-window-right :after #'my/balance-windows)
(advice-add 'split-window-below :after #'my/balance-windows)
(advice-add 'delete-window :after #'my/balance-windows)

(defun my/indent-yanked-region (&rest args)
  (if (derived-mode-p 'prog-mode)
      (let ((mark-even-if-inactive transient-mark-mode))
        (indent-region (region-beginning) (region-end) nil))))
(advice-add 'yank :after #'my/indent-yanked-region)
(advice-add 'yank-pop :after #'my/indent-yanked-region)

(defun my/colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

(use-package desktop
  :init (setq desktop-restore-eager 5)
  :config (desktop-save-mode 1))

(use-package autorevert
  :init (setq auto-revert-verbose nil
              global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode 1))

(use-package recentf
  :init (setq recentf-max-saved-items 1000)
  :config (recentf-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-c C-s". ivy-resume))
  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "(%d/%d) ")
  :config
  (bind-keys :map ivy-minibuffer-map
             ("C-m" . ivy-alt-done)
             ("C-j" . ivy-done))
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x y" . counsel-yank-pop)
         ("C-c a" . counsel-ag)
         ("M-i" . counsel-imenu)))

(use-package cua-base
  :init (setq cua-enable-cua-keys nil)
  :config (cua-mode 1))

(use-package semantic
  :bind ("M-." . semantic-ia-fast-jump)
  :init (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode global-semanticdb-minor-mode))
  :config (semantic-mode 1))

(use-package paren
  :init (setq show-paren-delay 0)
  :config (show-paren-mode 1))

(use-package saveplace
  :init (setq-default save-place t))

(use-package ediff
  :defer t
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package doc-view
  :defer t
  :config (setq doc-view-continuous t))

(use-package flycheck
  :init
  (setq flycheck-clang-language-standard "c++1y")
  (add-to-list 'after-init-hook 'global-flycheck-mode))

(use-package dired
  :defer t
  :config
  (setq dired-isearch-filenames t
        dired-recursive-deletes 'always)
  (add-hook 'dired-mode-hook (lambda ()
                               (require 'dired-x)
                               (dired-omit-mode 1))))

(use-package org
  :defer t
  :config
  (setq org-replace-disputed-keys t
        org-src-fontify-natively t)
  (add-hook 'org-mode-hook (lambda ()
                             (make-local-variable 'show-paren-mode)
                             (setq show-paren-mode nil)
                             (flyspell-mode 1))))

(add-hook 'rst-mode-hook (lambda ()
                           (flyspell-mode 1)))

;; Programming
(bind-keys
 :map prog-mode-map
 ("<return>" . newline-and-indent)
 ("C-<delete>" . subword-kill)
 ("C-<right>" . subword-forward)
 ("C-<left>" . subword-backward))
(add-hook 'prog-mode-hook (lambda ()
                            (subword-mode 1)
                            (drag-stuff-mode 1)
                            (rainbow-delimiters-mode 1)
                            (setq show-trailing-whitespace t)
                            (font-lock-add-keywords
                             nil
                             '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\)\\>" 1 font-lock-warning-face t)))))

(use-package cc-mode
  :mode ("\\.x\\'" . c++-mode)
  :init (setq c-basic-offset 4
              c-default-style "bsd")
  :config
  (bind-keys :map c-mode-base-map
             ("C-c o" . ff-get-other-file)
             ("C-c i a" . my/insert-all-special)
             ("C-c i c" . my/insert-default-ctor)
             ("C-c i d" . my/insert-virtual-dtor)
             ("C-c i p" . my/insert-copy-ctor)
             ("C-c i P" . my/insert-copy-assignment-operator)
             ("C-c i m" . my/insert-move-ctor)
             ("C-c i M" . my/insert-move-assignment-operator))
  (add-hook 'c-mode-common-hook (lambda ()
                                  (setq comment-start "/*"
                                        comment-end "*/")
                                  (c-set-offset 'innamespace 0))))

(use-package python
  :mode ("SCons\\(truct\\|cript\\)\\'" . python-mode))

(use-package auto-compile
  :config
  (auto-compile-on-save-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode 1)
                                  (paredit-mode 1)
                                  (local-set-key (kbd "C-c C-e") 'my/eval-and-replace)))

(add-hook 'python-mode-hook (lambda ()
                              (define-key python-mode-map (kbd "C-c C-f") nil)))

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :config (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package sh-script
  :mode ("rc\\'" . sh-mode))

(use-package glasses
  :defer t
  :init (setq glasses-separate-parentheses-p nil
              glasses-uncapitalize-p t))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                                   try-expand-dabbrev-all-buffers
                                                   try-expand-dabbrev-from-kill
                                                   try-complete-file-name-partially
                                                   try-complete-file-name
                                                   try-expand-all-abbrevs
                                                   try-complete-lisp-symbol-partially
                                                   try-complete-lisp-symbol)))

(use-package eldoc
  :defer t
  :config (setq eldoc-idle-delay 0.1))

(use-package imenu
  :defer t
  :config (setq imenu-auto-rescan t))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))

(use-package avy
  :bind (("C-`" . avy-goto-char)
         ("C-~" . avy-goto-word-or-subword-1))
  :init
  (cl-loop for c from ?0 to ?9 do (my/add-super-char-to-avy 'subword-1 c))
  (cl-loop for c from ?A to ?Z do (my/add-super-char-to-avy 'subword-1 c))
  (cl-loop for c from ?a to ?z do (my/add-super-char-to-avy 'subword-1 c))
  (cl-loop for c in '(?\( ?\) ?{ ?} ?[ ?] ?< ?>
                          ?` ?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?- ?_ ?= ?+
                          ?\\ ?| ?\; ?: ?\" ?' ?, ?. ?/ ??)
           do (my/add-super-char-to-avy 'char c)))

(use-package anzu
  :config (global-anzu-mode 1))

(use-package company
  :init (setq company-idle-delay 0
              company-minimum-prefix-length 2
              company-backends '(company-elisp company-bbdb company-nxml company-css company-eclim company-xcode company-cmake (company-dabbrev-code company-gtags company-keywords) company-oddmuse company-files company-dabbrev))
  :config (global-company-mode 1))

(use-package diff-hl
  :config (global-diff-hl-mode 1))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package drag-stuff
  :defer t
  :config (setq drag-stuff-modifier '(meta shift)))

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  :config (setq emmet-indentation 2
                emmet-preview-default nil))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config (setq git-messenger:show-detail t))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(use-package guide-key
  :init (setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-x 8" "C-c p" "C-c C-a" "C-c C-b" "C-c C-c" "C-c C-e" "C-c C-s" "C-c C-t" "C-c ,")
              guide-key/idle-delay 0.0
              guide-key/popup-window-position (quote bottom)
              guide-key/recursive-key-sequence-flag t)
  :config (guide-key-mode 1))

(use-package helm-mode
  :init (setq helm-idle-delay 0
              helm-input-idle-delay 0
              helm-exit-idle-delay 0
              helm-M-x-fuzzy-match t
              helm-ff-transformer-show-only-basename nil
              helm-move-to-line-cycle-in-source t
              helm-yank-symbol-first t)
  :bind (("C-x C-r" . helm-recentf)
         ("C-x a" . helm-apropos)
         ("C-x f" . helm-mini)
         ("C-x g" . helm-google-suggest)
         ("M-X" . helm-M-x)
         ("M-s M-o" . helm-occur)
         ("M-s m" . helm-multi-occur)))

(use-package highlight-symbol
  :defer t
  :config (setq highlight-symbol-idle-delay 0))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-formats '((mark modified read-only " "
                                (name 25 25 :left :elide) " "
                                (size 6 -1 :right) " "
                                (mode 10 10 :left :elide) " "
                                (filename-and-process -1 60 :left :elide))
                          (mark " " (name 30 -1)
                                " " filename))
        ibuffer-saved-filter-groups '(("default"
                                       ("Dired" (mode . dired-mode))
                                       ("C/C++" (or
                                                 (mode . c-mode)
                                                 (mode . c++-mode)))
                                       ("Python" (mode . python-mode))
                                       ("Go" (mode . go-mode))
                                       ("Rust" (mode . rust-mode))
                                       ("Elisp" (mode . emacs-lisp-mode))
                                       ("Web" (or
                                               (mode . sgml-mode)
                                               (mode . web-mode)
                                               (mode . css-mode)
                                               (mode . js-mode)))
                                       ("Docs" (or
                                                (mode . org-mode)
                                                (mode . rst-mode)))
                                       ("Misc" (name . "^\\*"))
                                       )))
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package magit
  :bind (("<f9>" . magit-status)
         ("S-<f9>" . magit-log-buffer-file)
         ("C-c g" . magit-dispatch-popup))
  :config (setq magit-push-always-verify nil
                magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package git-commit
  :init (setq git-commit-summary-max-length 80)
  :config (global-git-commit-mode t))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook (lambda ()
                                  (auto-fill-mode 1)
                                  (refill-mode 1)))
  (setq markdown-command "markdown_py"))

(use-package multiple-cursors
  :bind (("C-|" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)
         ("C-c C->" . mc/mark-all-like-this-dwim)))

(use-package paredit
  :defer t
  :config
  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t))

(use-package popwin
  :init (popwin-mode 1))

(use-package projectile
  :init (setq projectile-completion-system 'ivy
              projectile-use-git-grep t
              projectile-enable-caching t)
  :config
  (fset 'projectile-kill-buffers 'my/projectile-kill-buffers)
  (projectile-global-mode 1)
  :bind (("C-c f" . projectile-find-in-known-projects)
         ("C-c C-f" . projectile-find-file)))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package undo-tree
  :init (setq undo-tree-auto-save-history t
              undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undodir"))))
  :config (global-undo-tree-mode 1))

(use-package web-mode
  :mode "\\.html$"
  :init (setq web-mode-code-indent-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode 1))

(use-package wrap-region
  :config (wrap-region-global-mode 1))

(use-package yasnippet
  :init (setq yas-prompt-functions '(yas-completing-prompt) ; use normal completion, which is helm in our case
              yas-verbosity 1)
  :config (yas-global-mode 1))

;;; init.el ends here

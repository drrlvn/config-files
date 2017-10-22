;;; init.el --- emacs config -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(let ((gc-cons-threshold-original gc-cons-threshold)
      (file-name-handler-alist-original file-name-handler-alist))
  (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil
                                              file-name-handler-alist file-name-handler-alist-original
                                              gc-cons-threshold gc-cons-threshold-original))))

(setq gc-cons-threshold (* 100 1024 1024)
      file-name-handler-alist nil
      inhibit-message t
      load-prefer-newer t
      custom-file "~/.emacs.d/custom.el"
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (defvar use-package-enable-imenu-support)
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'bind-key))

(push "~/.emacs.d/lisp" load-path)
(let ((zplug-bin (expand-file-name "~/.zplug/bin")))
  (push zplug-bin exec-path)
  (setenv "PATH" (concat zplug-bin path-separator (getenv "PATH"))))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(require 'config-defuns-autoloads)
(require 'config-looks)

(defconst rg-executable (executable-find "rg"))

(defgroup my/customizations nil
  "Customizations"
  :group 'convenience)

(defcustom my/scroll-bindings nil
  "Line scroll bindings"
  :type 'boolean
  :group 'my/customizations)

(defcustom my/restricted-resources nil
  "Avoid using resource-demanding packages, which might lead to parformance degradation"
  :type 'boolean
  :group 'my/customizations)

(bind-key "<escape>" #'keyboard-escape-quit)
(bind-key "C-x r q" #'save-buffers-kill-emacs)
(unbind-key "C-x C-c")
(bind-key "<f5>" #'my/revert-buffer-no-confirmation)
(bind-key "M-<f9>" #'vc-revision-other-window)
(bind-key "<f11>" #'toggle-frame-fullscreen)
(bind-key "S-<f11>" #'whitespace-cleanup)
(bind-key "<f12>" #'my/cleanup-buffer)
(bind-key "S-<f12>" #'my/find-user-init-file)
(bind-key "C-<f12>" #'my/magit-status-config-project)

(bind-key "M-<return>" #'my/open-line-below)
(bind-key "M-S-<return>" #'my/open-line-above)
(bind-key "C-<delete>" #'kill-word)
(bind-key "M-SPC" #'cycle-spacing)

(bind-key "C-<tab>" #'previous-buffer)
(bind-key "C-S-<iso-lefttab>" #'next-buffer)

(bind-key "C-z" #'repeat)
(unbind-key "C-x C-z")
(bind-key "C-!" #'kill-this-buffer)
(bind-key "C-M-!" #'my/kill-buffer-other-window)
(bind-key "C-^" #'bury-buffer)
(bind-key "C-#" #'quick-calc)

(bind-key "C-c C-<return>" #'delete-blank-lines)
(bind-key "C-c n" #'my/cleanup-buffer)
(bind-key "C-c d" #'my/diff-current-buffer-with-file)
(bind-key "C-c r" #'my/rotate-windows)
(bind-key "C-c C-;" #'my/toggle-comment-line-or-region)
(bind-key "M-s M-s" #'sort-lines)
(bind-key "C-%" #'my/filter-buffer)

(bind-key "C-+" #'my/increment-number-at-point)
(bind-key "C-M-+" #'my/decrement-number-at-point)

(bind-key "C-x C-p" #'my/show-buffer-file-name)

(bind-key "C-x n r" #'narrow-to-region)
(bind-key "C-x n n" #'my/narrow-or-widen-dwim)

(bind-key "C-x p" #'my/package-upgrade-all)

(bind-key [remap goto-line] #'my/goto-line-with-feedback)

(when my/scroll-bindings
  (bind-key "C-e" #'my/scroll-up)
  (bind-key "C-y" #'my/scroll-down))

(dotimes (i 10)
      (bind-key (format "C-%d" i)  (intern (format "select-window-%d" i))))

(use-package mwim
  :ensure
  :bind (("<home>" . mwim-beginning)
         ("<end>" . mwim-end)))

(use-package misc
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char)
         ("C-$" . copy-from-above-command)))

(use-package windmove
  :bind (("M-<left>" . windmove-left)
         ("M-<right>" . windmove-right)
         ("M-<up>" . windmove-up)
         ("M-<down>" . windmove-down)))

(use-package hydra
  :ensure
  :bind ("<f8>" . my/hydra-error/body)
  :config (defhydra my/hydra-error ()
            "goto-error"
            ("P" first-error "first")
            ("n" next-error "next")
            ("p" previous-error "prev")
            ("v" recenter-top-bottom "recenter")
            ("q" nil "quit")))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(advice-add #'split-window-right :after #'my/balance-windows)
(advice-add #'split-window-below :after #'my/balance-windows)
(advice-add #'delete-window :after #'my/balance-windows)

(advice-add #'yank :after #'my/indent-yanked-region)
(advice-add #'yank-pop :after #'my/indent-yanked-region)

(fset #'yes-or-no-p #'y-or-n-p)

(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

(defun display-startup-echo-area-message () ".")

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      comment-padding nil
      diff-switches "-u"
      disabled-command-function nil
      history-length 500
      indicate-buffer-boundaries 'left
      indicate-empty-lines t
      inhibit-startup-screen t
      initial-scratch-message nil
      kill-whole-line t
      large-file-warning-threshold (* 100 1024 1024)
      lazy-highlight-initial-delay 0
      mouse-wheel-progressive-speed nil
      resize-mini-windows t
      ring-bell-function 'ignore
      scroll-conservatively 10000
      scroll-margin 5
      scroll-preserve-screen-position t
      visual-order-cursor-movement t
      )

(setq-default comment-column 0
              fill-column 100
              fringes-outside-margins t
              indent-tabs-mode nil
              tab-width 4
              cursor-type 'bar
              )

(use-package hl-line
  :config (global-hl-line-mode 1))

(use-package saveplace
  :config (save-place-mode 1))

(use-package calendar
  :defer
  :config (setq calendar-weekend-days '(5 6)))

(use-package compile
  :defer
  :config
  (setq compilation-scroll-output 'first-error
        compilation-read-command nil)
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

(use-package ediff
  :defer
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package doc-view
  :defer
  :config (setq doc-view-continuous t
                doc-view-resolution 300))

(use-package dired
  :defer
  :config
  (setq dired-recursive-deletes 'always)
  (add-hook 'dired-mode-hook (apply-partially #'dired-omit-mode 1)))

(use-package dired-aux
  :defer
  :config (setq dired-isearch-filenames t))

(use-package dired-x
  :commands dired-omit-mode)

(use-package org
  :defer
  :config
  (setq org-replace-disputed-keys t
        org-src-fontify-natively t
        org-startup-indented t)
  (add-hook 'org-mode-hook #'my/org-mode-hook))

(use-package ox-html
  :defer
  :config (setq org-html-postamble nil))

(use-package glasses
  :defer
  :config (setq glasses-separate-parentheses-p nil
                glasses-uncapitalize-p t))

(use-package eldoc
  :defer
  :config (setq eldoc-idle-delay 0.1))

(use-package imenu
  :defer
  :config (setq imenu-auto-rescan t))

(use-package tramp
  :defer
  :config (setq tramp-use-ssh-controlmaster-options nil
                tramp-default-method "scpx"
                tramp-histfile-override "/dev/null"))

(use-package uniquify
  :defer
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))

(use-package rst
  :defer
  :config (add-hook 'rst-mode-hook (apply-partially #'flyspell-mode 1)))

(use-package server
  :if window-system
  :config (server-start t))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

(use-package beginend
  :ensure
  :config (beginend-global-mode 1))

(use-package bln-mode
  :ensure
  :bind (("M-[" . bln-backward-half)
         ("M-]" . bln-forward-half)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode 1))

(use-package smex
  :ensure
  :defer
  :config (setq smex-history-length 3))

(use-package wgrep
  :ensure
  :defer
  :config (setq wgrep-auto-save-buffer t))

(use-package ivy
  :ensure
  :demand
  :bind (("C-c s". ivy-resume)
         :map ivy-minibuffer-map
         ("C-m" . ivy-alt-done)
         ("C-j" . ivy-done)
         ("<next>" . ivy-scroll-up-command)
         ("<prior>" . ivy-scroll-down-command))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-extra-directories '("./")
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-initial-inputs-alist nil)
  (push '(emacs-lisp-mode . swiper-match-face-1) ivy-switch-buffer-faces-alist)
  (push '(python-mode . swiper-match-face-2) ivy-switch-buffer-faces-alist)
  (push '(c++-mode . swiper-match-face-3) ivy-switch-buffer-faces-alist)
  (ivy-mode 1))

(use-package ivy-hydra
  :ensure
  :bind (:map ivy-minibuffer-map
              ("M-o" . ivy-dispatching-done-hydra)))

(use-package counsel
  :ensure
  :demand
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x y" . counsel-yank-pop)
         ("C-x C-r" . counsel-recentf)
         ("M-i" . counsel-imenu)
         ("C-c a" . my/counsel-rg)
         ("C-c u" . counsel-unicode-char)
         :map counsel-ag-map
         ("<down>" . ivy-next-line-and-call)
         ("<up>" . ivy-previous-line-and-call)
         :map counsel-imenu-map
         ("<down>" . ivy-next-line-and-call)
         ("<up>" . ivy-previous-line-and-call))
  :config
  (setq counsel-find-file-ignore-regexp (concat
                                         ;; file names beginning with # or .
                                         "\\(?:\\`[#.]\\)"
                                         ;; file names ending with # or ~
                                         "\\|\\(?:[#~]\\'\\)"))
  (when rg-executable
    (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
          counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' %s"
          counsel-ag-base-command counsel-rg-base-command))
  (counsel-mode 1))

(use-package swiper
  :ensure
  :bind (("C-S-s" . my/swiper-region-or-current-word)))

(use-package cua-base
  :config
  (setq cua-enable-cua-keys nil)
  (cua-mode 1))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package flycheck
  :ensure
  :demand
  :bind ("M-<f8>" . flycheck-list-errors)
  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-emacs-lisp-load-path 'inherit)
  (when window-system
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      (vector #b00010000
              #b00110000
              #b01110000
              #b11110000
              #b01110000
              #b00110000
              #b00010000)))
  (global-flycheck-mode 1))

(use-package prog-mode
  :defer
  :init (add-hook 'prog-mode-hook #'my/prog-mode-hook))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . my/eval-and-replace))
  :init (add-hook 'emacs-lisp-mode-hook (apply-partially #'eldoc-mode 1)))

(use-package lispy
  :ensure
  :defer
  :init
  (add-hook 'emacs-lisp-mode-hook (apply-partially #'lispy-mode 1))
  (add-hook 'hy-mode-hook (apply-partially #'lispy-mode 1))
  :config
  (unbind-key "M-i" lispy-mode-map-lispy)
  (unbind-key "C-," lispy-mode-map-lispy)
  (unbind-key "<M-left>" lispy-mode-map-lispy)
  (unbind-key "<M-right>" lispy-mode-map-lispy))

(use-package macrostep
  :ensure
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

(use-package suggest
  :ensure
  :defer)

(use-package cc-mode
  :mode ("\\.x\\'" . c++-mode)
  :bind (:map c-mode-base-map
              ("C-c o" . ff-get-other-file)
              ("C-c f" . my/maybe-clang-format-buffer)
              ("C-c i a" . my/insert-all-special)
              ("C-c i c" . my/insert-default-ctor)
              ("C-c i d" . my/insert-virtual-dtor)
              ("C-c i p" . my/insert-copy-ctor)
              ("C-c i P" . my/insert-copy-assignment-operator)
              ("C-c i m" . my/insert-move-ctor)
              ("C-c i M" . my/insert-move-assignment-operator))
  :config
  (add-hook 'c-mode-common-hook #'my/c-mode-common-hook)
  (setq c-basic-offset 4
        c-default-style "bsd"))

(use-package irony
  :if (not my/restricted-resources)
  :ensure
  :defer
  :init
  (add-hook 'c-mode-common-hook (apply-partially #'irony-mode 1))
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :if (not my/restricted-resources)
  :ensure
  :after company
  :config (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :if (not my/restricted-resources)
  :ensure
  :defer
  :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :if (not my/restricted-resources)
  :ensure
  :defer
  :init (add-hook 'irony-mode-hook (apply-partially #'irony-eldoc 1)))

(use-package clang-format
  :ensure
  :defer)

(use-package python
  :mode (("SCons\\(truct\\|cript\\)\\'" . python-mode)
         ("slashrc\\'" . python-mode))
  :bind (:map python-mode-map
              ("C-<f8>" . my/pylint-ignore-errors-at-point))
  :config
  (unbind-key "C-c C-f" python-mode-map)
  (advice-add #'python-indent-shift-left :around #'my/python-shift-region)
  (advice-add #'python-indent-shift-right :around #'my/python-shift-region))

(use-package hy-mode
  :ensure
  :defer)

(use-package py-isort
  :ensure
  :after python
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-c i" . my/python-insert-import)
              ("C-c I" . my/py-isort-buffer))
  :config (setq py-isort-options '("-ds")))

(use-package pyvenv
  :ensure
  :init (add-hook 'hack-local-variables-hook #'my/pyvenv-activate))

(use-package anaconda-mode
  :ensure
  :defer
  :init
  (add-hook 'python-mode-hook (apply-partially #'anaconda-mode 1))
  (add-hook 'python-mode-hook (apply-partially #'anaconda-eldoc-mode 1)))

(use-package company-anaconda
  :ensure
  :commands my/company-anaconda-setup
  :init (add-hook 'anaconda-mode-hook #'my/company-anaconda-setup))

(use-package go-mode
  :ensure
  :defer)

(use-package toml-mode
  :ensure
  :defer)

(use-package rust-mode
  :ensure
  :defer)

(use-package yaml-mode
  :ensure
  :defer)

(use-package cmake-font-lock
  :ensure
  :defer)

(use-package cmake-mode
  :ensure
  :defer
  :config (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                                   try-expand-dabbrev
                                                   try-expand-dabbrev-all-buffers
                                                   try-expand-dabbrev-from-kill
                                                   try-complete-file-name-partially
                                                   try-complete-file-name
                                                   try-expand-all-abbrevs
                                                   try-complete-lisp-symbol-partially
                                                   try-complete-lisp-symbol
                                                   )))

(use-package avy
  :ensure
  :bind ("s-s" . avy-goto-word-or-subword-1))

(use-package company
  :ensure
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-backends (delete 'company-clang company-backends))
  (global-company-mode 1))

(use-package company-statistics
  :ensure
  :init (add-hook 'global-company-mode-hook (apply-partially #'company-statistics-mode 1)))

(use-package conf-mode
  :mode "\\.pylintrc\\'")

(use-package diff-hl
  :if (not my/restricted-resources)
  :ensure
  :demand
  :bind ("C-]" . my/hydra-diff-hl/body)
  :config
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (defhydra my/hydra-diff-hl (:hint nil)
    "git-gutter"
    ("]" diff-hl-next-hunk "next")
    ("[" diff-hl-previous-hunk "previous")
    ("r" diff-hl-revert-hunk "revert")
    ("q" nil "quit"))
  (setq diff-hl-fringe-bmp-function
        (lambda (type _pos) (if (eq type 'delete) 'diff-hl-bmp-delete 'diff-hl-bmp-change)))
  (defun diff-hl-define-bitmaps ()
    (unless (fringe-bitmap-p 'diff-hl-bmp-delete)
      (define-fringe-bitmap 'diff-hl-bmp-delete
        (vector #b10000000
                #b11000000
                #b11100000
                #b11110000)
        nil nil 'bottom)
      (define-fringe-bitmap 'diff-hl-bmp-change
        (vector #b11100000)
        nil nil '(center t))))
  (global-diff-hl-mode 1))

(use-package discover-my-major
  :ensure
  :bind ("C-h <return>" . discover-my-major))

(use-package dockerfile-mode
  :ensure
  :defer)

(use-package drag-stuff
  :ensure
  :bind (("M-S-<up>" . drag-stuff-up)
         ("M-S-<down>" . drag-stuff-down)
         ("M-S-<left>" . drag-stuff-left)
         ("M-S-<right>" . drag-stuff-right))
  :config (add-hook 'drag-stuff-after-drag-hook #'my/indent-line-or-region))

(use-package dumb-jump
  :ensure
  :bind (("M-g o" . dumb-jump-go)
         ("M-g O" . dumb-jump-go-other-window)
         ("M-g M-o" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-selector 'ivy)
  (when rg-executable
    (setq dumb-jump-force-searcher 'rg)))

(use-package easy-kill
  :ensure
  :bind ([remap kill-ring-save] . easy-kill))

(use-package emmet-mode
  :ensure
  :defer
  :init
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'web-mode-hook #'emmet-mode)
  :config (setq emmet-indentation 2
                emmet-preview-default nil))

(use-package expand-region
  :ensure
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package eyebrowse
  :ensure
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t)
  (eyebrowse-mode 1))

(use-package git-messenger
  :ensure
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("d" . my/git-messenger-show-with-magit)
         ("l" . my/git-messenger-link-commit))
  :config (setq git-messenger:show-detail t))

(use-package gitignore-mode
  :ensure
  :defer)

(use-package helpful
  :ensure
  :bind (("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)))

(use-package highlight-symbol
  :ensure
  :bind (("C-\"" . highlight-symbol-at-point)
         ("C-," . highlight-symbol-prev)
         ("C-." . highlight-symbol-next))
  :config (setq highlight-symbol-idle-delay 0))

(use-package highlight-parentheses
  :ensure
  :config
  (setq hl-paren-delay 0)
  (global-highlight-parentheses-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t
        ibuffer-formats '((mark modified read-only " "
                                (name 25 25 :left :elide) " "
                                (size 6 -1 :right) " "
                                (mode 10 10 :left :elide) " "
                                (filename-and-process -1 60 :left :elide))
                          (mark " " (name 30 -1)
                                " " filename)))
  (add-hook 'ibuffer-mode-hook (apply-partially #'ibuffer-switch-to-saved-filter-groups "default")))

(use-package ibuf-ext
  :after ibuffer
  :config (setq ibuffer-show-empty-filter-groups nil
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
                                               ))))

(use-package magit
  :ensure
  :bind (("<f9>" . magit-status)
         ("S-<f9>" . magit-log-buffer-file)
         ("C-<f9>" . magit-blame)
         ("C-c g" . magit-dispatch-popup))
  :config
  (setq magit-bury-buffer-function 'magit-mode-quit-window
        magit-repository-directories '(("~/dev" . 1))
        magit-tag-arguments '("--annotate")
        magit-fetch-arguments '("--prune")
        magit-diff-refine-hunk 'all)
  (unless (eq system-type 'darwin)
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))
  (remove-hook 'magit-pre-display-buffer-hook #'magit-save-window-configuration)
  (magit-add-section-hook 'magit-status-sections-hook #'magit-insert-modules-overview nil 'append))

(use-package magit-gitflow
  :ensure
  :defer
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

(use-package git-commit
  :init (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  :config
  (setq git-commit-summary-max-length fill-column)
  (global-git-commit-mode 1))

(use-package git-link
  :ensure
  :bind (("C-c C-g h" . git-link-homepage)
         ("C-c C-g c" . git-link-commit)
         ("C-c C-g l" . git-link)
         ("C-c C-g H" . my/git-link-homepage-in-browser)))

(use-package man
  :bind ("<f1>" . man)
  :config
  (set-face-attribute 'Man-overstrike nil :inherit 'font-lock-keyword-face)
  (setq Man-notify-method 'pushy))

(use-package markdown-mode
  :ensure
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (add-hook 'markdown-mode-hook (apply-partially #'auto-fill-mode 1))
  (add-hook 'markdown-mode-hook (apply-partially #'flyspell-mode 1))
  :config (setq markdown-command "cmark"))

(use-package multiple-cursors
  :ensure
  :bind (("C-|" . mc/edit-lines)
         ("C-;" . mc/mark-all-like-this-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-:" . my/hydra-multiple-cursors/body))
  :init (advice-add #'zap-up-to-char :around #'my/mc-prompt-once)
  :config (defhydra my/hydra-multiple-cursors (:hint nil)
            "
^Up^           ^Down^         ^Miscellaneous^
---------------------------------------------
_p_:   Next    _n_:   Next    _l_: Edit lines
_P_:   Skip    _N_:   Skip    _a_: Mark all
_M-p_: Unmark  _M-n_: Unmark  _q_: Quit"
            ("l" mc/edit-lines :exit t)
            ("a" mc/mark-all-like-this-dwim :exit t)
            ("n" mc/mark-next-like-this)
            ("N" mc/skip-to-next-like-this)
            ("M-n" mc/unmark-next-like-this)
            ("p" mc/mark-previous-like-this)
            ("P" mc/skip-to-previous-like-this)
            ("M-p" mc/unmark-previous-like-this)
            ("q" nil)))

(use-package popwin
  :ensure
  :commands (popwin:display-buffer-condition popwin:display-buffer-action)
  :init (push '(popwin:display-buffer-condition popwin:display-buffer-action) display-buffer-alist)
  :config
  (push '("*Flycheck errors*" :stick t) popwin:special-display-config)
  (push 'helpful-mode popwin:special-display-config))

(use-package projectile
  :ensure
  :demand
  :bind (("C-c C-f" . projectile-find-file)
         :map projectile-command-map
         ("s" . my/counsel-projectile-rg))
  :config
  (setq projectile-completion-system 'ivy)
  (fset #'projectile-kill-buffers #'my/projectile-kill-buffers)
  (advice-add #'projectile-switch-project :around #'my/projectile-disable-remove-current-project)
  (projectile-mode 1))

(use-package counsel-projectile
  :ensure
  :config (counsel-projectile-on))

(use-package rainbow-delimiters
  :ensure
  :defer)

(use-package restclient
  :ensure
  :mode ("\\.http\\'" . restclient-mode))

(use-package syntax-subword
  :ensure
  :config (global-syntax-subword-mode 1))

(use-package systemd
  :ensure
  :defer)

(use-package undo-tree
  :ensure
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undodir"))))
  (global-undo-tree-mode 1))

(use-package web-mode
  :ensure
  :mode "\\.hbs\\'"
  :mode "\\.html\\'"
  :config (setq web-mode-code-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-style-padding 2
                web-mode-script-padding 2))

(use-package visual-regexp
  :ensure
  :defer)

(use-package js2-mode
  :ensure
  :mode "\\.js\\'")

(use-package winner
  :config (winner-mode))

(use-package which-key
  :ensure
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode 1))

(use-package whitespace-cleanup-mode
  :ensure
  :config (global-whitespace-cleanup-mode 1))

(use-package wrap-region
  :ensure
  :config (wrap-region-global-mode 1))

(use-package langtool
  :ensure
  :bind (:map text-mode-map
              ("C-c l" . langtool-check)))

(use-package yasnippet
  :ensure
  :config
  (setq yas-prompt-functions '(yas-completing-prompt) ; use normal completion
        yas-verbosity 1)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map)
  (yas-global-mode 1))

;;; init.el ends here

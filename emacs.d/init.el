;;; init.el --- emacs config -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(push "~/.emacs.d/lisp" load-path)

(setq load-prefer-newer t
      custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(require 'package)
(setq package-check-signature nil
      package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (defvar use-package-enable-imenu-support t)
  (require 'use-package)
  (require 'cl-macs))

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode 1))

(require 'config-defuns-autoloads)
(require 'config-looks)
(use-package bind-key)

(use-package misc
  :commands (zap-up-to-char copy-from-above-command))

(bind-key "C-x r q" #'save-buffers-kill-emacs)
(unbind-key "C-x C-c")
(bind-key "<home>" #'mwim-beginning-of-code-or-line)
(bind-key "<end>" #'end-of-line)
(bind-key "<escape>" #'keyboard-escape-quit)
(bind-key "<f5>" #'my/revert-buffer-no-confirmation)
(bind-key "M-<f9>" #'vc-revision-other-window)
(bind-key "<f11>" #'my/cleanup-buffer)
(bind-key "S-<f11>" #'whitespace-cleanup)
(bind-key "S-<f12>" #'my/find-user-init-file)

(bind-key "M-<return>" #'my/open-line-below)
(bind-key "M-S-<return>" #'my/open-line-above)
(bind-key "C-<delete>" #'kill-word)
(bind-key "M-SPC" #'cycle-spacing)

(bind-key "C-<tab>" #'previous-buffer)
(bind-key "C-S-<iso-lefttab>" #'next-buffer)

(bind-key "C-n" #'my/scroll-up)
(bind-key "C-p" #'my/scroll-down)
(bind-key "M-n" #'my/scroll-other-window-up)
(bind-key "M-p" #'my/scroll-other-window-down)

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
(bind-key "M-s s" #'sort-lines)
(bind-key "M-s M-s" #'sort-lines)
(bind-key "M-s O" #'occur)
(bind-key "M-z" #'zap-up-to-char)
(bind-key "M-Z" #'zap-to-char)
(bind-key "C-%" #'my/filter-buffer)

(bind-key "C-+" #'my/increment-number-at-point)
(bind-key "C-M-+" #'my/decrement-number-at-point)
(bind-key "C-$" #'copy-from-above-command)

(bind-key "C-h C-f" #'find-function)

(bind-key "C-x C-p" #'my/show-buffer-file-name)

(bind-key "C-x n r" #'narrow-to-region)
(bind-key "C-x n n" #'my/narrow-or-widen-dwim)

(bind-key [remap goto-line] #'my/goto-line-with-feedback)

(use-package hydra
  :ensure t
  :bind ("<f8>" . my/hydra-error/body)
  :config
  (defhydra my/hydra-error ()
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

(fset #'yes-or-no-p #'y-or-n-p)

(windmove-default-keybindings 'super)   ; enable windmove

(defun display-startup-echo-area-message ()
  "Override default function and display nothing."
  )

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      comment-padding nil
      diff-switches "-u"
      disabled-command-function nil     ; enable all disabled commands
      history-length 500
      indicate-buffer-boundaries 'left
      indicate-empty-lines t
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
      )

(setq-default fill-column 100
              comment-column 0
              indent-tabs-mode nil
              tab-width 4)

(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)
(winner-mode 1)

(electric-layout-mode 1)
(global-ede-mode 1)
(global-hl-line-mode 1)
(save-place-mode 1)

(use-package compile
  :defer t
  :config (setq compilation-scroll-output 'first-error
                compilation-read-command nil))

(use-package ediff
  :defer t
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package doc-view
  :defer t
  :config (setq doc-view-continuous t
                doc-view-resolution 300))

(use-package dired
  :defer t
  :config
  (setq dired-recursive-deletes 'always)
  (add-hook 'dired-mode-hook (apply-partially #'dired-omit-mode 1)))

(use-package dired-aux
  :defer t
  :config (setq dired-isearch-filenames t))

(use-package dired-x
  :commands dired-omit-mode)

(use-package org
  :defer t
  :config
  (setq org-replace-disputed-keys t
        org-src-fontify-natively t
        org-startup-indented t)
  (add-hook 'org-mode-hook #'my/org-mode-hook))

(use-package ox-html
  :defer t
  :config (setq org-html-postamble nil))

(use-package glasses
  :defer t
  :config (setq glasses-separate-parentheses-p nil
                glasses-uncapitalize-p t))

(use-package eldoc
  :defer t
  :config (setq eldoc-idle-delay 0.1))

(use-package imenu
  :defer t
  :config (setq imenu-auto-rescan t))

(use-package tramp
  :defer t
  :config (setq tramp-use-ssh-controlmaster-options nil
                tramp-default-method "scpx"
                tramp-histfile-override "/dev/null"))

(use-package uniquify
  :defer t
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"))

(use-package rst
  :defer t
  :config (add-hook 'rst-mode-hook (apply-partially #'flyspell-mode 1)))

(defun my/balance-windows (&rest _args)
  "Call `balance-windows' while ignoring ARGS."
  (balance-windows))
(advice-add #'split-window-right :after #'my/balance-windows)
(advice-add #'split-window-below :after #'my/balance-windows)
(advice-add #'delete-window :after #'my/balance-windows)

(defun my/indent-yanked-region (&rest _args)
  "Indent region in major modes that don't mind indentation, ignoring ARGS."
  (if (and
       (derived-mode-p 'prog-mode)
       (not (member major-mode '(python-mode ruby-mode makefile-mode))))
      (let ((mark-even-if-inactive transient-mark-mode))
        (indent-region (region-beginning) (region-end) nil))))
(advice-add #'yank :after #'my/indent-yanked-region)
(advice-add #'yank-pop :after #'my/indent-yanked-region)

(defun my/colorize-compilation-buffer ()
  "Colorize complication buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook #'server-start t))

(use-package autorevert
  :config
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

(use-package recentf
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'recentf-mode 1))
  :config (setq recentf-max-saved-items 1000))

(use-package smex
  :ensure t
  :init (defvar smex-history-length 3))

(use-package wgrep
  :ensure t
  :config (setq wgrep-auto-save-buffer t))

(use-package ivy
  :ensure t
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
        ivy-initial-inputs-alist (my/multi-filter-alist
                                  '(counsel-M-x counsel-describe-function counsel-describe-variable)
                                  ivy-initial-inputs-alist))
  (ivy-set-actions
   'projectile-switch-project
   `(("g" magit-status "magit status")
     ("s" ,(apply-partially #'counsel-rg nil) "search (rg)")))
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :demand
  :bind (("C-x y" . counsel-yank-pop)
         ("C-x C-r" . counsel-recentf)
         ("M-i" . counsel-imenu)
         ("C-c a" . my/counsel-rg)
         ("C-c u" . counsel-unicode-char))
  :config
  (setq counsel-find-file-ignore-regexp (concat
                                         ;; file names beginning with # or .
                                         "\\(?:\\`[#.]\\)"
                                         ;; file names ending with # or ~
                                         "\\|\\(?:[#~]\\'\\)")
        counsel-rg-base-command "rg -S --no-heading --line-number --max-columns 150 --color never %s .")
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-S-s" . my/swiper-region-or-current-word)))

(use-package cua-base
  :config
  (setq cua-enable-cua-keys nil)
  (cua-mode 1))

(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package flycheck
  :ensure t
  :init (add-hook 'after-init-hook #'global-flycheck-mode 1)
  :config (setq flycheck-global-modes '(not c++-mode)
                flycheck-emacs-lisp-load-path 'inherit))

(use-package prog-mode
  :bind (:map prog-mode-map
              ("<return>" . newline-and-indent))
  :init (add-hook 'prog-mode-hook #'my/prog-mode-hook))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-e" . my/eval-and-replace))
  :init (add-hook 'emacs-lisp-mode-hook (apply-partially #'eldoc-mode 1)))

(use-package cc-mode
  :mode ("\\.x\\'" . c++-mode)
  :init (setq c-basic-offset 4
              c-default-style "bsd")
  :bind (:map c-mode-base-map
              ("C-c o" . ff-get-other-file)
              ("C-c i a" . my/insert-all-special)
              ("C-c i c" . my/insert-default-ctor)
              ("C-c i d" . my/insert-virtual-dtor)
              ("C-c i p" . my/insert-copy-ctor)
              ("C-c i P" . my/insert-copy-assignment-operator)
              ("C-c i m" . my/insert-move-ctor)
              ("C-c i M" . my/insert-move-assignment-operator))
  :config (add-hook 'c-mode-common-hook #'my/c-mode-common-hook))

(use-package python
  :mode (("SCons\\(truct\\|cript\\)\\'" . python-mode)
         ("slashrc\\'" . python-mode))
  :bind (:map python-mode-map
              ("C-<f8>" . my/pylint-ignore-errors-at-point))
  :config (unbind-key "C-c C-f" python-mode-map))

(use-package py-isort
  :ensure t
  :after python
  :commands py-isort-buffer
  :bind (:map python-mode-map
              ("C-c i" . my/python-insert-import)
              ("C-c I" . my/py-isort-buffer))
  :config (setq py-isort-options '("-ds")))

(use-package pyvenv
  :ensure t
  :init (add-hook 'hack-local-variables-hook #'my/pyvenv-activate))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'my/turn-on-anaconda-mode))

(use-package company-anaconda
  :ensure t
  :defer t
  :after company
  :config (add-hook 'anaconda-mode-hook #'my/company-anaconda-setup))

(use-package go-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package cmake-font-lock
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t
  :config (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :init (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
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
  :ensure t
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

(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'global-company-mode 1))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-backends (delete 'company-clang company-backends)))

(use-package company-statistics
  :ensure t
  :after company
  :init (add-hook 'global-company-mode-hook (apply-partially #'company-statistics-mode 1)))

(use-package conf-mode
  :mode "\\.pylintrc\\'")

(use-package diff-hl
  :ensure t
  :after hydra
  :bind (("C-]" . my/hydra-diff-hl/body))
  :defer t
  :init
  (add-hook 'after-init-hook (apply-partially #'global-diff-hl-mode 1))
  (defhydra my/hydra-diff-hl (:hint nil)
    "diff-hl actions"
    ("n" diff-hl-next-hunk "next")
    ("p" diff-hl-previous-hunk "previous")
    ("r" diff-hl-revert-hunk "revert")
    ("q" nil "quit")))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package drag-stuff
  :ensure t
  :bind (("M-S-<up>" . drag-stuff-up)
         ("M-S-<down>" . drag-stuff-down)
         ("M-S-<left>" . drag-stuff-left)
         ("M-S-<right>" . drag-stuff-right)))

(use-package dumb-jump
  :ensure t
  :init (setq dumb-jump-selector 'ivy)
  :bind (("M-g o" . dumb-jump-go)
         ("M-g O" . dumb-jump-go-other-window)
         ("M-g M-o" . dumb-jump-quick-look)))

(use-package easy-kill
  :ensure t
  :bind ([remap kill-ring-save] . easy-kill))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (setq emmet-indentation 2
        emmet-preview-default nil)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'web-mode-hook #'emmet-mode))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package eyebrowse
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'eyebrowse-mode 1))
  :config (setq eyebrowse-wrap-around t
                eyebrowse-new-workspace t))

(use-package git-messenger
  :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("d" . my/git-messenger-show-with-magit)
         ("l" . my/git-messenger-link-commit))
  :init (setq git-messenger:show-detail t))

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(use-package which-key
  :ensure t
  :init (add-hook 'after-init-hook (apply-partially #'which-key-mode 1))
  :defer t
  :config (setq which-key-idle-delay 0.5))

(use-package highlight-symbol
  :ensure t
  :bind (("C-\"" . highlight-symbol-at-point)
         ("C-," . highlight-symbol-prev)
         ("C-." . highlight-symbol-next)
         ("M-s o" . highlight-symbol-occur))
  :init (setq highlight-symbol-idle-delay 0))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init (add-hook 'ibuffer-mode-hook #'my/ibuffer-mode-hook)
  :config (setq ibuffer-expert t
                ibuffer-formats '((mark modified read-only " "
                                        (name 25 25 :left :elide) " "
                                        (size 6 -1 :right) " "
                                        (mode 10 10 :left :elide) " "
                                        (filename-and-process -1 60 :left :elide))
                                  (mark " " (name 30 -1)
                                        " " filename))))

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
  :ensure t
  :bind (("<f9>" . magit-status)
         ("S-<f9>" . magit-log-buffer-file)
         ("C-<f9>" . magit-blame)
         ("C-c g" . magit-dispatch-popup))
  :config
  (setq magit-bury-buffer-function 'magit-mode-quit-window
        magit-repository-directories '(("~/dev" . 1))
        magit-tag-arguments '("--annotate")
        magit-fetch-arguments '("--prune")
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (remove-hook 'magit-pre-display-buffer-hook #'magit-save-window-configuration))

(use-package magit-gitflow
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow))

(use-package git-commit
  :init (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  :config
  (setq git-commit-summary-max-length fill-column)
  (global-git-commit-mode 1))

(use-package git-link
  :ensure t
  :bind (("C-c C-g h" . git-link-homepage)
         ("C-c C-g c" . git-link-commit)
         ("C-c C-g l" . git-link)
         ("C-c C-g H" . my/git-link-homepage-in-browser)))

(use-package markdown-mode
  :ensure t
  :defer t
  :init (add-hook 'markdown-mode-hook #'my/markdown-mode-hook))

(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind (("C-|" . mc/edit-lines)
         ("C-;" . mc/mark-all-like-this-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-:" . my/hydra-multiple-cursors/body))
  :init
  (defhydra my/hydra-multiple-cursors (:hint nil)
    "
^Up^           ^Down^         ^Miscellaneous^
---------------------------------------------
_p_:   Next    _n_:   Next    _l_: Edit lines
_P_:   Skip    _N_:   Skip    _a_: Mark all
_M-p_: Unmark  _M-n_: Unmark  _q_: Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("q" nil)))

(use-package mwim
  :ensure t)

(use-package paredit
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  :config
  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t))

(use-package popwin
  :ensure t
  :commands popwin-mode
  :init (add-hook 'after-init-hook (apply-partially #'popwin-mode 1)))

(use-package projectile
  :ensure t
  :demand
  :init (setq projectile-completion-system 'ivy
              projectile-use-git-grep t)
  :config
  (fset #'projectile-kill-buffers #'my/projectile-kill-buffers)
  (advice-add #'projectile-switch-project :around #'my/projectile-disable-remove-current-project)
  (projectile-mode 1)
  :bind (("C-c f" . projectile-find-file-in-known-projects)
         ("C-c C-f" . projectile-find-file)
         :map projectile-command-map
         ("s g" . counsel-git-grep)
         ("s s" . my/counsel-projectile-rg)))

(use-package rainbow-delimiters
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package syntax-subword
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'global-syntax-subword-mode 1)))

(use-package systemd
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'global-undo-tree-mode 1)))

(use-package web-mode
  :ensure t
  :mode "\\.hbs\\'"
  :init (setq web-mode-code-indent-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2))

(use-package whitespace-cleanup-mode
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'global-whitespace-cleanup-mode 1)))

(use-package wrap-region
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'wrap-region-global-mode 1)))

(use-package yasnippet
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (apply-partially #'yas-global-mode 1))
  :config
  (setq yas-prompt-functions '(yas-completing-prompt) ; use normal completion
        yas-verbosity 1)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))

;;; init.el ends here

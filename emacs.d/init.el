;;; init.el --- emacs config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'cl-macs)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'config-defuns-autoloads)

(setq package-check-signature nil
      package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))
(use-package bind-key
  :ensure t)
(require 'config-looks)

(use-package misc
  :commands (zap-to-char zap-up-to-char copy-from-above-command))

(bind-key "C-x r q" 'save-buffers-kill-emacs)
(unbind-key "C-x C-c")
(bind-key "<home>" 'mwim-beginning-of-code-or-line)
(bind-key "<end>" 'end-of-line)
(bind-key "<escape>" 'keyboard-escape-quit)
(bind-key "<f5>" 'my/revert-buffer-no-confirmation)
(bind-key "M-<f9>" 'vc-revision-other-window)
(bind-key "<f11>" 'my/cleanup-buffer)
(bind-key "S-<f11>" 'whitespace-cleanup)
(bind-key "S-<f12>" 'my/find-user-init-file)

(bind-key "M-<return>" 'my/open-line-below)
(bind-key "M-S-<return>" 'my/open-line-above)
(bind-key "C-<delete>" 'kill-word)
(bind-key "M-SPC" 'cycle-spacing)

(bind-key "C-<tab>" 'previous-buffer)
(bind-key "C-S-<iso-lefttab>" 'next-buffer)

(bind-key "C-n" 'my/scroll-up)
(bind-key "C-p" 'my/scroll-down)
(bind-key "M-n" 'my/scroll-other-window-up)
(bind-key "M-p" 'my/scroll-other-window-down)

(bind-key "C-z" 'repeat)
(unbind-key "C-x C-z")
(bind-key "C-!" 'kill-this-buffer)
(bind-key "C-M-!" 'my/kill-buffer-other-window)
(bind-key "C-^" 'bury-buffer)
(bind-key "C-#" 'quick-calc)

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

(bind-key "C-x C-p" 'my/show-buffer-file-name)

(bind-key "C-x n r" 'narrow-to-region)
(bind-key "C-x n n" 'my/narrow-or-widen-dwim)

(bind-key [remap goto-line] 'my/goto-line-with-feedback)

(use-package hydra
  :ensure t
  :bind ("<f8>" . my/hydra-error/body)
  :init
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

(fset 'yes-or-no-p 'y-or-n-p)

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
      compilation-scroll-output 'first-error
      compilation-read-command nil
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

(setq
 ;; ediff
 ediff-split-window-function 'split-window-horizontally
 ;; doc-view
 doc-view-continuous t
 ;; dired
 dired-isearch-filenames t
 dired-recursive-deletes 'always
 ;; org
 org-replace-disputed-keys t
 org-src-fontify-natively t
 org-startup-indented t
 org-html-postamble nil
 ;; glasses
 glasses-separate-parentheses-p nil
 glasses-uncapitalize-p t
 ;; eldoc
 eldoc-idle-delay 0.1
 ;; imenu
 imenu-auto-rescan t
 ;; tramp
 tramp-use-ssh-controlmaster-options nil
 tramp-default-method "scpx"
 ;; uniquify
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 )

(add-hook 'dired-mode-hook 'my/dired-mode-hook)
(add-hook 'org-mode-hook 'my/org-mode-hook)
(add-hook 'rst-mode-hook 'my/rst-mode-hook)

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plinkx")
  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
  (add-to-list 'exec-path "C:/Go/bin"))

(defun my/balance-windows (&rest args)
  "Call `balance-windows' while ignoring ARGS."
  (balance-windows))
(advice-add 'split-window-right :after 'my/balance-windows)
(advice-add 'split-window-below :after 'my/balance-windows)
(advice-add 'delete-window :after 'my/balance-windows)

(defun my/indent-yanked-region (&rest args)
  "Indent region in major modes that don't mind indentation, ignoring ARGS."
  (if (and
       (derived-mode-p 'prog-mode)
       (not (member major-mode '(python-mode ruby-mode makefile-mode))))
      (let ((mark-even-if-inactive transient-mark-mode))
        (indent-region (region-beginning) (region-end) nil))))
(advice-add 'yank :after 'my/indent-yanked-region)
(advice-add 'yank-pop :after 'my/indent-yanked-region)

(defun my/colorize-compilation-buffer ()
  "Colorize complication buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

(use-package autorevert
  :init (setq auto-revert-verbose nil
              global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode 1))

(use-package recentf
  :init (setq recentf-max-saved-items 1000)
  :config (recentf-mode 1))

(use-package smex
  :ensure t
  :init (setq smex-history-length 3))

(use-package wgrep
  :ensure t
  :init (setq wgrep-auto-save-buffer t))

(use-package ivy
  :ensure t
  :defer 0
  :bind (("C-c s". ivy-resume)
         ("C-x C-r" . ivy-recentf))
  :init (setq ivy-use-virtual-buffers t
              ivy-count-format "(%d/%d) "
              ivy-extra-directories '("./")
              ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :bind (:map ivy-minibuffer-map
              ("C-m" . ivy-alt-done)
              ("C-j" . ivy-done)
              ("<next>" . ivy-scroll-up-command)
              ("<prior>" . ivy-scroll-down-command))
  :config
  (ivy-add-actions
   t
   '(("I" insert "insert in buffer")))
  (ivy-set-actions
   'projectile-switch-project
   `(("g" magit-status "magit status")
     ("s" ,(apply-partially 'counsel-rg nil) "search (rg)")))
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x y" . counsel-yank-pop)
         ("C-c a" . counsel-rg)
         ("C-c u" . counsel-unicode-char)
         ("M-i" . counsel-imenu))
  :init (setq counsel-find-file-ignore-regexp (concat
                                               ;; file names beginning with # or .
                                               "\\(?:\\`[#.]\\)"
                                               ;; file names ending with # or ~
                                               "\\|\\(?:[#~]\\'\\)")
              counsel-rg-base-command "rg -S --no-heading --line-number --max-columns 150 --color never %s ."))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-S-s" . my/swiper-region-or-current-word)))

(use-package cua-base
  :init (setq cua-enable-cua-keys nil)
  :config (cua-mode 1))

(use-package paren
  :init (setq show-paren-delay 0)
  :config (show-paren-mode 1))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-clang-language-standard "c++1z")
  (add-hook 'after-init-hook 'global-flycheck-mode 1))

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
  :config (add-hook 'c-mode-common-hook 'my/c-mode-common-hook))

(use-package python
  :mode ("SCons\\(truct\\|cript\\)\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-<f8>" . my/pylint-ignore-errors-at-point))
  :config (unbind-key "C-c C-f" python-mode-map))

(use-package pyvenv
  :ensure t
  :init (add-hook 'hack-local-variables-hook 'my/pyvenv-activate))


(use-package go-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-save-mode 1))

(use-package cmake-font-lock
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists\\.txt\\'"
  :config (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package sh-script
  :mode ("rc\\'" . sh-mode))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :init (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                                 try-expand-dabbrev-all-buffers
                                                 try-expand-dabbrev-from-kill
                                                 try-complete-file-name-partially
                                                 try-complete-file-name
                                                 try-expand-all-abbrevs
                                                 try-complete-lisp-symbol-partially
                                                 try-complete-lisp-symbol)))

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

(use-package anzu
  :ensure t
  :config (global-anzu-mode 1))

(use-package company
  :ensure t
  :init (setq company-idle-delay 0
              company-minimum-prefix-length 2
              company-backends '(company-bbdb company-nxml company-css company-eclim company-semantic company-xcode company-cmake company-capf company-files (company-dabbrev-code company-gtags company-keywords) company-oddmuse company-dabbrev))
  :config (global-company-mode 1))

(use-package company-statistics
  :ensure t
  :config (company-statistics-mode 1))

(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode 1))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

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
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package eyebrowse
  :ensure t
  :init (setq eyebrowse-wrap-around t
              eyebrowse-new-workspace t)
  :config (eyebrowse-mode t))

(use-package git-messenger
  :ensure t
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("d" . my/git-messenger-show-with-magit))
  :init (setq git-messenger:show-detail t))

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(use-package which-key
  :ensure t
  :init (setq which-key-idle-delay 0.5)
  :config (which-key-mode 1))

(use-package highlight-symbol
  :ensure t
  :bind (("C-\"" . highlight-symbol-at-point)
         ("C-," . highlight-symbol-prev)
         ("C-." . highlight-symbol-next)
         ("M-s o" . highlight-symbol-occur))
  :init (setq highlight-symbol-idle-delay 0))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
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
  (add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook))

(use-package magit
  :ensure t
  :bind (("<f9>" . magit-status)
         ("S-<f9>" . magit-log-buffer-file)
         ("C-<f9>" . magit-blame)
         ("C-c g" . magit-dispatch-popup))
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window
        magit-repository-directories '(("~/dev" . 1))
        magit-tag-arguments '("--annotate")
        magit-push-always-verify nil
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (remove-hook 'magit-pre-display-buffer-hook 'magit-save-window-configuration))

(use-package git-commit
  :init
  (setq git-commit-summary-max-length 80
        git-commit-fill-column 80)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  :config (global-git-commit-mode t))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :init
  (add-hook 'markdown-mode-hook 'my/markdown-mode-hook)
  (setq markdown-command "markdown_py"))

(use-package multiple-cursors
  :ensure t
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
  :config
  ;; making paredit work with delete-selection-mode
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

(use-package projectile
  :ensure t
  :defer 0
  :init (setq projectile-completion-system 'ivy
              projectile-use-git-grep t)
  :config
  (fset 'projectile-kill-buffers 'my/projectile-kill-buffers)
  (advice-add 'projectile-switch-project :around 'my/projectile-disable-remove-current-project)
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
  :config (global-syntax-subword-mode 1))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :init (setq web-mode-code-indent-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

(use-package wrap-region
  :ensure t
  :config (wrap-region-global-mode 1))

(use-package yasnippet
  :ensure t
  :init (setq yas-prompt-functions '(yas-completing-prompt) ; use normal completion
              yas-verbosity 1)
  :config (yas-global-mode 1))

(bind-keys :map prog-mode-map
           ("<return>" . newline-and-indent))
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

(bind-key "C-c C-e" 'my/eval-and-replace emacs-lisp-mode-map)
(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)

;;; init.el ends here

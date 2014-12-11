;;; config-hooks.el --- major mode hooks
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook #'global-flycheck-mode)

;; ibuffer
(setq ibuffer-saved-filter-groups '(("default"
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
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; dired
(add-hook 'dired-mode-hook (lambda ()
                             (require 'dired-x)
                             (dired-omit-mode 1)))

;; org-mode
(add-hook 'org-mode-hook (lambda ()
                           (make-local-variable 'show-paren-mode)
                           (setq show-paren-mode nil)
                           (flyspell-mode t)))

;; rst-mode
(add-hook 'rst-mode-hook (lambda ()
                           (flyspell-mode t)))

;; Programming
(add-hook 'prog-mode-hook (lambda ()
                            (subword-mode t)
                            (drag-stuff-mode t)
                            (rainbow-delimiters-mode t)
                            (setq show-trailing-whitespace t)
                            (font-lock-add-keywords
                             nil
                             '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\)\\>" 1 font-lock-warning-face t)))
                            (local-set-key (kbd "<return>") 'newline-and-indent)
                            (local-set-key (kbd "C-<delete>") 'subword-kill)
                            (local-set-key (kbd "C-<right>") 'subword-forward)
                            (local-set-key (kbd "C-<left>") 'subword-backward)))

;; C/C++
(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key (kbd "C-c o") 'ff-get-other-file)
                                (setq comment-start "/*"
                                      comment-end "*/")))

(add-to-list 'auto-mode-alist '("\\.x\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("SCons\\(truct\\|cript\\)\\'" . python-mode))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode t)
                                  (paredit-mode t)
                                  (local-set-key (kbd "C-c C-e") 'my/eval-and-replace)
                                  (add-hook 'after-save-hook (lambda () (byte-compile-file buffer-file-name))
                                            nil t)))

(add-hook 'python-mode-hook (lambda ()
                              (define-key python-mode-map (kbd "C-c C-f") nil)))

;; CMake
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

(provide 'config-hooks)
;;; config-hooks.el ends here

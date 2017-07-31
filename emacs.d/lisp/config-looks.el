;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

(cond
 ((find-font (font-spec :name "Operator Mono"))
  (set-frame-font "Operator Mono 11" nil t))
 ((find-font (font-spec :name "Fira Mono"))
  (set-frame-font "Fira Mono Bold 10" nil t)))

(use-package doom-one-theme
  :ensure doom-themes
  :config
  (load-theme 'doom-one t)
  (set-face-attribute 'vertical-border nil :background "#3f444a" :foreground "#3f444a")
  (set-face-background 'mode-line-inactive (face-attribute 'mode-line :background)))

(use-package git-gutter-fringe
  :defer t
  :config (set-face-foreground 'git-gutter-fr:modified "#da8548"))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode 1))

(use-package powerline
  :defer t
  :config (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

(use-package spaceline
  :defer t
  :config (setq spaceline-responsive nil))

(use-package spaceline-all-the-icons
  :ensure t
  :config
  (setq spaceline-all-the-icons-primary-separator ""
        spaceline-all-the-icons-secondary-separator ""
        spaceline-all-the-icons-separator-type 'none
        spaceline-all-the-icons-icon-set-modified 'circle
        spaceline-all-the-icons-icon-set-window-numbering 'square
        spaceline-all-the-icons-flycheck-alternate t)
  (spaceline-all-the-icons-theme))

(provide 'config-looks)

;;; config-looks.el ends here

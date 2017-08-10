;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

(let ((font-size (if (eq system-type 'darwin) 15 12)))
  (cond
   ((find-font (font-spec :name "Fantasque Sans Mono"))
    (set-frame-font (format "Fantasque Sans Mono %d" font-size) nil t))
   ((find-font (font-spec :name "Fira Mono"))
    (set-frame-font (format "Fira Mono %d" font-size) nil t))))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

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

(when window-system
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)

(provide 'config-looks)

;;; config-looks.el ends here

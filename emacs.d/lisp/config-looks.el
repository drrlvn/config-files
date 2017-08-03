;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

(let ((font-size (if (eq system-type 'darwin) 15 10)))
  (cond
   ((find-font (font-spec :name "Operator Mono"))
    (set-frame-font (format "Operator Mono %d" (1+ font-size)) nil t))
   ((find-font (font-spec :name "Fira Mono"))
    (set-frame-font (format (concat "Fira Mono " (if (eq system-type 'gnu/linux) "Bold ") "%d") font-size) nil t))))

(use-package doom-one-theme
  :ensure doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config)
  (doom-themes-set-faces 'doom-one (vertical-border :background base4 :foreground base4)))

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

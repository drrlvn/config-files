;;; config-looks.el --- look configuration -*- lexical-binding: t; byte-compile-warnings: (not unresolved) -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

(unless (get 'default 'saved-face)
  (let ((font-size (if (eq system-type 'darwin) 15 12)))
    (cond
     ((find-font (font-spec :name "Fantasque Sans Mono"))
      (set-frame-font (format "Fantasque Sans Mono %d" font-size) nil t))
     ((find-font (font-spec :name "Fira Mono"))
      (set-frame-font (format "Fira Mono %d" font-size) nil t)))))

(use-package doom-themes
  :ensure
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package window-numbering
  :ensure
  :config (window-numbering-mode 1))

(use-package powerline
  :defer
  :config (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq spaceline-responsive nil
        spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
  (spaceline-toggle-minor-modes-off)
  (spaceline-spacemacs-theme))

(use-package mode-icons
  :ensure
  :config (mode-icons-mode 1))

(when window-system
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 1)

(provide 'config-looks)

;;; config-looks.el ends here

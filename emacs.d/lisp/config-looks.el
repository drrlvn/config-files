;;; config-looks.el --- look configuration
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

(set-frame-font "Fira Mono 10" nil t)

(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode 1))
(use-package powerline
  :ensure t
  :init (setq powerline-default-separator 'wave))
(use-package spaceline-config
  :ensure spaceline
  :init (setq spaceline-workspace-numbers-unicode t
              spaceline-window-numbers-unicode t)
  :config
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-flycheck-error-off)
  (spaceline-toggle-flycheck-warning-off)
  (spaceline-toggle-flycheck-info-off)
  (spaceline-spacemacs-theme))

(provide 'config-looks)
;;; config-looks.el ends here

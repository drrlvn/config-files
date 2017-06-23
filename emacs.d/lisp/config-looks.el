;;; config-looks.el --- look configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

(if (find-font (font-spec :name "Fira Mono"))
    (set-frame-font "Fira Mono 10" nil t))

(use-package atom-one-dark-theme
  :ensure t
  :config (load-theme 'atom-one-dark t))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode 1))
(use-package powerline
  :ensure t
  :config (setq powerline-default-separator 'wave))
(use-package spaceline-config
  :ensure spaceline
  :config
  (setq spaceline-workspace-numbers-unicode t
        spaceline-window-numbers-unicode t)
  (spaceline-toggle-minor-modes-off)
  (spaceline-spacemacs-theme))

(use-package mode-icons
  :ensure t
  :config (mode-icons-mode 1))

(provide 'config-looks)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:

;;; config-looks.el ends here

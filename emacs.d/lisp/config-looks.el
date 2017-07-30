;;; config-looks.el --- look configuration -*- lexical-binding: t; -*-
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

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-one t))

(use-package window-numbering
  :ensure t
  :config (window-numbering-mode 1))

(use-package spaceline-all-the-icons
  :ensure t
  :config
  (setq spaceline-all-the-icons-primary-separator ""
        spaceline-all-the-icons-secondary-separator ""
        spaceline-all-the-icons-separator-type 'none
        spaceline-all-the-icons-flycheck-alternate t)
  (spaceline-all-the-icons-theme))

(provide 'config-looks)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:

;;; config-looks.el ends here

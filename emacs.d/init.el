;;; init.el --- emacs config
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'config-defuns)
(require 'config-misc)
(require 'config-keys)
(require 'config-hooks)
(require 'config-packages)

;;; init.el ends here

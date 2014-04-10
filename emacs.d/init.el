;;; init.el --- emacs config
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'config-defuns)
(require 'config-misc)
(require 'config-keys)
(require 'config-packages)
(require 'config-hooks)

;;; init.el ends here

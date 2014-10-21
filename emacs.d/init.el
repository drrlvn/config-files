;;; init.el --- emacs config
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file)

(require 'config-defuns)
(require 'config-misc)
(require 'config-keys)
(require 'config-packages)
(require 'config-hooks)

;;; init.el ends here

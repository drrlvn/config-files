;;; config-defuns-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "config-defuns" "config-defuns.el" (21746 57667
;;;;;;  883115 28000))
;;; Generated autoloads from config-defuns.el

(autoload 'my/install-packages "config-defuns" "\
Install given packages.

\(fn &rest PACKAGES)" nil nil)

(autoload 'my/cleanup-buffer "config-defuns" "\
Perform a bunch of operations on the whitespace content of a buffer.

\(fn)" t nil)

(autoload 'my/filter-buffer "config-defuns" "\
Run shell command on buffer and replace it with the output.

\(fn)" t nil)

(autoload 'my/open-line-below "config-defuns" "\
Go to end of line, then insert newline and indent.

\(fn)" t nil)

(autoload 'my/open-line-above "config-defuns" "\
Go to end of line, then insert newline and indent.

\(fn)" t nil)

(autoload 'my/diff-current-buffer-with-file "config-defuns" "\
View the differences between current buffer and its associated file.

\(fn)" t nil)

(autoload 'my/revert-buffer-no-confirmation "config-defuns" "\
Invoke `revert-buffer' without the confirmation.

\(fn)" t nil)

(autoload 'my/kill-buffer-other-window "config-defuns" "\
Kill buffer in other window.

\(fn)" t nil)

(autoload 'my/url-edit "config-defuns" "\
Open a new buffer with the contents of the URL provided.

\(fn URL)" t nil)

(autoload 'my/isearch-current-region-or-word "config-defuns" "\
Reset current isearch to a search of the region or the word under point.

\(fn)" t nil)

(autoload 'my/autoload-and-set-key "config-defuns" "\
Autoloads PACKAGE for keys and function pairs in KEYS-AND-FUNCTIONS.

\(fn PACKAGE KEYS-AND-FUNCTIONS)" nil nil)

(autoload 'my/eval-and-replace "config-defuns" "\
Replace the preceding sexp with its value.

\(fn)" t nil)

(autoload 'my/rotate-windows "config-defuns" "\
Rotate your windows

\(fn)" t nil)

(autoload 'my/toggle-comment-line-or-region "config-defuns" "\
Toggle comment on line if no region is active, or comment region.

\(fn)" t nil)

(autoload 'my/smart-beginning-of-line "config-defuns" "\
Move point to first non-whitespace character or beginning-of-line.

\(fn)" t nil)

(autoload 'my/increment-number-at-point "config-defuns" "\


\(fn N)" t nil)

(autoload 'my/decrement-number-at-point "config-defuns" "\


\(fn N)" t nil)

(autoload 'my/goto-line-with-feedback "config-defuns" "\
Show line numbers temporarily, while prompting for the line number input

\(fn)" t nil)

(autoload 'my/get-current-class "config-defuns" "\


\(fn)" nil nil)

(autoload 'my/insert-default-ctor "config-defuns" "\


\(fn)" t nil)

(autoload 'my/insert-virtual-dtor "config-defuns" "\


\(fn)" t nil)

(autoload 'my/insert-copy-ctor "config-defuns" "\


\(fn)" t nil)

(autoload 'my/insert-copy-assignment-operator "config-defuns" "\


\(fn)" t nil)

(autoload 'my/insert-move-ctor "config-defuns" "\


\(fn)" t nil)

(autoload 'my/insert-move-assignment-operator "config-defuns" "\


\(fn)" t nil)

(autoload 'my/insert-all-special "config-defuns" "\


\(fn)" t nil)

(autoload 'my/add-super-char-to-ace-jump-mode "config-defuns" "\


\(fn M C)" nil nil)

(autoload 'my/projectile-kill-buffers "config-defuns" "\


\(fn)" t nil)

;;;***

(provide 'config-defuns-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; config-defuns-autoloads.el ends here

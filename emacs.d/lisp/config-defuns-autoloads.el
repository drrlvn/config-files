;;; config-defuns-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "config-defuns" "config-defuns.el" (22586 41679
;;;;;;  883099 386000))
;;; Generated autoloads from config-defuns.el

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

(autoload 'my/swiper-region-or-current-word "config-defuns" "\
Run swiper on region or current word.

\(fn)" t nil)

(autoload 'my/counsel-projectile-ag "config-defuns" "\
Run `counsel-ag' in the project root.

\(fn)" t nil)

(autoload 'my/autoload-and-set-key "config-defuns" "\
Autoloads PACKAGE for keys and function pairs in KEYS-AND-FUNCTIONS.

\(fn PACKAGE KEYS-AND-FUNCTIONS)" nil nil)

(autoload 'my/eval-and-replace "config-defuns" "\
Replace the preceding sexp with its value.

\(fn)" t nil)

(autoload 'my/rotate-windows "config-defuns" "\
Rotate your windows.

\(fn)" t nil)

(autoload 'my/toggle-comment-line-or-region "config-defuns" "\
Toggle comment on line if no region is active, or comment region.

\(fn)" t nil)

(autoload 'my/increment-number-at-point "config-defuns" "\
Increment number at point by N.

\(fn N)" t nil)

(autoload 'my/decrement-number-at-point "config-defuns" "\
Decrement number at point by N.

\(fn N)" t nil)

(autoload 'my/goto-line-with-feedback "config-defuns" "\
Show line numbers temporarily, while prompting for the line number input.

\(fn)" t nil)

(autoload 'my/projectile-disable-remove-current-project "config-defuns" "\
Call ORIG-FUN with ARGS while replacing projectile--remove-current-project with identity function.

\(fn ORIG-FUN &rest ARGS)" nil nil)

(autoload 'my/projectile-switch-to-git "config-defuns" "\
Run `projectile-vc' (magit) on selected project.

\(fn)" t nil)

(autoload 'my/projectile-switch-to-ag "config-defuns" "\
Run `my/counsel-projectile-ag' on selected project.

\(fn)" t nil)

(autoload 'my/narrow-or-widen-dwim "config-defuns" "\
Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed.

Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html

\(fn P)" t nil)

(autoload 'my/get-current-class "config-defuns" "\
Return name of enclosing class.

\(fn)" nil nil)

(autoload 'my/insert-default-ctor "config-defuns" "\
Insert default constructor.

\(fn)" t nil)

(autoload 'my/insert-virtual-dtor "config-defuns" "\
Insert virtual destructor.

\(fn)" t nil)

(autoload 'my/insert-copy-ctor "config-defuns" "\
Insert copy constructor.

\(fn)" t nil)

(autoload 'my/insert-copy-assignment-operator "config-defuns" "\
Insert copy assignment operator.

\(fn)" t nil)

(autoload 'my/insert-move-ctor "config-defuns" "\
Insert move constructor.

\(fn)" t nil)

(autoload 'my/insert-move-assignment-operator "config-defuns" "\
Insert move assignment operator.

\(fn)" t nil)

(autoload 'my/insert-all-special "config-defuns" "\
Insert all special methods.

\(fn)" t nil)

(autoload 'my/add-super-char-to-avy "config-defuns" "\
Add binding for avy-goto M to key C.

\(fn M C)" nil nil)

(autoload 'my/projectile-kill-buffers "config-defuns" "\
Kill all buffers from current project.

\(fn)" t nil)

(autoload 'my/pylint-ignore-errors-at-point "config-defuns" "\


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

(defun my/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun my/zap-to-char-exclusive (arg char)
  "Kill up to but excluding ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (zap-to-char arg char)
  (insert char)
  (backward-char))

(defun my/sudo-edit (&optional arg)
  "Edit current buffer as super user.
Interactively, with prefix argument, sudo \\[find-file] instead."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "Find file (sudo): ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun my/revert-buffer-no-confirmation ()
  "Invoke `revert-buffer' without the confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " buffer-file-name)))

(defun my/kill-buffer-other-window ()
  "Kill buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window -1))

(defun my/ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun my/isearch-current-region-or-word ()
  "Reset current isearch to a search of the region or the word under point."
  (interactive)
  (setq isearch-string ""
        isearch-message "")
  (isearch-yank-string (if (use-region-p)
                           (let ((region-beginning (region-beginning))
                                 (region-end (region-end)))
                             (deactivate-mark)
                             (buffer-substring region-beginning region-end))
                         (setq isearch-word t)
                         (thing-at-point 'word))))

(defun my/flymake-goto-error (goto-error-func)
  "Call GOTO-ERROR-FUNC and print flymake error info to echo area."
  (interactive)
  (funcall goto-error-func)
  (message "%s" (flymake-ler-text (caar (flymake-find-err-info flymake-err-info (flymake-current-line-no))))))

(defun my/autoload-and-set-key (package keys-and-functions)
  "Autoloads PACKAGE for keys and function pairs in KEYS-AND-FUNCTIONS."
  (dolist (key-and-function keys-and-functions)
    (let ((key (car key-and-function))
          (function (cadr key-and-function)))
      (autoload function package nil t)
      (global-set-key (read-kbd-macro key) function))))

(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((original-point (point)))
    (eval-last-sexp t)
    (let ((distance (- (point) original-point)))
      (backward-char distance)
      (backward-kill-sexp)
      (forward-char distance))))

(dolist (command '(kill-ring-save kill-region))
  (eval `(defadvice ,command (before current-line-or-region activate compile)
           "When called interactively with no active region, use a single line instead."
           (interactive
            (if (use-region-p)
                (list (region-beginning) (region-end))
              (list (line-beginning-position) (line-beginning-position 2)))))))

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate compile)
           "If `major-mode' derives from `prog-mode' then `indent-region' after yank."
           (if (derived-mode-p 'prog-mode)
               (let ((mark-even-if-inactive transient-mark-mode))
                 (indent-region (region-beginning) (region-end) nil))))))

;; for ace-jump-mode
(defun my/add-super-char-to-ace-jump-mode (m c)
  (global-set-key
   (read-kbd-macro (concat "s-" (string c)))
   `(lambda () (interactive) (,(intern (concat "ace-jump-" (symbol-name m) "-mode")) ,c))))

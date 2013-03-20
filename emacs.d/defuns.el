(defun my/install-packages (&rest packages)
  "Install given packages."
  (let ((refreshed-contents nil))
    (mapc
     (lambda (package)
       (unless (package-installed-p package)
         (unless refreshed-contents
           (setq refreshed-contents t)
           (package-refresh-contents))
         (package-install package)))
     packages)))

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

(defun my/open-line-below ()
  "Go to end of line, then insert newline and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun my/open-line-above ()
  "Go to end of line, then insert newline and indent."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun my/diff-current-buffer-with-file ()
  "View the differences between current buffer and its associated file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

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

(defun my/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun my/toggle-comment-line-or-region ()
  "Toggle comment on line if no region is active, or comment region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (if (and this-command-keys-shift-translated (not mark-active))
    (push-mark nil t t))
  (let ((oldpoint (point)))
    (beginning-of-line-text)
    (if (= oldpoint (point))
        (beginning-of-line))))

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

(defun my/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; for ace-jump-mode
(defun my/add-super-char-to-ace-jump-mode (m c)
  (global-set-key
   (read-kbd-macro (concat "s-" (string c)))
   `(lambda () (interactive) (,(intern (concat "ace-jump-" (symbol-name m) "-mode")) ,c))))

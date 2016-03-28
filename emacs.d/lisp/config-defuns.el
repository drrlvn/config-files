;;; config-defuns.el --- custom functions, macros and advices
;;; Commentary:
;;; Code:

;;;###autoload
(defun my/install-packages (&rest packages)
  "Install given packages."
  (dolist (package packages)
    (unless (package-installed-p package)
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

;;;###autoload
(defun my/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (if (derived-mode-p 'go-mode)
      (gofmt)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

;;;###autoload
(defun my/filter-buffer ()
  "Run shell command on buffer and replace it with the output."
  (interactive)
  (let ((prev-point (point)))
    (call-process-region (point-min) (point-max) shell-file-name t t nil shell-command-switch
                         (read-shell-command "Shell command on buffer: "))
    (goto-char prev-point)))

;;;###autoload
(defun my/open-line-below ()
  "Go to end of line, then insert newline and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;;###autoload
(defun my/open-line-above ()
  "Go to end of line, then insert newline and indent."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;;###autoload
(defun my/diff-current-buffer-with-file ()
  "View the differences between current buffer and its associated file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;;###autoload
(defun my/revert-buffer-no-confirmation ()
  "Invoke `revert-buffer' without the confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " buffer-file-name)))

;;;###autoload
(defun my/kill-buffer-other-window ()
  "Kill buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window -1))

;;;###autoload
(defun my/url-edit (url)
  "Open a new buffer with the contents of the URL provided."
  (interactive "sURL: ")
  (url-retrieve url
                (lambda (status url)
                  (progn
                    (rename-buffer url)
                    (fundamental-mode)
                    (switch-to-buffer (current-buffer))))
                (list url)))

;;;###autoload
(defun my/swiper-region-or-current-word ()
  "Run swiper on region or current word"
  (interactive)
  (swiper
   (if (region-active-p)
       (buffer-substring (region-beginning) (region-end))
     (current-word))))

;;;###autoload
(defun my/autoload-and-set-key (package keys-and-functions)
  "Autoloads PACKAGE for keys and function pairs in KEYS-AND-FUNCTIONS."
  (dolist (key-and-function keys-and-functions)
    (let ((key (car key-and-function))
          (function (cadr key-and-function)))
      (autoload function package nil t)
      (global-set-key (read-kbd-macro key) function))))

;;;###autoload
(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((original-point (point)))
    (let ((eval-expression-print-length nil)
          (eval-expression-print-level nil))
      (eval-last-sexp t))
    (let ((distance (- (point) original-point)))
      (backward-char distance)
      (backward-kill-sexp)
      (forward-char distance))))

;;;###autoload
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

;;;###autoload
(defun my/toggle-comment-line-or-region ()
  "Toggle comment on line if no region is active, or comment region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (if (and this-command-keys-shift-translated (not mark-active))
    (push-mark nil t t))
  (let ((oldpoint (point)))
    (beginning-of-line-text)
    (if (= oldpoint (point))
        (beginning-of-line))))

;;;###autoload
(defun my/increment-number-at-point (n)
  (interactive "p")
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (str (buffer-substring start end))
         (new-num (number-to-string (+ n (string-to-number str)))))
    (delete-region start end)
    (insert (if (s-starts-with? "0" str) (s-pad-left (length str) "0" new-num) new-num))))

;;;###autoload
(defun my/decrement-number-at-point (n)
  (interactive "p")
  (my/increment-number-at-point (- n)))

;;;###autoload
(defun my/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (let ((current-prefix-arg (read-number "Goto line: ")))
          (call-interactively 'goto-line)))
    (linum-mode -1)))


;; C++ auto insert

;;;###autoload
(defun my/get-current-class ()
  (save-excursion
    (search-backward "class" nil t)
    (forward-word 2)
    (backward-word)
    (current-word)))

;;;###autoload
(defun my/insert-default-ctor ()
  ""
  (interactive)
  (insert (my/get-current-class) "() = default;"))

;;;###autoload
(defun my/insert-virtual-dtor ()
  ""
  (interactive)
  (insert "virtual ~" (my/get-current-class) "() = default;"))

;;;###autoload
(defun my/insert-copy-ctor ()
  ""
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class "(const " current-class " &) = default;")))

;;;###autoload
(defun my/insert-copy-assignment-operator ()
  ""
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class " & operator=(const " current-class " &) = default;")))

;;;###autoload
(defun my/insert-move-ctor ()
  ""
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class "(" current-class " &&) = default;")))

;;;###autoload
(defun my/insert-move-assignment-operator ()
  ""
  (interactive)
  (let ((current-class (my/get-current-class)))
    (insert current-class " & operator=(" current-class " &&) = default;")))

;;;###autoload
(defun my/insert-all-special ()
  ""
  (interactive)
  (my/insert-copy-ctor)
  (newline-and-indent)
  (my/insert-copy-assignment-operator)
  (newline-and-indent)
  (my/insert-move-ctor)
  (newline-and-indent)
  (my/insert-move-assignment-operator)
  (newline-and-indent)
  )

;; for avy
;;;###autoload
(defun my/add-super-char-to-avy (m c)
  (global-set-key
   (read-kbd-macro (concat "s-" (string c)))
   `(lambda () (interactive) (,(intern (concat "avy-goto-" (symbol-name m))) ,c))))

;;;###autoload
(defun my/projectile-kill-buffers ()
  (interactive)
  (mapc 'kill-buffer (-remove 'buffer-base-buffer (projectile-project-buffers))))

(provide 'config-defuns)
;;; config-defuns.el ends here

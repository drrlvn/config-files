(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/tomorrow-theme/GNU Emacs")

;; set font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :foreground "royal blue"))))
 '(diff-changed ((t (:weight bold))))
 '(diff-refine-change ((t (:background "grey20"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red3"))))
 '(magit-diff-add ((t (:inherit diff-added))))
 '(magit-diff-del ((t (:inherit diff-removed))))
 '(magit-item-highlight ((t nil)))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#c397d8"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#7aa6da"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#70c0b1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#b9ca4a"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#e7c547"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#e78c45"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#d54e53"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#969896"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#eaeaea"))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes (quote ("ca2d69f5dd853dbf6fbcf5d0f1759ec357fda19c481915431015417ec9c1fbd8" default)))
 '(dabbrev-case-replace nil)
 '(default-frame-alist (quote ((font . "Ubuntu Mono 12"))))
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(flyspell-auto-correct-binding [(control 39)])
 '(frame-background-mode (quote dark))
 '(global-auto-revert-mode t)
 '(global-ede-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(history-length 500)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 25 25 :left :elide) " " (size 6 -1 :right) " " (mode 10 10 :left :elide) " " (filename-and-process -1 60 :left :elide)) (mark " " (name 30 -1) " " filename))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-prospects 128)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-whole-line t)
 '(lazy-highlight-initial-delay 0)
 '(recentf-max-saved-items 250)
 '(recentf-mode t)
 '(save-place t nil (saveplace))
 '(scroll-preserve-screen-position t)
 '(semantic-default-submodes (quote (global-semantic-stickyfunc-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode)))
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-separator ":")
 '(winner-mode t nil (winner)))

;; mode-line
(setq-default
 mode-line-format
 (list
  " "
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b") 'face '(font-lock-keyword-face bold)
                      'help-echo (buffer-file-name)))
  " "
  ;; was this buffer modified since the last save?
  '(:eval (when (buffer-modified-p) (propertize "[*] " 'face 'font-lock-warning-face 'help-echo "Buffer has been modified")))
  ;; relative position, size of file
  "["
  (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
  "/"
  '(:eval (propertize (concat (int-to-string (count-lines (point-min) (point-max))) "L") 'face 'font-lock-constant-face)) ;; line num
  "/"
  (propertize "%I" 'face 'font-lock-constant-face) ;; size
  "] "
  ;; encoding and eol-type
  "["
  '(:eval (concat
           (propertize (upcase (symbol-name (coding-system-get buffer-file-coding-system :mime-charset))) 'face 'font-lock-comment-face
                       'help-echo (coding-system-doc-string buffer-file-coding-system))
           (propertize
            (case (coding-system-eol-type buffer-file-coding-system)
              (0 "|UNIX")
              (1 "|DOS")
              (2 "|MAC")
              (t ""))
            'face 'font-lock-comment-face)))
  "] "
  ;; is this buffer in overwrite-mode?
  '(:eval (when overwrite-mode
            (concat "["
                    (propertize "Ovr" 'face 'font-lock-preprocessor-face 'help-echo "Buffer is in overwrite mode")
                    "] ")))
  ;; is this buffer read-only?
  '(:eval (when buffer-read-only
            (concat "["
                    (propertize "RO" 'face 'font-lock-type-face 'help-echo "Buffer is read-only")
                    "] ")))
  ;; the current major mode for the buffer.
  "{"
  (propertize "%m" 'face 'font-lock-function-name-face 'help-echo buffer-file-coding-system)
  ;; i don't want to see minor-modes; but if you want, uncomment this:
  ;; minor-mode-alist  ;; list of minor modes
  "} "
  ;; justify right by filling with spaces to right fringe
  (propertize " " 'display '((space :align-to (- right-fringe 15))))
  '(:eval (let ((spaces-needed (- 11 (length (format "(%d,%d)" (line-number-at-pos) (current-column))))))
            (make-string spaces-needed ? )))
  ;; line and column
  "(" ;; '%02' to set to 2 chars at least; prevents flickering
  (propertize "%l" 'face 'font-lock-type-face) "," (propertize "%c" 'face 'font-lock-type-face)
  ") "
  ;; add the time, with the date and the emacs uptime in the tooltip
  '(:eval (propertize (format-time-string "%H:%M") 'face 'bold
                      'help-echo (concat (format-time-string "%c; ") "Uptime: " (emacs-uptime "%D, %z%2h:%.2m"))))
  ))

;; misc configuration
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings 'meta)    ; enable windmove
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ring-bell-function 'ignore
      resize-mini-windows t)
(setq disabled-command-function nil)    ; enable all disabled commands

(setq ido-ignore-buffers (cons "^\\*.*\\*$" ido-ignore-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;
(defun revert-buffer-no-confirmation ()
  "Invoke `revert-buffer' without the confirmation."
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " buffer-file-name)))

(defun kill-buffer-other-window ()
  "Kill buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window -1))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun isearch-current-region-or-word ()
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
                         (word-at-point))))

(defun autoload-and-set-key (package keys-and-functions)
  "Autoloads PACKAGE for keys and function pairs in KEYS-AND-FUNCTIONS."
  (dolist (key-and-function keys-and-functions)
    (let ((key (car key-and-function))
          (function (cadr key-and-function)))
      (autoload function package nil t)
      (global-set-key key function))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mappings
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirmation)
(global-set-key (kbd "<f6>") 'ack-and-a-half)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f11>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-!") 'kill-this-buffer)
(global-set-key (kbd "C-M-!") 'kill-buffer-other-window)
(global-set-key (kbd "C-#") 'calculator)
(global-set-key (kbd "S-SPC") 'dabbrev-expand)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key isearch-mode-map (kbd "C-*") 'isearch-current-region-or-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;

;; ibuffer
(setq ibuffer-saved-filter-groups '(("default"
                                     ("Dired" (mode . dired-mode))
                                     ("C/C++" (or
                                               (mode . c-mode)
                                               (mode . c++-mode)))
                                     ("Python" (mode . python-mode))
                                     ("Elisp" (mode . emacs-lisp-mode))
                                     ("Docs" (or
                                              (mode . org-mode)
                                              (mode . rst-mode)))
                                     ("Misc" (name . "^\\*"))
                                     )))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; dired
(setq dired-isearch-filenames t)
(add-hook 'dired-mode-hook (lambda ()
                             (dired-omit-mode 1)))

;; org-mode
(setq org-replace-disputed-keys t
      org-startup-indented t)
(add-hook 'org-mode-hook (lambda ()
                           (make-local-variable 'show-paren-mode)
                           (setq show-paren-mode nil)
                           (flyspell-mode t)))

;; rst-mode
(add-hook 'rst-mode-hook (lambda ()
                           (flyspell-mode t)))

;; Programming
(add-hook 'prog-mode-hook (lambda ()
                            (semantic-mode t)
                            (subword-mode t)
                            (flyspell-prog-mode)
                            (rainbow-delimiters-mode t)
                            (setq show-trailing-whitespace t)
                            (font-lock-add-keywords
                             nil
                             '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\)\\>" 1 font-lock-warning-face t)))
                            (local-set-key (kbd "<return>") 'newline-and-indent)
                            (local-set-key (kbd "C-<delete>") 'subword-kill)
                            (local-set-key (kbd "C-<right>") 'subword-forward)
                            (local-set-key (kbd "C-<left>") 'subword-backward)
                            (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)))

;; C/C++
(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(setq c-default-style "bsd"
      c-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.x\\'" . c++-mode))

;; Python
(add-hook 'python-mode-hook (lambda () (flymake-mode)))
(require 'flymake)
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init))

(add-to-list 'auto-mode-alist '("SCons\\(truct\\|cript\\)\\'" . python-mode))

(global-set-key (kbd "S-<f7>") (lambda ()
                                 (interactive)
                                 (flymake-goto-prev-error)
                                 (message "%s"
                                          (flymake-ler-text (caar (flymake-find-err-info
                                                                   flymake-err-info
                                                                   (flymake-current-line-no)))))))
(global-set-key (kbd "S-<f8>") (lambda ()
                                 (interactive)
                                 (flymake-goto-next-error)
                                 (message "%s"
                                          (flymake-ler-text (caar (flymake-find-err-info
                                                                   flymake-err-info
                                                                   (flymake-current-line-no)))))))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "C-c C-e") 'eval-and-replace)
                                  (add-hook 'after-save-hook (lambda () (byte-compile-file buffer-file-name))
                                            nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external packages
;;

;; automatically add all packages to load-path
(let ((package-root-dir "~/.emacs.d/packages"))
  (dolist (file (directory-files package-root-dir))
    (let ((package-dir (concat package-root-dir "/" file)))
      (when (and (not (equal file "."))
                 (not (equal file ".."))
                 (file-directory-p package-dir))
        (add-to-list 'load-path package-dir)))))

;; ace-jump-mode
(autoload 'ace-jump-char-mode "ace-jump-mode" nil t)
(global-set-key (kbd "C-`") 'ace-jump-char-mode)

;; ack-and-a-half
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)

;; anything
(setq anything-input-idle-delay 0)
(autoload-and-set-key "anything-config" `((,(kbd "C-x a") anything-c-apropos)
                                          (,(kbd "C-x f") anything)
                                          (,(kbd "C-x y") anything-show-kill-ring)
                                          (,(kbd "M-X")   anything-M-x)
                                          (,(kbd "M-i")   anything-imenu)
                                          (,(kbd "M-s o") anything-occur)))

;; drag-stuff
(setq drag-stuff-modifier '(meta shift))
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; expand-region
(autoload 'er/expand-region "expand-region" nil t)
(global-set-key (kbd "C-=") 'er/expand-region)

;; iedit
(autoload 'iedit-mode "iedit" nil t)
(global-set-key (kbd "C-;") 'iedit-mode)

;; magit
(autoload 'magit-status "magit" nil t)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; minimap
(autoload 'minimap-create "minimap" nil t)
(setq minimap-update-delay 0.1
      minimap-width-fraction 0.1)

;; rainbow-delimiters
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" nil t)

;; smex
(defun load-smex (original-function)
  (interactive)
  (require 'smex)
  (if (fboundp 'smex-initialize)
      (smex-initialize))
  (global-set-key (kbd "M-x") 'smex)
  (funcall original-function))
(global-set-key (kbd "M-x") (lambda () (interactive) (load-smex 'smex)))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)

;; YASnippet
(require 'yasnippet)
(yas/global-mode 1)

;; zencoding
(autoload 'zencoding-mode "zencoding-mode" nil t)
(add-hook 'sgml-mode-hook 'zencoding-mode)

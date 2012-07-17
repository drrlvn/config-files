;; mode-line
(setq-default
 mode-line-format
 (list
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b") 'face '(font-lock-keyword-face bold)
                      'help-echo (buffer-file-name)))
  " "
  ;; was this buffer modified since the last save?
  '(:eval (when (buffer-modified-p) (propertize "[*] " 'face 'font-lock-warning-face 'help-echo "Buffer has been modified")))
  ;; line and column
  "(" ;; '%02' to set to 2 chars at least; prevents flickering
  (propertize "%l" 'face 'font-lock-type-face) "," (propertize "%c" 'face 'font-lock-type-face)
  ") "
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
           "|"
           (propertize
            (case (coding-system-eol-type buffer-file-coding-system)
              (0 "UNIX")
              (1 "DOS")
              (2 "MAC")
              (t "NIL"))
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
  (propertize "%n" 'face 'font-lock-type-face)
  "} "
  ;; justify right by filling with spaces to right fringe
  (propertize " " 'display '((space :align-to (- right 4))))
  ;; add the time, with the date and the emacs uptime in the tooltip
  '(:eval (propertize (format-time-string "%H:%M") 'face 'bold
                      'help-echo (concat (format-time-string "%c; ") "Uptime: " (emacs-uptime "%D, %z%2h:%.2m"))))
  ))

;; misc configuration
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'banish)
(windmove-default-keybindings 'super)   ; enable windmove
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ring-bell-function 'ignore
      resize-mini-windows t)
(setq disabled-command-function nil)    ; enable all disabled commands

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plinkx"))

(load "~/.emacs.d/defuns.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mappings
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f5>") 'my/revert-buffer-no-confirmation)
(global-set-key (kbd "<f6>") 'ack-and-a-half)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f11>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-!") 'kill-this-buffer)
(global-set-key (kbd "C-M-!") 'my/kill-buffer-other-window)
(global-set-key (kbd "C-#") 'calculator)
(global-set-key (kbd "C-x C-r") 'my/ido-recentf-open)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key isearch-mode-map (kbd "C-*") 'my/isearch-current-region-or-word)

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
(add-hook 'dired-mode-hook (lambda ()
                             (require 'dired-x)
                             (dired-omit-mode 1)))

;; org-mode
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
                            (drag-stuff-mode t)
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
(add-to-list 'auto-mode-alist '("\\.x\\'" . c++-mode))

;; Python
(add-hook 'python-mode-hook (lambda ()
                              (flymake-mode t)
                              (local-set-key (kbd "S-<f7>") (lambda ()
                                                              (interactive)
                                                              (my/flymake-goto-error 'flymake-goto-prev-error)))
                              (local-set-key (kbd "S-<f8>") (lambda ()
                                                              (interactive)
                                                              (my/flymake-goto-error 'flymake-goto-next-error)))))
(require 'flymake)
(defun flymake-pylint-init ()
  (let ((local-file (file-relative-name
                     (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)
                     (file-name-directory buffer-file-name))))
    `("epylint" (,local-file))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init))

(add-to-list 'auto-mode-alist '("SCons\\(truct\\|cript\\)\\'" . python-mode))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "C-c C-e") 'my/eval-and-replace)
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
(autoload 'ace-jump-word-mode "ace-jump-mode" nil t)
(global-set-key (kbd "C-`") 'ace-jump-word-mode)
(global-set-key (kbd "C-~") 'ace-jump-char-mode)

(loop for c from ?0 to ?9 do (my/add-super-char-to-ace-jump-word-mode c))
(loop for c from ?A to ?Z do (my/add-super-char-to-ace-jump-word-mode c))
(loop for c from ?a to ?z do (my/add-super-char-to-ace-jump-word-mode c))

;; ack-and-a-half
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)

;; anything
(setq anything-input-idle-delay 0)
(my/autoload-and-set-key "anything-config" '(("C-x a" anything-c-apropos)
                                             ("C-x f" anything)
                                             ("C-x g" anything-google-suggest)
                                             ("C-x y" anything-show-kill-ring)
                                             ("M-X"   anything-M-x)
                                             ("M-i"   anything-imenu)
                                             ("M-s o" anything-occur)))

;; drag-stuff
(setq drag-stuff-modifier '(meta shift))
(autoload 'drag-stuff-mode "drag-stuff" nil t)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

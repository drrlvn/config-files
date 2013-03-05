(eval-when-compile
  (require 'cl))

;; frame title
(setq frame-title-format
      '("" invocation-name ": " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b"))))

;; mode-line
(setq-default
 mode-line-format
 (list
  " "
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize "%b" 'face '(font-lock-keyword-face bold) 'help-echo (buffer-file-name)))
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

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plinkx")
  (windmove-default-keybindings 'meta))

(load "~/.emacs.d/defuns.el")

(package-initialize)
(my/install-packages
 'ace-jump-mode
 'ack-and-a-half
 'drag-stuff
 'expand-region
 'helm
 'iedit
 'magit
 'markdown-mode
 'multiple-cursors
 'rainbow-delimiters
 'smex
 'undo-tree
 'wrap-region
 'yasnippet
 'zencoding-mode
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mappings
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'my/revert-buffer-no-confirmation)
(global-set-key (kbd "<f6>") 'ack-and-a-half)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f11>") 'my/cleanup-buffer)

(global-set-key (kbd "M-<return>") 'my/open-line-below)
(global-set-key (kbd "M-S-<return>") 'my/open-line-above)
(global-set-key (kbd "C-<delete>") 'kill-word)

(global-set-key (kbd "C-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-other-window 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-other-window -1)))

(global-set-key (kbd "C-!") 'kill-this-buffer)
(global-set-key (kbd "C-M-!") 'my/kill-buffer-other-window)
(global-set-key (kbd "C-#") 'calculator)

(global-set-key (kbd "C-x C-r") 'my/ido-recentf-open)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)
(global-set-key (kbd "C-c n") 'my/cleanup-buffer)
(global-set-key (kbd "C-c d") 'my/diff-current-buffer-with-file)
(global-set-key (kbd "C-c r") 'my/rotate-windows)
(global-set-key (kbd "C-c C-;") 'my/toggle-comment-line-or-region)
(global-set-key (kbd "M-s l") 'sort-lines)
(global-set-key (kbd "M-s O") 'occur)
(global-set-key (kbd "M-s e") 'my/sudo-edit)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'my/zap-to-char-exclusive)
(global-set-key (kbd "M-Z") 'zap-to-char)

(define-key isearch-mode-map (kbd "C-*") 'my/isearch-current-region-or-word)

(global-set-key [remap goto-line] 'my/goto-line-with-feedback)

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
                            (add-hook 'write-contents-functions 'delete-trailing-whitespace)))

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

(add-to-list 'load-path "~/.emacs.d/packages/tomorrow-theme/GNU Emacs")

;; ace-jump-mode
(global-set-key (kbd "C-`") 'ace-jump-char-mode)
(global-set-key (kbd "C-~") 'ace-jump-word-mode)

(loop for c from ?0 to ?9 do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c from ?A to ?Z do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c from ?a to ?z do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c in '(?\( ?\) ?{ ?} ?[ ?] ?< ?>
                     ?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?- ?_ ?= ?+
                     ?\\ ?| ?\; ?: ?\" ?' ?, ?. ?/ ??)
      do (my/add-super-char-to-ace-jump-mode 'char c))

;; drag-stuff
(setq drag-stuff-modifier '(meta shift))

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; helm
(setq helm-input-idle-delay 0)
(helm-mode t)
(global-set-key (kbd "C-x a") 'helm-c-apropos)
(global-set-key (kbd "C-x f") 'helm-mini)
(global-set-key (kbd "C-x g") 'helm-google-suggest)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-X")   'helm-M-x)
(global-set-key (kbd "M-i")   'helm-semantic-or-imenu)
(global-set-key (kbd "M-s o") 'helm-occur)

;; iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; multiple-cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; smex
(global-set-key (kbd "M-x") 'smex)

;; undo-tree
(global-undo-tree-mode t)

;; wrap-region
(wrap-region-global-mode t)

;; YASnippet
(setq yas-prompt-functions '(yas-completing-prompt)) ; use normal completion, which is helm in our case
(yas-global-mode 1)

;; zencoding
(add-hook 'sgml-mode-hook 'zencoding-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

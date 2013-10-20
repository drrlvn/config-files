(eval-when-compile
  (require 'cl))

(require 'misc)

(add-to-list 'load-path "~/.emacs.d/packages/tomorrow-theme/GNU Emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "~/.emacs.d/defuns.el")

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
(setq-default cursor-type 'bar)

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

(package-initialize)
(my/install-packages
 'ace-jump-mode
 'ag
 'auto-complete
 'drag-stuff
 'expand-region
 'flx-ido
 'flycheck
 'git-gutter
 'git-messenger
 'guide-key
 'helm
 'helm-projectile
 'highlight-symbol
 'ido-vertical-mode
 'iedit
 'jedi
 'magit
 'markdown-mode
 'multiple-cursors
 'paredit
 'projectile
 'rainbow-delimiters
 'smex
 'undo-tree
 'whitespace-cleanup-mode
 'wrap-region
 'yaml-mode
 'yasnippet
 'zencoding-mode
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mappings
;;
(global-set-key (kbd "C-x r q") 'save-buffers-kill-emacs)
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "<home>") 'my/smart-beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'my/revert-buffer-no-confirmation)
(global-set-key (kbd "<f6>") 'ag-project-at-point)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "<f9>") 'magit-status)
(global-set-key (kbd "<f11>") 'my/cleanup-buffer)
(global-set-key (kbd "S-<f11>") 'whitespace-cleanup)
(global-set-key (kbd "S-<f12>") (lambda () (interactive) (find-file user-init-file)))

(global-set-key (kbd "M-<return>") 'my/open-line-below)
(global-set-key (kbd "M-S-<return>") 'my/open-line-above)
(global-set-key (kbd "C-<delete>") 'kill-word)

(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)

(global-set-key (kbd "C-n") (lambda (n) (interactive "p") (scroll-up n)))
(global-set-key (kbd "C-p") (lambda (n) (interactive "p") (scroll-down n)))
(global-set-key (kbd "M-n") (lambda (n) (interactive "p") (scroll-other-window n)))
(global-set-key (kbd "M-p") (lambda (n) (interactive "p") (scroll-other-window (- n))))

(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-!") 'kill-this-buffer)
(global-set-key (kbd "C-M-!") 'my/kill-buffer-other-window)
(global-set-key (kbd "C-#") 'calculator)
(global-set-key (kbd "C-'") 'highlight-symbol-at-point)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-.") 'highlight-symbol-next)
(global-set-key (kbd "M-s o") 'highlight-symbol-occur)
(global-set-key (kbd "M-{") 'git-gutter:previous-hunk)
(global-set-key (kbd "M-}") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c C-s") 'git-gutter:stage-hunk)

(global-set-key (kbd "C-x C-r") 'my/ido-recentf-open)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)
(global-set-key (kbd "C-c n") 'my/cleanup-buffer)
(global-set-key (kbd "C-c d") 'my/diff-current-buffer-with-file)
(global-set-key (kbd "C-c r") 'my/rotate-windows)
(global-set-key (kbd "C-c C-;") 'my/toggle-comment-line-or-region)
(global-set-key (kbd "M-s s") 'sort-lines)
(global-set-key (kbd "M-s M-s") 'sort-lines)
(global-set-key (kbd "M-s O") 'occur)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

(global-set-key (kbd "C-+") 'my/increment-number-at-point)
(global-set-key (kbd "C-M-+") 'my/decrement-number-at-point)
(global-set-key (kbd "C-#") 'copy-from-above-command)

(define-key isearch-mode-map (kbd "C-*") 'my/isearch-current-region-or-word)

(global-set-key [remap goto-line] 'my/goto-line-with-feedback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;

(add-hook 'after-init-hook #'global-flycheck-mode)

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
                            (subword-mode t)
                            (drag-stuff-mode t)
                            (rainbow-delimiters-mode t)
                            (setq show-trailing-whitespace t)
                            (font-lock-add-keywords
                             nil
                             '(("\\<\\(FIXME\\|TODO\\|XXX\\|BUG\\)\\>" 1 font-lock-warning-face t)))
                            (local-set-key (kbd "<return>") 'newline-and-indent)
                            (local-set-key (kbd "C-<delete>") 'subword-kill)
                            (local-set-key (kbd "C-<right>") 'subword-forward)
                            (local-set-key (kbd "C-<left>") 'subword-backward)))

;; C/C++
(add-hook 'c-mode-common-hook (lambda ()
                                (local-set-key (kbd "C-c o") 'ff-find-other-file)
                                (setq comment-start "/*"
                                      comment-end "*/")))

(add-to-list 'auto-mode-alist '("\\.x\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("SCons\\(truct\\|cript\\)\\'" . python-mode))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode t)
                                  (paredit-mode t)
                                  (local-set-key (kbd "C-c C-e") 'my/eval-and-replace)
                                  (add-hook 'after-save-hook (lambda () (byte-compile-file buffer-file-name))
                                            nil t)))

(add-hook 'python-mode-hook (lambda ()
                              (jedi:setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external packages
;;

;; ace-jump-mode
(global-set-key (kbd "C-`") 'ace-jump-char-mode)
(global-set-key (kbd "C-~") 'ace-jump-word-mode)

(loop for c from ?0 to ?9 do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c from ?A to ?Z do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c from ?a to ?z do (my/add-super-char-to-ace-jump-mode 'word c))
(loop for c in '(?\( ?\) ?{ ?} ?[ ?] ?< ?>
                     ?` ?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?- ?_ ?= ?+
                     ?\\ ?| ?\; ?: ?\" ?' ?, ?. ?/ ??)
      do (my/add-super-char-to-ace-jump-mode 'char c))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; drag-stuff
(setq drag-stuff-modifier '(meta shift))

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; git-gutter
(global-git-gutter-mode t)

;; git-messenger
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;; guide-key
(guide-key-mode t)

;; helm
(setq helm-input-idle-delay 0)
(helm-mode t)
(global-set-key (kbd "C-x a") 'helm-c-apropos)
(global-set-key (kbd "C-x f") 'helm-mini)
(global-set-key (kbd "C-x g") 'helm-google-suggest)
(global-set-key (kbd "C-x y") 'helm-show-kill-ring)
(global-set-key (kbd "M-X") 'helm-M-x)
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-s M-o") 'helm-occur)
(global-set-key (kbd "M-s m") 'helm-multi-occur)

;; ido
(ido-vertical-mode t)
(flx-ido-mode t)

;; iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; multiple-cursors
(global-set-key (kbd "C-|") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this-dwim)

;; paredit
;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; projectile
(projectile-global-mode t)
(global-set-key (kbd "C-c f") 'helm-projectile)

;; smex
(global-set-key (kbd "M-x") 'smex)

;; undo-tree
(global-undo-tree-mode t)

;; whitespace-cleanup-mode
(global-whitespace-cleanup-mode t)

;; wrap-region
(wrap-region-global-mode t)

;; YASnippet
(setq yas-prompt-functions '(yas-completing-prompt)) ; use normal completion, which is helm in our case
(setq yas-verbosity 1)
(yas-global-mode 1)

;; zencoding
(add-hook 'sgml-mode-hook 'zencoding-mode)

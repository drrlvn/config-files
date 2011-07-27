(add-to-list 'load-path "~/.emacs.d/packages/")

; set font
(if (window-system)
  (progn
    (set-face-attribute 'default nil :font "Consolas 10")
    (custom-set-faces '(minimap-font-face ((default (:height 30)) (nil nil))))
    )
  )

; color-theme
(add-to-list 'load-path "~/.emacs.d/packages/color-theme")
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

(setq x-select-enable-clipboard t)
(setq dabbrev-case-replace nil)
(setq display-time-24hr-format t)
(setq display-time-mode t)
(setq ring-bell-function 'ignore)
(setq history-length 500)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222")
; enable IDO mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
; cedet
(global-ede-mode 1)
(semantic-mode 1)
(setq semantic-default-submodes
      '(global-semantic-idle-scheduler-mode
         global-semanticdb-minor-mode
         global-semantic-idle-summary-mode
         global-semantic-idle-completions-mode
         global-semantic-highlight-func-mode
         global-semantic-decoration-mode
         global-semantic-stickyfunc-mode))
(setq semantic-complete-inline-analyzer-idle-displayor-class 'semantic-displayor-ghost)
; enable windmove
(windmove-default-keybindings 'meta)
; misc configuration
(setq x-select-enable-clipboard t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq backup `((".*" . ,temporary-file-directory)))
(setq autoload `((".*" ,temporary-file-directory t)))
(setq max-mini-window-height 1)
(setq lazy-highlight-initial-delay 0)
(show-paren-mode 1)
(setq show-paren-delay 0)
; enable disabled features
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
; show line numbers
(global-linum-mode 1)

; mappings
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key [escape] 'keyboard-quit)
(global-set-key [(control x) (k)] 'kill-this-buffer)

; C/C++
(add-hook 'c-mode-common-hook (lambda () (semantic-mode 1)))
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))
(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(add-hook 'c-mode-common-hook (lambda () (add-hook 'local-write-file-hooks
                                                   '(lambda()
                                                      (save-excursion
                                                        (delete-trailing-whitespace))))))
(setq c-default-style "linux"
      c-basic-offset 4)

; Python
(add-hook 'python-mode-hook (lambda () (semantic-mode 1)))

; disable splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
; hide toolbar
(tool-bar-mode 0)

; ido style symbol jumping
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "M-i") 'ido-goto-symbol)

(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

; recentf and ido-recentf
(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(require 'tramp)

(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

; anything
(add-to-list 'load-path "~/.emacs.d/packages/anything-config")
(require 'anything-config)
(global-set-key [(control x) (a)] 'anything)

; drag-stuff
(add-to-list 'load-path "~/.emacs.d/packages/drag-stuff")
(require 'drag-stuff)
(setq drag-stuff-modifier '(meta shift))
(drag-stuff-global-mode t)

; saveplace
(require 'saveplace)
(setq-default save-place t)

; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
               ("Dired" (mode . dired-mode))
               ("C/C++" (or
                          (mode . c-mode)
                          (mode . c++-mode)))
               ("Python" (mode . python-mode))
               ("emacs" (name . "^\\*"))
               )))
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

; ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/packages/ace-jump-mode")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; minimap
(add-to-list 'load-path "~/.emacs.d/packages/minimap")
(require 'minimap)
(setq minimap-update-delay 0.1)
(setq minimap-width-fraction 0.1)

; YASnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")
(setq yas/indent-line 'fixed)

; auto-complete
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
(ac-config-default)
(setq ac-dwim t)
(setq ac-auto-show-menu 0)

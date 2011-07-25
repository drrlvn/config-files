; set font
(set-face-attribute 'default nil :font "Consolas 10")

; enable IDO mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
; C-n at EOF inserts newlines
(setq x-select-enable-clipboard t)
(setq next-line-add-newlines t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq max-mini-window-height 1)
; indent newlines
(define-key global-map (kbd "RET") 'newline-and-indent)
; enable goal column (C-x C-n)
(put 'set-goal-column 'disabled nil)
; show line numbers
(global-linum-mode 1)

; C/C++
(add-hook 'c-mode-common-hook (lambda () (subword-mode 1)))
(add-hook 'c-mode-common-hook (lambda () (c-toggle-auto-state 1)))
(setq c-default-style "linux"
      c-basic-offset 4)

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

; YASnippet
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")
(setq yas/indent-line 'fixed)

; color-theme
(add-to-list 'load-path "~/.emacs.d/packages/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
                 '(progn
                    (color-theme-initialize)
                    (color-theme-dark-laptop)))

; auto-complete
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/ac-dict")
(ac-config-default)

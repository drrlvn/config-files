;;; config-packages.el --- external packages configuration
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))

(package-initialize)
(my/install-packages
 'ace-jump-mode
 'ag
 'auto-complete
 'diff-hl
 'drag-stuff
 'expand-region
 'flx-ido
 'flycheck
 'git-messenger
 'guide-key
 'helm
 'helm-projectile
 'helm-swoop
 'highlight-symbol
 'ido-vertical-mode
 'iedit
 'jedi
 'magit
 'markdown-mode
 'multiple-cursors
 'paredit
 'projectile
 'protobuf-mode
 'rainbow-delimiters
 'smex
 'undo-tree
 'whitespace-cleanup-mode
 'wrap-region
 'yaml-mode
 'yasnippet
 'zencoding-mode
 )

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

;; diff-hl
(global-diff-hl-mode t)

;; drag-stuff
(setq drag-stuff-modifier '(meta shift))

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; git-messenger
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
(setq git-messenger:show-detail t)

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

;; helm-swoop
(global-set-key (kbd "C-s") (lambda () (interactive) (helm-swoop :$query "")))
(global-set-key (kbd "C-S-s") 'helm-swoop)

;; ido
(ido-vertical-mode t)
(flx-ido-mode t)

;; iedit
(global-set-key (kbd "C-;") 'iedit-mode)

;; jedi
(require 'jedi)
(setq jedi:server-command (list "python2" jedi:server-script))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
                                (auto-fill-mode t)
                                (refill-mode t)))
(setq markdown-command "markdown_py")

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

(provide 'config-packages)
;;; config-packages.el ends here

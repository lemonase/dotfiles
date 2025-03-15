;;; -*- lexical-binding: t -*-
;;; package --- jams experimental emacs config
;;; commentary --- not much

;;; Code:
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq inhibit-splash-screen t)   ; Do not show splash screen
(setq visible-bell t)            ; Flash when the bell rings
(setq frame-resize-pixelwise t)  ; Yes, I would like to be able to **resize** emacs, thanks!

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

(set-frame-font "Recursive 12" nil t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(global-display-line-numbers-mode 1)
(load-theme 'modus-vivendi t)

;; Auto-refresh buffers when files on disk change.
(global-auto-revert-mode t)
(delete-selection-mode 1)

;; Place backups in a separate folder.
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/" t)))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

(transient-mark-mode 1)

(set-register ?e (find-file (or user-init-file "")))
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package magit
  :ensure t)

(use-package multiple-cursors
  :ensure t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package emmet-mode
  :ensure t
  :init)

(use-package which-key
  :ensure t
  :defer t
  :init (which-key-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  ;; Download company if not found
  :ensure t
  :init
  ;; Turn on company after emacs starts up
  (global-company-mode))
  
(use-package lsp-mode
  :ensure t
  :hook (( ;; and any other mode you want to hook into lsp
          prog-mode) . lsp-deferred))

(use-package rainbow-delimiters
    :ensure t
    :hook (prog-mode . rainbow-delimiters-mode))

(setq mode-line-format(list '(:eval (list (nyan-create)))))

(nyan-mode)

;;; init.el ends here

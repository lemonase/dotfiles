;;; init.el --- Jam Emacs config -*- lexical-binding: t -*-
;;;
;;; Author: James Dixon <notjamesdixon@gmail.com>
;;; Maintainer: James Dixon <notjamesdixon@gmail.com>
;;;
;;; Commentary:
;;; Emacs from scratch (somewhat), this is my ~~story~~ config
;;; ((((((((((((((((((((!!!GNU EMACS!!!))))))))))))))))))))
;;;

;;; Code:

;;; User Info
(setq user-full-name "James Dixon")
(setq user-mail-address "notjamesdixon@gmail.com")

;;; * Start "Sensible Defaults" *

;; Do not do these things
(setq inhibit-startup-screen t)        ; Don't show the splash screen

;; Lines and columns
(global-display-line-numbers-mode 1)	; Display line numbers
(column-number-mode 1)                  ; Toggle column number display in the mode line.

;; Bells and whistles
(setq visible-bell 1)                   ; Flash when the bell rings (no sound)
(setq frame-resize-pixelwise t)         ; Yes, I would like to be able to **resize** emacs frame, thanks!
(setq window-resize-pixelwise nil)      ; Not for windows inside emacs though

;; UI cleanup
(tool-bar-mode -1)                      ; Disable tool bar
(scroll-bar-mode -1)                    ; Disable scroll bar

;; Mark and go-to modes
(transient-mark-mode 1)                 ; Easier starting of marks/regions
(delete-selection-mode 1)               ; Easier deleting of marks/regions
(global-goto-address-mode 1)            ; Make links and addresses go-to able

;; Allow for shorter responses: "y" for yes and "n" for no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Show paren differently
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Memory Limits and Performance

;; Undo/Redo
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;; Garbage Collection
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Perf: Reduce command completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;;; Theme (Default)
(set-frame-font "Maple Mono 12" nil t)
(load-theme 'modus-vivendi t)

;;; File History, Saving and Reverting

;; Auto-refresh buffers when files on disk change.
(global-auto-revert-mode t)
;; Place backups in a separate folder.
(setq backup-directory-alist `(("." . "~/.config/emacs/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/saves/" t)))

;; No duplicates in kill ring
(setq kill-do-not-save-duplicates t)

;; Easy edit init file
(set-register ?i (cons 'file user-init-file))

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(global-set-key (kbd "C-x C-r") 'recentf)

;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions.
(setq history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
        register-alist                   ; macros
        mark-ring global-mark-ring       ; marks
        search-ring regexp-search-ring)) ; searches

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)
(setq auto-save-visited-interval 10)
(auto-save-visited-mode 1)

;; Auto-revert in Emacs is a feature that automatically updates buffer to reflect
;; changes on disk
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf  maintains a list of recently accessed files
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))

;; savehist is an Emacs feature that preserves the minibuffer history between sessions
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
(add-hook 'after-init-hook #'save-place-mode)
;; End File History, Saving and Reverting

;;; Comment Settings
(setq comment-multi-line t)
(setq comment-empty-lines t)
(setq-default fill-column 80)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;;; Custom Tab Settings
; START TABS CONFIG from https://dougie.io/emacs/indentation/
;; Create a variable for our preferred tab width
(setq custom-tab-width 4)
(setq default-tab-width 4)
(setq standard-indent 4)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () (setq indent-tabs-mode nil))
(defun enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Enable Tabs
;; (add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)
(add-hook 'sh-mode-hook 'disable-tabs)

;; Language-Specific Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript

(setq-default sh-indent-level custom-tab-width)      ;; Shell
(setq-default sh-basic-offset custom-tab-width)      ;; Shell

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)
(global-whitespace-mode -1) ; Enable whitespace mode everywhere
; END TABS CONFIG

;;; Custom Vanilla Config
;; https://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell/7053298#7053298
(defun shell-region (start end)
  "Execute region START to END in an inferior shell."
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

;; Ediff
;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; Backwards kill with C-w
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))


;;; * "Sensible Defaults" ends here *
;;; ** Start Package Manager (straight.el) **
;; bootstrap straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Set Emacs path == shell path (exec-path-from-shell)
;; add paths from shell by default
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; EVIL Config (evil-mode)
(use-package evil
  :straight t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package undo-fu
  :straight t)

(use-package evil-commentary
  :straight t
  :after evil
  :init
  (evil-commentary-mode))

(use-package evil-collection
  :straight t
  :after evil
  :defer t
  :init
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Custom Evil Keybinds

;; More ergonomic M-x and C-x
(define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)
(define-key evil-normal-state-map (kbd "SPC x") ctl-x-map)
(define-key evil-normal-state-map (kbd "SPC w") 'save-buffer)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC f") 'find-file)
(define-key evil-normal-state-map (kbd "SPC d") 'dired)
(define-key evil-normal-state-map (kbd "SPC b") 'bookmark-jump)
(define-key evil-normal-state-map (kbd "SPC o") 'occur)
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC r") 'recentf)
;; end evil

;;; Writing Org / Markdown / HTML (org-mode, markdown-mode, emmet)
(use-package org
  :straight nil)

;; TODO: setup org more
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
'(org-export-backends '(ascii html icalendar latex man md odt org))

;; better markdown
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t) ; Make code block syntax highlighted
  :bind(:map markdown-mode-map
             ("C-c C-e" . markdown-do)))

;; make writing HTML tags way easier
(use-package emmet-mode
  :straight t
  :init)

;;; Themes and Colors (doom-themes, rainbow-mode, rainbow-delimiters)
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-ir-black t))

(use-package rainbow-mode
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;;; Vanilla+ Plugins (dired, magit)
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package magit
  :straight t)

;;; Mini-buffer improvements (vertico, orderless, marginalia, embark, consult)
(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  :straight t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :straight t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;;; Better discoverability for key mappings (which-key)
(use-package which-key
  :straight t
  :defer t
  :init (which-key-mode))

;;; Matching brackets with (electric-pair-mode) and (smartparens)
(use-package smartparens
  :straight smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))
(electric-pair-mode 1)

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;;; Completions in buffer (corfu and cape)
;; Corfu Completion Framework
(use-package corfu
  :straight t
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

;; Cape Competion
(use-package cape
  :straight t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; Treesitter (treesit)
(use-package treesit
  :commands (treesit-install-language-grammar)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))


;; Tree Sitter auto config
(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)
(setq treesit-auto-langs '(python rust go gomod))

;;; LSP Configurations (lsp-mode)
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-mode
  :hook ((go-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

;;; ** Package Manager (straight.el) ends here **
;;; Additional Language Modes
;; JavaScript
(use-package js
  :defer t
  :custom
  (js-indent-level 2))

;; CSS
(use-package css
  :defer t
  :custom
  (css-indent-level 2))

;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;; Other misc modes (docker, gptel, load-env-vars, csv-mode)
(use-package docker
  :straight t
  :defer t)

(use-package dockerfile-mode
  :straight t
  :defer t)

(use-package csv-mode
  :straight t
  :defer t)

(use-package load-env-vars
  :straight t)

(use-package gptel
  :straight t)

(load-env-vars "~/.local/.env")

;; (setq gemini-api-key (funcall (lambda (prompt) (read-passwd prompt)) "Enter Gemini API key: "))
;; (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
;; (gptel-make-openai "OpenAI" :key (getenv "OPENAI_KEY") :stream t)
(gptel-make-gemini "Gemini" :stream t :key gptel-api-key)
(gptel-make-openai "OpenAI" :stream t :key gptel-api-key)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company corfu csv-mode embark-consult emmet-mode evil-collection
	     evil-surround exec-path-from-shell fireplace flycheck
	     go-mode json-mode lsp-mode magit marginalia
	     multiple-cursors nyan-mode rainbow-delimiters rust-mode
	     smartparens typescript-mode vertico yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

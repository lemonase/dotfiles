;;; init.el --- James' Emacs config -*- lexical-binding: t -*-
;;;
;;; Author: James Dixon <notjamesdixon@gmail.com>
;;; Maintainer: James Dixon <notjamesdixon@gmail.com>
;;;
;;; Commentary:
;;; Emacs from scratch (somewhat), this is my ~~story~~ config
;;; ((((((((((((((((((((!!!GNU EMACS!!!))))))))))))))))))))
;;;

;;; Code:
(setq inhibit-startup-message t)        ; Don't show the splash screen
(setq inhibit-splash-screen t)          ; Do not show splash screen
(setq visible-bell nil)                 ; Flash when the bell rings
(setq frame-resize-pixelwise t)         ; Yes, I would like to be able to **resize** emacs frame, thanks!
(setq window-resize-pixelwise nil); Not for windows inside emacs though

(global-display-line-numbers-mode 1)	; Display line numbers
(column-number-mode -1)                 ; Toggle column number display in the mode line.
(global-goto-address-mode 1)            ; Make links and addresses go-to able

(tool-bar-mode -1)                      ; Disable tool bar
(scroll-bar-mode -1)                    ; Disable scroll bar
(transient-mark-mode 1)                 ; Easier starting of marks/regions
(delete-selection-mode 1)               ; Easier deleting of marks/regions

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Show paren differently
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Undo/redo
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;; Theme
(set-frame-font "Maple Mono 12" nil t)
(load-theme 'modus-vivendi t)

;; More memory for Garbage Collection
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; *** History and Saving ***

;; Auto-refresh buffers when files on disk change.
(global-auto-revert-mode t)
;; Place backups in a separate folder.
(setq backup-directory-alist `(("." . "~/.config/emacs/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/saves/" t)))

;; No duplicates in kill ring
(setq kill-do-not-save-duplicates t)

;; Easy edit init file
(set-register ?i (cons 'file user-init-file))
;; (define-key global-map (kbd "C-c e") (lambda()(interactive)(find-file user-init-file)))

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(global-set-key (kbd "C-c f") 'recentf)

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

;; Comments
(setq comment-multi-line t)
(setq comment-empty-lines t)
(setq-default fill-column 80)

;; 4 space tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq-default tab-always-indent nil)
(setq python-indent-guess-indent-offset-verbose nil)
(setq sh-indent-after-continuation 'always)

;; Perf: Reduce command completion overhead.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Ediff
;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

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

;; Eglot
(setq eglot-sync-connect 1
      eglot-autoshutdown t)
;; Activate Eglot in cross-referenced non-project files
(setq eglot-extend-to-xref t)

;; Custom Vanilla Stuff
;; https://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell/7053298#7053298
(defun shell-region (start end)
  "Execute region START to END in an inferior shell."
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

;; Quick switching windows
(define-key global-map (kbd "M-p") 'previous-multiframe-window)
(define-key global-map (kbd "M-n") 'other-window)

;; Package Manager
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

;; add paths from shell by default
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; start evil
(unless (package-installed-p 'evil)
  (package-install 'evil))
(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode -1)

(use-package evil-collection
  :straight t
  :after evil
  :defer t
  :init
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :defer t
  :config
  (global-evil-surround-mode 1))
;; end evil

;; writing
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
'(org-export-backends '(ascii html icalendar latex man md odt org))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

;; emmet - html abberviations
(use-package emmet-mode
  :straight t
  :init)

;; end writing

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package magit
  :straight t)

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
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
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

(use-package multiple-cursors
  :straight t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(use-package which-key
  :straight t
  :defer t
  :init (which-key-mode))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; Corfu Completion Framework
(use-package corfu
  :straight t
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  (corfu-cycle t)
  (corfu-auto t)
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

;; Tree Sitter auto config
(use-package treesit-auto
  :straight t
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)
(setq treesit-auto-langs '(python rust go))

;; vterm terminal emulator
(use-package vterm
  :commands vterm
  :straight t)

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))


(setq confirm-kill-emacs #'yes-or-no-p)
(defalias 'yes-or-no #'y-or-n-p)

;;; init.el ends here

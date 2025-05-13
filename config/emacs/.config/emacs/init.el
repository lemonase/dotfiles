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
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ; Column width indicator

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

;; Encoding UTF-8
(set-language-environment "UTF-8")

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
(cond
 ((find-font (font-spec :name "Maple Mono"))
      (set-frame-font "Maple Mono 12" nil t)))

(load-theme 'modus-vivendi t)

;;; File History, Saving and Reverting

;; Auto-refresh buffers when files on disk change.
(global-auto-revert-mode t)
;; Place backups in a separate folder.
(setq backup-directory-alist `(("." . "~/.config/emacs/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/saves/" t)))

;; No duplicates in kill ring
(setq kill-do-not-save-duplicates t)

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))

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

(define-minor-mode clean-trailing-whitespace-mode
  "Tidy up trailing whitespace with `delete-trailing-whitespace' before saving."
  :lighter " ctsv"
  (if clean-trailing-whitespace-mode
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t)))

(define-minor-mode clean-all-whitespace-mode
  "Tidy up *all* whitespace with `whitespace-cleanup' before saving."
  :lighter " casv"
  (if clean-trailing-whitespace-mode
      (add-hook 'before-save-hook #'whitespace-cleanup nil t)
    (remove-hook 'before-save-hook #'whitespace-cleanup t)))

(define-minor-mode check-parens-save-mode
  "Check the balance of parens with `check-parens' before saving."
  :lighter " cpns"
  (if check-parens-save-mode
      (add-hook 'before-save-mode #'check-parens nil t)
    (remove-hook 'before-save-mode #'check-parens t)))

(add-hook 'prog-mode #'clean-all-whitespace-mode)
(add-hook 'emacs-lisp-mode #'check-parens-save-mode)
;; End File History, Saving and Reverting

;;; Comment Settings
(setq comment-multi-line t)
(setq comment-empty-lines t)
(setq-default fill-column 80)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;;; Custom Tab Settings
; START TABS CONFIG from https://dougie.io/emacs/indentation/
;; Create a variable for our preferred tab width
(setq custom-tab-width 2)
(setq default-tab-width 4)
(setq standard-indent 4)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs () "Disable tabs for indenting." (setq indent-tabs-mode nil))
(defun enable-tabs  () "Enable tabs for indenting."
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

;; TODO: make it work with defaults of Linux/macOS
(defun ext-terminal-in-workdir ()
  "Open an external terminal emulator in working directory."
  (interactive)
  (call-process-shell-command (concat "wt -d " default-directory) nil 0))

;; TODO: make it work with defaults of Linux/macOS
(defun ext-file-browser-in-workdir ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (shell-command (concat "start " (expand-file-name default-directory)))
    (error "No `default-directory' to open")))

(defun insert-current-iso-date ()
  "Insert the current ISO 8601 date."
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-current-iso-date-seconds()
  "Insert the current ISO 8601 date (with time res of seconds)."
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

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
  (setq evil-want-C-i-jump nil)
  (setq evil-symbol-word-search t)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-ex-visual-char-range t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-insert-state-cursor '(box "violet")
      evil-normal-state-cursor '(box "yellow")
      evil-visual-state-cursor '(box "#1aa5db"))
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
    :config
  (evil-mode 1))

(use-package undo-fu
  :straight t)

(use-package evil-collection
  :straight t
  :after evil
  :defer t
  :init
  (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :init
  (evil-commentary-mode))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :straight t
  :after evil)

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-markdown
  :straight `(el-patch :type git :host github :repo "Somelauw/evil-markdown")
  :after evil)
;; Custom Evil Keybinds

;;; Evil customizations

;;; Basic way to do pulse for evil yank text (like goggles.el package)
;;; https://blog.meain.io/2020/emacs-highlight-yanked/
(defun hl-yank-advice (yank-fn beg end &rest args)
  "Give advice to YANK-FN BEG END ARGS for temp highlighting of region."
  (pulse-momentary-highlight-region beg end)
  (apply yank-fn beg end args))
(advice-add 'evil-yank :around 'hl-yank-advice)

;;; evil-numbers keybinds
(evil-define-key '(normal visual) 'global (kbd "C-a +") 'evil-numbers/inc-at-pt)
(evil-define-key '(normal visual) 'global (kbd "C-a -") 'evil-numbers/dec-at-pt)
(evil-define-key '(normal visual) 'global (kbd "C-a C-+") 'evil-numbers/inc-at-pt-incremental)
(evil-define-key '(normal visual) 'global (kbd "C-a C--") 'evil-numbers/dec-at-pt-incremental)


;;; general keybinds
(evil-set-leader nil (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader> :") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "<leader> e") 'eval-last-sexp)
(evil-define-key 'visual 'global (kbd "<leader> e") 'eval-region)
(evil-define-key 'normal 'global (kbd "<leader> p") 'eval-print-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader> E") 'eval-expression)
(evil-define-key 'normal 'global (kbd "<leader> b") 'eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader> w") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader> W") 'whitespace-mode)
(evil-define-key 'normal 'global (kbd "<leader> k") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "<leader> f") 'ffap)
(evil-define-key 'normal 'global (kbd "<leader> F") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader> d") 'dired)
(evil-define-key 'normal 'global (kbd "<leader> K") 'dired-jump)
(evil-define-key 'normal 'global (kbd "<leader> o") 'occur)
(evil-define-key 'normal 'global (kbd "<leader> B") 'bookmark-jump)
(evil-define-key 'normal 'global (kbd "<leader> g") 'magit-status)
(evil-define-key 'normal 'global (kbd "<leader> r") 'replace-regexp)
(evil-define-key 'normal 'global (kbd "<leader> R") 'recentf)
(evil-define-key 'normal 'global (kbd "<leader> x") ctl-x-map)
(evil-define-key 'normal 'global (kbd "<leader> O") 'ext-file-browser-in-workdir)
(evil-define-key 'normal 'global (kbd "<leader> T") 'ext-terminal-in-workdir)
(evil-define-key 'normal 'global (kbd "<leader> A") 'abbrev-mode)
(evil-define-key 'normal 'global (kbd "C-c i") (lambda () (interactive) (find-file user-init-file)))
;; end evil

;; Easy find init file
(set-register ?i (cons 'file user-init-file))

;;; Writing Org / Markdown / HTML (org-mode, markdown-mode)
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

;;; Themes and Colors (doom-themes, hl-todo, rainbow-mode, rainbow-delimiters)
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-ir-black t))

; TODO: look into todo integrations
; https://github.com/tarsius/hl-todo?tab=readme-ov-file#integrations
(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package rainbow-mode
  :straight t)

(use-package rainbow-delimiters
  :straight t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :straight t
  :config
  (setq git-gutter:update-interval 0.2))

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;;; Vanilla+ Plugins (dired, magit)
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))
(setq dired-dwim-target t)

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

;;; Better discoverability for key mappings (which-key)
(use-package which-key
  :straight t
  :defer t
  :init (which-key-mode))

;;; Better help menus (helpful)
(use-package helpful
  :straight t
  :bind
  (("C-c C-d" . helpful-at-point)    ; Lookup the symbol at point
   ("C-h f" . helpful-callable)      ; Describe a function
   ("C-h v" . helpful-variable)      ; Describe a variable
   ("C-h k" . helpful-key)           ; Describe a key binding
   ("C-h x" . helpful-command)))     ; Describe a command

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

;; Abbrevs and Snippets

;;; Abbrevs
(define-abbrev global-abbrev-table "bg" "background")
(define-abbrev global-abbrev-table "ty" "thank you")
(define-abbrev global-abbrev-table "yw" "you are welcome")
(define-abbrev global-abbrev-table "u" "you")
(define-abbrev global-abbrev-table "mygh" "https://github.com/lemonase")
(define-abbrev global-abbrev-table "dt" "" 'insert-current-iso-date)
(define-abbrev global-abbrev-table "dts" "" 'insert-current-iso-date-seconds)
(define-abbrev global-abbrev-table "td" "" 'insert-current-iso-date)
(define-abbrev global-abbrev-table "tds" "" 'insert-current-iso-date-seconds)

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

;; Language mode configurations
(use-package lsp-mode
  :hook ((go-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

;;; emmet: make writing HTML tags much easier
(use-package emmet-mode
  :straight t
  :init)

;;; Lua
(use-package lua-mode
  :straight t)

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
(defvar my-env-file "~/.local/.env" "Local environment file.")
(let ((my-env-file "~/.local/.env"))
  (if (file-exists-p my-env-file)
    (load-env-vars my-env-file)))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

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

;; LLM support (must configure with api keys)
(use-package gptel
  :straight t)

;; (setq gemini-api-key (funcall (lambda (prompt) (read-passwd prompt)) "Enter Gemini API key: "))
;; (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
;; (gptel-make-openai "OpenAI" :key (getenv "OPENAI_KEY") :stream t)
(gptel-make-gemini "Gemini" :stream t :key gptel-api-key)
(gptel-make-openai "OpenAI" :stream t :key gptel-api-key)

;;; Platform Specifics

;; for Win32
;; TODO: check if fonts exists before setting
(if (eq system-type 'windows-nt)
    (set-frame-font "Cascadia Code 12" nil t))

;; on windows, make pwsh the default shell
(when (eq system-type 'windows-nt)
  (let ((xlist
         '(
           "C:/Program Files/PowerShell/7/pwsh.exe"
           "~/AppData/Local/Microsoft/WindowsApps/pwsh.exe"
           "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
           ))
        xfound)
    (setq xfound (seq-some (lambda (x) (if (file-exists-p x) x nil)) xlist))
    (when xfound (setq explicit-shell-file-name xfound))))

;; Still handy having cygwin / mingw for minimal set of Linux CLI tools.
;; Alternative approach is installing Emacs inside WSL, which has pros and cons
;; as far as configuring PATH, SHELL, compiler and interop between Linux/Windows.
;; Overall it depends how much you are interacting with the native Windows NTFS
;; filesystem vs developing for things inside of Linux.
(when (eq system-type 'windows-nt)
  (setq exec-path (cons "C:/cygwin/bin" exec-path))
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))


;; Have to change emacs init dir for Windows
;; https://emacs.stackexchange.com/a/12886
;; (setenv "HOME" "C:/Users/itzja")
;; (setq default-directory "C:/Users/user")
;; (setq user-init-file "C:/Users/user/.emacs.d/init.el")
;; (setq user-emacs-directory "C:/Users/user/.emacs")
;; (load user-init-file)

;;; init.el ends here

;;; init.el -*- lexical-binding: t -*-
;;;
;;; Author: James Dixon <notjamesdixon@gmail.com>
;;; Maintainer: James Dixon <notjamesdixon@gmail.com>
;;;
;;; Commentary:
;;; My Emacs Config
;;;
;;; Code:
(setq user-full-name "James Dixon")
(setq user-mail-address "notjamesdixon@gmail.com")

;; Set default language encoding to UTF-8
(set-language-environment "UTF-8")

(setq use-short-answers t) ; Allow for shorter responses: "y" for yes and "n" for no.

;; Do not do these things
(setq inhibit-startup-screen t)        ; Don't show the splash screen
(setq kill-do-not-save-duplicates t)   ; No duplicates in kill ring

;; UI cleanup
(tool-bar-mode -1)                      ; Disable tool bar
(scroll-bar-mode -1)                    ; Disable scroll bar
(menu-bar-mode -1)                      ; Disable menu bar

;; Enable visual bells and disable whistles
(setq visible-bell 1)                   ; Flash when the bell rings (no sound)
(setq frame-resize-pixelwise t)         ; Yes, I would like to be able to **resize** emacs frame, thanks!
(setq window-resize-pixelwise nil)      ; Not for windows inside emacs though
(winner-mode 1)                         ; Record the changes in window configuration (undo/redo window changes)

;; Show lines and columns
(global-display-line-numbers-mode 1)    ; Display line numbers
(column-number-mode 1)                  ; Toggle column number display in the mode line.

;; Show relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Mark and go-to modes
(transient-mark-mode 1)                 ; Easier starting of marks/regions
(delete-selection-mode 1)               ; Easier deleting of marks/regions
(global-goto-address-mode 1)            ; Make links and addresses go-to able

;; Show paren differently
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Comment Settings
(setq comment-multi-line t)
(setq comment-empty-lines t)
(setq-default fill-column 80)

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
;; (load-theme 'modus-operandi t)

(display-time-mode 1) ;; Display time in modeline

;;; File History, Saving and Reverting

;; Place backups in a separate folder.
(setq backup-directory-alist `(("." . "~/.config/emacs/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/saves/" t)))

;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(add-hook 'after-init-hook #'(lambda() (let ((inhibit-message t)) (recentf-mode 1))))

;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions.
(setq history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default
(setq savehist-additional-variables
      '(kill-ring                        ; clipboard
	register-alist                   ; macros
	mark-ring global-mark-ring       ; marks
	search-ring regexp-search-ring)) ; searches
;; savehist is an Emacs feature that preserves the minibuffer history between sessions
(add-hook 'after-init-hook #'savehist-mode)
;; save-place-mode enables Emacs to remember the last location within a file
(add-hook 'after-init-hook #'save-place-mode)
;; Auto-revert in Emacs is a feature that automatically updates buffer to reflect changes on disk
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)
(setq auto-save-interval 300)
(setq auto-save-timeout 30)
(setq auto-save-visited-interval 10)
(auto-save-visited-mode 1)

;; Auto-refresh buffers when files on disk change.
(global-auto-revert-mode t)

;;; Custom Tab Settings
;; https://dougie.io/emacs/indentation/
;; Create a variable for our preferred tab width
(setq custom-tab-width 2)
(setq default-tab-width 4)
(setq standard-indent 4)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs ()
  "Disable tabs for indenting."
  (setq indent-tabs-mode nil))

(defun enable-tabs  ()
  "Enable tabs for indenting."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; For the vim-like motions of ">>" and "<<".
(setq-default evil-shift-width custom-tab-width)

;; Hooks to Enable Tabs
;; (add-hook 'prog-mode-hook 'enable-tabs)
;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)
(add-hook 'sh-mode-hook 'disable-tabs)
(add-hook 'python-mode-hook 'disable-tabs)

;; Language-Specific Tab Tweaks
(setq-default python-indent-offset custom-tab-width) ;; Python
(setq-default python-indent-level custom-tab-width)  ;; Python
(setq-default js-indent-level custom-tab-width)      ;; Javascript
(setq-default sh-indent-level custom-tab-width)      ;; Shell
(setq-default sh-basic-offset custom-tab-width)      ;; Shell

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

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

;;; Minor Mode Hooks
;; (add-hook 'prog-mode #'clean-all-whitespace-mode)
;; (add-hook 'org-mode #'clean-all-whitespace-mode)
;;
;; (add-hook 'emacs-lisp-mode #'check-parens-save-mode)
;; (add-hook 'emacs-lisp-mode #'outline-minor-mode)

;; https://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell/7053298#7053298
(defun shell-region (start end)
  "Execute region START to END in an inferior shell."
  (interactive "r")
  (shell-command  (buffer-substring-no-properties start end)))

;; Builtin `rgrep' asks too many (4) questions. Just run grep -r in cwd.
;; Inspired by: https://emacs.stackexchange.com/a/26349
(defun recursive-grep ()
  "Recursively grep file contents.  `i` case insensitive; `n` print line number;
`I` ignore binary files; `E` extended regular expressions; `r` recursive"
  (interactive)
  (let* ((grep-flags "-inrEI --color=always -C3")
	 (search-term (read-string (format "Recursive regex search with grep %s: " grep-flags)))
	 (search-path (directory-file-name (expand-file-name (read-directory-name "directory:  "))))
	 (default-directory (file-name-as-directory search-path))
	 (grep-command (concat grep-program " " grep-flags " " search-term " " search-path)))
    (compilation-start grep-command 'grep-mode (lambda (mode) "*grep*") nil)))

;; Open External Terminal Emulator
(defun ext-terminal-in-workdir ()
  "Open an external terminal emulator in working directory."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (call-process-shell-command (concat "wt -d " default-directory) nil 0))
   ((eq system-type 'darwin)
    (shell-command (concat "open -a iTerm " (shell-quote-argument (expand-file-name default-directory)))))
   ((eq system-type 'gnu/linux)
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))

;; TODO: Look at using the EAT package for terminal things
;; https://codeberg.org/akib/emacs-eat

;; Open External File Browser
(defun ext-file-browser-in-workdir ()
  "Open the current file's directory however the OS would."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command (concat "start " (expand-file-name default-directory))))
   ((eq system-type 'darwin)
    (shell-command (concat "open " (expand-file-name default-directory))))
   ((eq system-type 'gnu/linux)
    (shell-command (concat "xdg-open " (expand-file-name default-directory))))))

(defun insert-current-time ()
  "Insert the current time H:M:S." (insert (format-time-string "%H:%M:%S")))

(defun insert-current-iso-date ()
  "Insert the current ISO 8601 date." (insert (format-time-string "%Y-%m-%d")))

(defun insert-current-iso-date-time()
  "Insert the current ISO 8601 date (with time res of seconds)."
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

;;; Make backwards kill with C-w work
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;; TODO: disable this in terminal mode
;;; Basic way to do pulse for evil yank text (like goggles.el package)
;;; https://blog.meain.io/2020/emacs-highlight-yanked/
(defun hl-yank-advice (yank-fn beg end &rest args)
  "Give advice to YANK-FN BEG END ARGS for temp highlighting of region."
  (pulse-momentary-highlight-region beg end)
  (apply yank-fn beg end args))
(advice-add 'evil-yank :around 'hl-yank-advice)

;; https://github.com/radian-software/straight.el
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

(setq package-install-upgrade-built-in t)

;;; Set Emacs path == shell path (exec-path-from-shell)
;; add paths from shell by default
(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; EVIL Config :: Vi/Vim Emulation++ (evil-mode)
;;; Evil Package
;; https://github.com/emacs-evil/evil
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
	evil-visual-state-cursor '(hollow "#1aa5db")
	evil-emacs-state-cursor '(box "cyan"))
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode -1))

;;; Undo Nicities
;; https://github.com/emacsmirror/undo-fu
(use-package undo-fu
  :straight t)
;;; TODO: Look into https://codeberg.org/ideasman42/emacs-undo-fu-session

;; ;;; Make Evil work in more modes than by default
;; ;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :straight t
  :after evil
  :defer t
  :init
  (evil-collection-init))
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'magit-mode 'emacs)

;;; Bindings and functionality to comment out code and other text objects
;; https://github.com/linktohack/evil-commentary
(use-package evil-commentary
  :straight t
  :after evil
  :init
  (evil-commentary-mode))

;;; Bindings to surround text objects.
;; https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; Vim like increment and decrement of numbers
;; https://github.com/cofi/evil-numbers
(use-package evil-numbers
  :straight t
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-a +") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-a -") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-a C-+") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "C-a C--") 'evil-numbers/dec-at-pt-incremental))

;; Custom Evil Keybinds
;; Evil Guide: https://github.com/noctuid/evil-guide?tab=readme-ov-file#keybindings-and-states
;; General keybind definition helper
;; https://github.com/noctuid/general.el
(use-package general
  :straight t
  :config (general-evil-setup))

;; Global Normal Mode :: General Keymaps
(general-nmap
  :prefix "SPC"
  ;; Eval Keybinds
  ":" 'eval-expression
  ";" 'execute-extended-command
  "p" 'execute-extended-command
  "x" 'eval-defun
  "e" 'eval-last-sexp
  "E" 'eval-print-last-sexp
  "b" 'eval-buffer
  "." 'repeat-complex-command
  "q" 'evil-quit
  "Q" 'evil-quit-all
  ;; Buffer Management
  "w" 'save-buffer
  "l" 'ibuffer
  "q" 'evil-delete-buffer
  ;; Running external stuff
  "c" 'compile
  "r" 'recompile
  "!" 'shell-command
  "&" 'async-shell-command
  ;; Jumping places
  "f" 'ffap
  "-" 'dired-jump
  "B" 'bookmark-jump
  ;; External Apps
  "O" 'ext-file-browser-in-workdir
  "T" 'ext-terminal-in-workdir
  ;; Extra packages
  "s" 'yas-insert-snippet
  "F" 'format-all-region-or-buffer
  "D" 'dirvish-side
  "/" 'consult-line)

;; Extra meta prefixes
(general-nmap
  :prefix "SPC m"
  "x" 'execute-extended-command)

;; Global Normal Mode :: Toggle Keymaps
(general-nmap
  :prefix "SPC t"
  ;; Toggle Modes
  "a" 'abbrev-mode
  "w" 'whitespace-mode
  "t" 'indent-tabs-mode
  "c" 'display-fill-column-indicator-mode)

;; Global Normal Mode :: Magit Keymaps
(general-nmap
  :prefix "SPC g"
  ;; Magit Commands
  "s" 'magit-status
  "l" 'magit-log
  "d" 'magit-diff-dwim
  "b" 'magit-branch
  "i" 'magit-init
  "a" 'magit-stage-files
  "t" 'magit-stage-files
  "u" 'magit-unstage-files
  "c" 'magit-commit-create
  "p" 'magit-push-to-remote
  "f" 'magit-fetch-from-upstream
  "F" 'magit-pull-from-upstream)

;; Global Visual Mode Keymaps
(general-vmap
  :prefix "SPC"
  "e" 'eval-region)

;; Custom Ex commands
(evil-ex-define-cmd "Format" 'format-all-region-or-buffer) ;; format-all-code

;;; Vanilla Emacs Keybinds
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file (concat user-emacs-directory "/init.org"))))
(global-set-key (kbd "C-c d") (lambda () (interactive) (find-file (getenv "DOTFILES"))))

;; more ergo keybind for switching to normal<->emacs state
(global-set-key (kbd "C-;") (kbd "C-z"))

;;; Better discoverability for key mappings (which-key)
;; https://github.com/justbur/emacs-which-key
;; builtin to emacs > 30.1
(use-package which-key
  :straight t
  :init (which-key-mode 1))

;;; Command log (show list of commands in separate buffer)
;; https://github.com/lewang/command-log-mode
(use-package command-log-mode
  :straight t)

;;; Better help menus (helpful)
;; https://github.com/Wilfred/helpful
(use-package helpful
  :straight t
  :bind
  (("C-c C-d" . helpful-at-point)    ; Lookup the symbol at point
   ("C-h f" . helpful-callable)      ; Describe a function
   ("C-h v" . helpful-variable)      ; Describe a variable
   ("C-h k" . helpful-key)           ; Describe a key binding
   ("C-h x" . helpful-command)))     ; Describe a command

;; Org mode (organization outline framework)
(straight-use-package '(org :type built-in)) ;; use builtin org
;; https://orgmode.org/
(use-package org
  :straight nil
  :config
  '(org-export-backends '(ascii html icalendar latex man md odt org))
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c l") #'org-store-link)
  (setq org-agenda-files (list "~/Documents/notes/org/life.org"))
  (setq org-html-htmlize-output-type 'css)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "DONE")))
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-log-done t)
  (setq org-capture-templates '(("j" "Journal Entry"
                                 entry (file+olp+datetree "~/Google Drive/My Drive/Org/journal.org")
                                 "* %?" :empty-lines 1))))

;; Sticky headers at the top of the buffer (matching org outline)
;; https://github.com/alphapapa/org-sticky-header/tree/master
(use-package org-sticky-header
  :config
  (setq org-sticky-header-full-path 'full)
  :straight t
  :hook (org-mode . org-sticky-header-mode))

;; css and syntax highlighting for exported docs
(use-package htmlize
  :straight t)

;; org-export packages
;; (use-package ox-pandoc
;;   :straight t)

;;; Markdown support for emacs
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t) ; Make code block syntax highlighted
  :bind(:map markdown-mode-map
	     ("C-c C-e" . markdown-do)))

;; spell checking w/ flyspell
(use-package flyspell
  :straight nil
  :config
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;; Magit (intuitive git interface)
;; https://magit.vc/
(use-package magit
  :straight t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :config
  (setq dired-dwim-target t))

;; Fancy, polished and modernized dired
;; https://github.com/alexluigit/dirvish
;; https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#sample-config
(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")))
  :config
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
  (setq dirvish-mode-line-format
	'(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
	'(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
	dirvish-side-attributes
	'(vc-state nerd-icons collapse file-size))
  (setq dirvish-large-directory-threshold 20000)
  :bind
  (("C-c -" . dirvish-side)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; https://github.com/doomemacs/themes
(use-package doom-themes
  :straight t
  :config)

;; (load-theme 'doom-badger t)
;; (load-theme 'doom-ir-black t)

;; Doom Modeline - much easier on the eyes
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; Run M-x nerd-icons-install-fonts to install the necessary fonts.
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; (nerd-icons-font-family "Martian Mono Nerd Font")
  :straight )

;; Sticky headers for programming modes
;; https://github.com/alphapapa/topsy.el
(use-package topsy
  :straight t)
;; :hook (prog-mode . topsy-mode))

;; Highlights TODOs and other configured keywords in buffer
;; https://github.com/tarsius/hl-todo
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
                                        ; TODO: look into todo integrations

;; Colorize color names in buffers
;; https://github.com/emacsmirror/rainbow-mode
(use-package rainbow-mode
  :straight t)

;; Rainbow Delimiters - who doesn't love colors
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Git Gutter -- sidebar / fringe indicators of changes
;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :straight t
  :config
  (setq git-gutter:update-interval 0.2))

;; TODO: disable this in terminal mode
;; highlights the modified region (yank/kill)
;; https://github.com/minad/goggles
(use-package goggles
  :straight t
  :hook ((prog-mode org-mode) . goggles-mode)
  :config
  (goggles-define yank evil-paste-after) ; make pasting from evil mode highlighted
  (setq-default goggles-pulse t))

;; NYAN cat in modeline
;; https://github.com/TeMPOraL/nyan-mode
(use-package nyan-mode
  :straight t
  :config
  (setq nyan-wavy-trail t)
  :init
  (nyan-mode 1))

;; Party Parrot in modeline
;; https://github.com/dp12/parrot
(use-package parrot
  :straight t
  :init (parrot-mode 1))

(add-hook 'post-command-hook 'parrot-start-animation)
(add-hook 'post-command-hook 'nyan-start-animation)

;;; Mini-buffer improvements (fido, orderless, marginalia)
;; Let's try [icomplete / fido / ido] mode for a while.
;; (icomplete-vertical-mode)

;; Minibuffer style stwaeks
;; https://github.com/minad/vertico
(use-package vertico
  :straight t
  :init
  (vertico-indexed-mode)
  (vertico-reverse-mode)
  (vertico-mode))

;; Ordering regex for completion
;; https://github.com/oantolin/orderless
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Show docstrings and other useful info in minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia
  :straight t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :straight t
  :bind (
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; corfu: mini completion ui
;;https://github.com/minad/corfu
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode)
  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Add corfu extensions
(use-package cape
  :bind ("M-p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package emacs
  :custom
  (tab-always-indent 'complete) ;; Enable indentation+completion using the TAB key.
  (text-mode-ispell-word-completion nil) ; Emacs 30 and newer: Disable Ispell completion function, use `cape-dict' as an alternative.
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Syntax checking
;; https://www.flycheck.org/en/latest/languages.html
;; https://github.com/flycheck/flycheck
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;;; External code formatting tool integration (format-all)
;; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :straight t)

;; Editorconfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; Whitespace cleanup
(use-package whitespace-cleanup-mode
  :straight t
  :config
  (global-whitespace-cleanup-mode))

(use-package compile
  :straight nil
  :config
  (setq compilation-scroll-output t))

(add-hook 'python-mode-hook
  	  (lambda () (set (make-local-variable 'compile-command)
  			  (format "python3 %s" (file-name-nondirectory buffer-file-name)))))

;; Abbrevs and Snippets
(define-abbrev global-abbrev-table "t" "the")
(define-abbrev global-abbrev-table "g" "go")
;; URLs
(define-abbrev global-abbrev-table "mygh" "https://github.com/lemonase")
;; Timestamps
(define-abbrev global-abbrev-table "dt" "" 'insert-current-iso-date)
(define-abbrev global-abbrev-table "dts" "" 'insert-current-iso-date-time)
(define-abbrev global-abbrev-table "td" "" 'insert-current-iso-date)
(define-abbrev global-abbrev-table "tds" "" 'insert-current-iso-date-time)

;;; Matching brackets and parens with (electric-pair-mode)
(electric-pair-mode 1)

;; Snippets
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :straight t
  :init (yas-global-mode 1))

;; Snippet Files / Contents
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; https://github.com/AndreaCrotti/yasnippet-snippets/tree/master/snippets/emacs-lisp-mode
(use-package yasnippet-snippets
  :straight t)

;; Emmet: for writing HTML tags much easier and quicker
;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :straight t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

;; LSP Modes
(use-package eglot
  :straight nil
  :defer t
  :hook ((python-mode . eglot-ensure)
	 (go-mode . eglot-ensure)))

;; Allow flycheck errors to show with eglot
;;https://github.com/flycheck/flycheck-eglot
;; (use-package flycheck-eglot
;;   :straight t
;;   :after (flycheck eglot)
;;   :config
;;   (global-flycheck-eglot-mode 1))

;; Lua mode
(use-package lua-mode
  :straight t)

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

;;; Extra Language Modes
;; docker
(use-package docker
  :straight t
  :defer t)
(use-package dockerfile-mode
  :straight t
  :defer t)

;; csv
(use-package csv-mode
  :straight t
  :defer t)

;; vimrc 
(use-package vimrc-mode
  :straight t)

;; Local Environment File
(use-package load-env-vars
  :straight t)

(defvar my-env-file "~/.local/.env" "Local environment file.")
(let ((my-env-file "~/.local/.env"))
  (if (file-exists-p my-env-file)
      (load-env-vars my-env-file)))

;; LLM support (must configure with api keys)
;; (use-package gptel
;;   :straight t)
;; (setq gemini-api-key (funcall (lambda (prompt) (read-passwd prompt)) "Enter Gemini API key: "))
;; (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
;; (gptel-make-openai "OpenAI" :key (getenv "OPENAI_KEY") :stream t)
;; (gptel-make-gemini "Gemini" :stream t :key gptel-api-key)
;; (gptel-make-openai "OpenAI" :stream t :key gptel-api-key)

;; for Win32
(when (eq system-type 'windows-nt)
  ;; Powershell
  (use-package powershell
    :straight t)

  (set-frame-font "Cascadia Code 12" nil t)

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
  (setq find-program "C:/cygwin64/bin/find.exe")
  (setq grep-program "C:/cygwin64/bin/grep.exe")
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

;; Have to change emacs init dir for Windows
;; https://emacs.stackexchange.com/a/12886
;; (setenv "HOME" "C:/Users/itzja")
;; (setq default-directory "C:/Users/user")
;; (setq user-init-file "C:/Users/user/.emacs.d/init.el")
;; (setq user-emacs-directory "C:/Users/user/.emacs")
;; (load user-init-file)

;; xah-lee functions
;; http://xahlee.info/index.html

(defun open-in-vscode ()
  "Open current file or dir in vscode."
  (interactive)
  (let ((xpath (if buffer-file-name buffer-file-name (expand-file-name default-directory))))
    (message "path is %s" xpath)
    (cond
     ((eq system-type 'darwin)
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument xpath))))
     ((eq system-type 'windows-nt)
      (shell-command (format "code.cmd %s" (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (shell-command (format "code %s" (shell-quote-argument xpath)))))))

;;; Buffer local variables - ask to save/tangle.
;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Reload?")(load-file user-init-file))) nil t)
;; eval: (add-hook 'after-save-hook (lambda ()(if (y-or-n-p "Tangle?")(org-babel-tangle))) nil t)
;; End:
;;; init.el ends here

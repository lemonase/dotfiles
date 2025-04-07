;; init-vanilla.el emacs with no external packages

;; init inspo:
;; hjkjttps://github.com/suvratapte/dot-emacs-dot-d/blob/e7f44d981004b44bb9ae23b6b67c421404ea6b4e/init.el
;; https://emacsredux.com/
;; https://github.com/bbatsov/crux

;;; User Info
(setq user-full-name "James Dixon")
(setq user-mail-address "notjamesdixon@gmail.com")

;;; Inhibit - Do not do these things
(setq-default inhibit-startup-screen t)  ; No startup message plz
(setq-default visible-bell 1)            ; Flash when the bell rings (no sound)
(setq-default create-lockfiles nil)      ; Do not create lockfiles
(setq-default auto-save-default nil)     ; Do not autosave
(setq-default global-auto-revert-mode t) ; Update to on disk
(menu-bar-mode -1)                       ; No menu bar
(tool-bar-mode -1)                       ; No tool bar
(global-goto-address-mode 1)             ; Make links and addresses go-to able

;; Show paren differently
(setq-default show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Tabs
(setq-default indent-tabs-mode nil)

;;; Comment Settings
(setq-default comment-multi-line t)
(setq-default comment-empty-lines t)
(setq-default fill-column 80)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;;; Backwards kill
(defadvice kill-region (before unix-werase activate compile)
  "When called interactively with no active region, delete a single word backwards instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (save-excursion (backward-word 1) (point)) (point)))))

;;; Modes

;; icomplete
(icomplete-vertical-mode 1)

;; which-key mode
(which-key-mode t)

;; fly* modes
(flymake-mode t)
(flyspell-mode t)

;;; Keybinds
;; keybinds that just make sense
(global-set-key (kbd "C-x C-r") 'recentf)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file user-init-file)))
(fset 'yes-or-no-p 'y-or-n-p)

;;; General Hooks

;; Lines and columns
(add-hook 'prog-mode-hook 'column-number-mode) ; Toggle column number display in the mode line.
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; Only show line nums for program modes

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Platform Specifics

;; for Win32
(if (eq system-type 'windows-nt)
    (set-frame-font "Cascadia Code 12" nil t))

;; https://emacs.stackexchange.com/a/12886
;; (setenv "HOME" "C:/Users/itzja")
;; (setq default-directory "C:/Users/user")
;; (setq user-init-file "C:/Users/user/.emacs.d/init.el")
;; (setq user-emacs-directory "C:/Users/user/.emacs")
;; (load user-init-file)

(load-theme 'modus-vivendi)

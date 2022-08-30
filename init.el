;; Initialize package sources
;;(require 'package)
;;
;;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                         ("org" . "https://orgmode.org/elpa/")
;;                         ("elpa" . "https://elpa.gnu.org/packages/")))
;;
;;(package-initialize)
;;(unless package-archive-contents
;; (package-refresh-contents))

;; Enable straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Initialize use-package on non-Linux platforms
;;(unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; UI Configs
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Happy Hacking")

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
;;(set-fringe-mode nil)      ; Give some breathing room on the edges

;; Set up the visible bell
(setq visible-bell t)

;; Add columns and line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;;(global-whitespace-mode -1)
;;(setq display-line-numbers 'relative)

;; Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook
;;                term-mode-hook
;;                shell-mode-hook
;;                eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; TODO: customize based on monitor size
;;(add-to-list 'default-frame-alist '(height . 80))
;;(add-to-list 'default-frame-alist '(width . 200))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; TODO: figure out best way to autosave
;; Don't use #filename#, it's cluttered
;; Other configs
(setq make-backup-files t
      auto-save-default t
      backup-directory-alist
      `(("." . "~/.emacs-auto-saves"))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

(desktop-save-mode t)
(savehist-mode t)
(add-to-list 'savehist-additional-variables 'kill-ring)

;; Auto reload buffer when changed on disk
(global-auto-revert-mode t)

;; TODO fix kill-emacs error
;;(setq kill-emacs-hook nil)
;; For some reason org-babel is getting added to kill-emacs-hook
;; May have something to do with running emacs from terminal instead of Gnome (which I can't do with home-manager for some reason)
;; Old value (org-babel caused errors)
;; (savehist-autosave org-babel-remove-temporary-stable-directory org-babel-remove-temporary-directory lsp--global-teardown recentf-save-list bookmark-exit-hook-internal tramp-archive-cleanup-hash tramp-dump-connection-properties straight--delete-stderr-file)

;;;; Custom keybinding
(use-package general
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "TAB" '(switch-to-previous-buffer :which-key "previous buffer")
  "SPC" '(helm-M-x :which-key "M-x")
  "y" '(helm-show-kill-ring :which-key "Show kill ring")
  ;; Buffers
  "b"   '(:which-key "buffer")
  "bb"  '(helm-mini :which-key "buffers list")
  "bd"  '(kill-current-buffer :which-key "kill current buffer")
  ;; Window
  "w"   '(:which-key "window")
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wa"  '(ace-window :which-key "select window")
  "ws"  '(ace-swap-window :which-key "swap windows")
  "wx"  '(delete-window :which-key "delete window")
  "q"   '(:which-key "quit")
  "qz"  '(delete-frame :which-key "delete frame")
  "qq"  '(kill-emacs :which-key "quit")
  ;; Magit
  "g"   '(:which-key "git")
  "gs"  '(magit-status :which-key "git status")
  ;; Add something new
  "a"   '(:which-key "add")
  "at"  '(eshell :which-key "shell")
  "af"  '(make-frame :which-key "frame")
  ;; NeoTree
  ;;"ft"  '(neotree-toggle :which-key "toggle neotree")
  ;; Org
  ;; Projectil
  ;; "pf"  '(helm-projectile-find-file :which-key "find files")
  ;; "pp"  '(helm-projectile-switch-project :which-key "switch project")
  ;; "pb"  '(helm-projectile-switch-to-buffer :which-key "switch buffer")
  ;; "pr"  '(helm-show-kill-ring :which-key "show kill ring")
  ;;  "/"   '(helm-projectile-rg :which-key "ripgrep")
))
;;

;; Switch to most recent buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; Vim Evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;;; Delete without register, "DD" deletes line
;;;;(evil-define-operator evil-delete-without-register (beg end type yank-handler)
;;;;  (interactive "<R><y>")
;;;;  (evil-delete beg end type ?_ yank-handler))
;;;;(define-key evil-normal-state-map (kbd "D") 'evil-delete-without-register)
;;;;(define-key evil-visual-state-map (kbd "D") 'evil-delete-without-register)
;;
;;;; Do something similar with 'evil-change "C-w" or "x" change without register/kill ring
;;;;(define-key evil-visual-state-map (kbd "c") 'evil-delete-without-register)
;;
;;;; Use visual line motions, allowing scroll within wrapped lines
;;;;(evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;;;(evil-global-set-key 'motion "k" 'evil-previous-visual-line)



;; Keep cursor in place when scrolling
(setq scroll-preserve-screen-position 1)
;;
;;;; Below scroll commands are backwards, e.g. "scroll-down" actually scrolls up
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

;; Set C-k/C-j to scroll up/down
(global-set-key (kbd "C-k") 'scroll-half-page-down)
(global-set-key (kbd "C-j") 'scroll-half-page-up)

;; scroll window up/down by one line
(global-set-key (kbd "M-k") 'scroll-down-line)
(global-set-key (kbd "M-j") 'scroll-up-line)

(use-package ace-window)

;; TODO
(use-package helm
  :config
  (helm-mode 1))

;;;; Helm
;;(use-package helm
;;  :init
;;  (setq helm-M-x-fuzzy-match t
;;        helm-mode-fuzzy-match t
;;        helm-buffers-fuzzy-matching t
;;        helm-recentf-fuzzy-match t
;;        helm-locate-fuzzy-match t
;;        helm-semantic-fuzzy-match t
;;        helm-imenu-fuzzy-match t
;;        helm-completion-in-region-fuzzy-match t
;;        helm-candidate-number-list 80
;;        (setq helm-split-window-inside-p t
;;        helm-move-to-line-cycle-in-source t
;;        helm-echo-input-in-header-line t
;;        helm-autoresize-max-height 0
;;       helm-autoresize-min-height 20)
;;  :config
;;  (helm-mode 1))
;;
;;
;;;;(use-package ivy
;;;;  :diminish
;;;;  :bind (("C-s" . swiper)
;;;;         :map ivy-minibuffer-map
;;;;         ("TAB" . ivy-alt-done)       
;;;;         ("C-l" . ivy-alt-done)
;;;;         ("C-j" . ivy-next-line)
;;;;         ("C-k" . ivy-previous-line)
;;;;         :map ivy-switch-buffer-map
;;;;         ("C-k" . ivy-previous-line)
;;;;         ("C-l" . ivy-done)
;;;;         ("C-d" . ivy-switch-buffer-kill)
;;;;         :map ivy-reverse-i-search-map
;;;;         ("C-k" . ivy-previous-line)
;;;;         ("C-d" . ivy-reverse-i-search-kill))
;;;;  :config
;;;;  (ivy-mode 1))
;;
;;;;(use-package command-log-mode)
;;

;; TODO
;; which-key-setup-minibuffer
;; Which Key
;;(use-package which-key)
  ;;:init
  ;;(setq which-key-separator " ")
  ;;(setq which-key-prefix-prefix "+")
;;  :diminish which-key-mode
  ;;:config
  ;;(which-key-mode)
  ;;(setq which-key-idle-delay 0.5))
;;
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; All The Icons
;; Requires fonts: `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; TODO
;;(use-package counsel
;;  :bind(("M-x" . counsel-M-x))


;; Shell configs

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;;(use-package vterm)

(use-package direnv
 :config
 (direnv-mode))

(use-package sudo-edit)

;;;; Programming Configs

;;;; Global tabs into two spaces unless otherwise specified
;;(setq indent-tabs-mode nil)
;;(setq tab-width 2)
;;;;
;;;;(use-package rustic
;;;;  :config
;;;;  (setq rustic-indent-offset 2)
;;;;  (setq rustic-format-on-save t)
;;;;  (add-hook 'rustic-mode-hook rustic-mode-hook))
;;;;
;;;;(defun rustic-mode-hook ()
;;;;  (when buffer-file-name
;;;;    (setq-local buffer-save-without-query t)))
;;
(use-package rust-mode
  :config
  (setq rust-indent-offset 2)
  (setq rust-format-on-save nil))

;;(use-package haskell-mode
;;  :config
;;  (setq haskell-indent-mode t)
;;  (setq haskell-indent-offset 2))
;;
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package yaml-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

;;(setq js-indent-level 2)
;;(setq js-format-on-save t)
;;

;;Keybindings for lambda and forall
(global-set-key (kbd "M-l") "λ")
(global-set-key (kbd "M-;") "∀")
;;
(use-package magit)
;;
;;(use-package lsp-mode)
;;
(use-package lean4-mode
  :straight (lean4-mode :type git :host github :repo "leanprover/lean4-mode")
  ;; to defer loading the package until required
  :commands (lean4-mode))
;;
;;(global-set-key (kbd "S-SPC") #'company-complete)
;;;;(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
;;
;;;; Org mode -------------------------------------------------
;;(defun org-mode-setup ()
;;  (org-indent-mode)
;;  (visual-line-mode 1)
;;  (font-lock-add-keywords 'org-mode
;;                          '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))
;;  
(use-package org
;;  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ⬎"))
;;
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Set faces for heading levels
;;(dolist (face '((org-level-1 . 1.2)
;;                (org-level-2 . 1.1)
;;                (org-level-3 . 1.05)
;;                (org-level-4 . 1.0)
;;                (org-level-5 . 1.1)
;;                (org-level-6 . 1.1)
;;                (org-level-7 . 1.1)
;;                (org-level-8 . 1.1)))
;;  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

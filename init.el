
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; UI Configs
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Happy Hacking")

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room on the edges

;; Add columns and line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Customized for 34" as well
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

(use-package ace-window)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Other configs
(setq make-backup-files t
      auto-save-default t
      backup-directory-alist
      `(("." . "~/.emacs-auto-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(desktop-save-mode t)
(savehist-mode t)
(add-to-list 'savehist-additional-variables 'kill-ring)
;; Auto reload buffer when changed on disk
(global-auto-revert-mode t)

;; Custom keybinding
(use-package general
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
;;  "/"   '(helm-projectile-rg :which-key "ripgrep")
;;  "TAB" '(switch-to-previous-buffer :which-key "previous buffer")
  "SPC" '(helm-M-x :which-key "M-x")
;;  "pf"  '(helm-projectile-find-file :which-key "find files")
;;  "pp"  '(helm-projectile-switch-project :which-key "switch project")
;;  "pb"  '(helm-projectile-switch-to-buffer :which-key "switch buffer")
;;  "pr"  '(helm-show-kill-ring :which-key "show kill ring")
  ;; Buffers
  "bb"  '(helm-mini :which-key "buffers list, enter name to create new")
;;  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wr"  '(ace-window :which-key "rotate windows")
  "ws"  '(ace-swap-window :which-key "swap windows")
  "wx"  '(delete-window :which-key "delete window")
  "qz"  '(delete-frame :which-key "delete frame")
  "qq"  '(kill-emacs :which-key "quit")
;;  ;; NeoTree
;;  ;;"ft"  '(neotree-toggle :which-key "toggle neotree")
;;  ;;"br"  '(rename-current-buffer-file :which-key "rename current buffer file")
;;  ;; Org
;;  ;; Others
;;  "at"  '(eshell :which-key "open shell")
  "af"  '(make-frame :which-key "new frame")
))

;; Vim Evil mode
(use-package evil
  :config
  (evil-mode 1))

;; Delete without register, "DD" deletes line
  (evil-define-operator evil-delete-without-register (beg end type yank-handler)
    (interactive "<R><y>")
    (evil-delete beg end type ?_ yank-handler))
  (define-key evil-normal-state-map (kbd "D") 'evil-delete-without-register)
  (define-key evil-visual-state-map (kbd "D") 'evil-delete-without-register)

;; Do something similar with 'evil-change "C-w" change without register/kill ring
;;(define-key evil-visual-state-map (kbd "c") 'evil-delete-without-register)

;; Helm
(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
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
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

;;(use-package ivy
;;  :diminish
;;  :bind (("C-s" . swiper)
;;         :map ivy-minibuffer-map
;;         ("TAB" . ivy-alt-done)       
;;         ("C-l" . ivy-alt-done)
;;         ("C-j" . ivy-next-line)
;;         ("C-k" . ivy-previous-line)
;;         :map ivy-switch-buffer-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-l" . ivy-done)
;;         ("C-d" . ivy-switch-buffer-kill)
;;         :map ivy-reverse-i-search-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-d" . ivy-reverse-i-search-kill))
;;  :config
;;  (ivy-mode 1))

(use-package command-log-mode)

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;;(use-package which-key
;;  :init (which-key-mode)
;;  :diminish which-key-mode
;;  :config
;;  (setq which-key-idle-delay 0.3))

;; All The Icons
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Programming Configs
;; Global tabs into two spaces unless otherwise specified
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)
;;(defvaralias 'rust-indent-offset 'tab-width)

;;Keybindings for lambda and forall
(global-set-key (kbd "M-l") "λ")
(global-set-key (kbd "M-;") "∀")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package counsel
  :bind(("M-x" . counsel-M-x))



        
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(rainbow-delimiters ace-window which-key use-package typescript-mode tern spaceline smooth-scrolling rustic rust-mode neotree lsp-ui js2-mode ivy helm-rg helm-projectile general flycheck evil-org evil-escape eglot doom-themes doom-modeline company-lsp command-log-mode anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

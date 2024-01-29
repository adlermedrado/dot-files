;; init file tips/examples:
;; https://github.com/ebellani/Emacs.d/blob/master/init.el
;; https://github.com/dunossauro/dotfiles/blob/main/.emacs.d/init.el

;; Configure straight.el
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

(straight-use-package 'use-package)

;; Install use-package
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("nongnu"     . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"       . "https://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize t))

(eval-when-compile
  (require 'use-package))

;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; Remove menus, setup GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; window size
(add-to-list 'default-frame-alist '(height . 26))
(add-to-list 'default-frame-alist '(width . 89))

;; window modes
(ido-mode 1)
(cua-mode 1)
(display-time-mode 1)

;; Buffer Tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-x [" . centaur-tabs-backward)
  ("C-x ]" . centaur-tabs-forward))
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'left)
(centaur-tabs-headline-match)

;; Font
(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 150)

;; Flex buffer
;; ibuffer defaults to C-x C-b
(defalias 'list-buffers 'ibuffer-other-window)

;; Enable line numbers
(global-display-line-numbers-mode 1)
(use-package hl-line
  :config
  (global-hl-line-mode t))

;; Remove welcome buffer
(setq inhibit-startup-message t
      inhibit-startup-screen t
      initial-buffer-choice  nil
      initial-scratch-message nil
      auto-save-default nil
      make-backup-files nil
      ido-enable-flex-matching t
      ido-everywhere t)

;; Update buffers
(global-auto-revert-mode t)

(use-package company
  :straight t
  :demand t
  :commands (company-mode company-indent-or-complete-common)
  :config
  (setf company-idle-delay 0
	company-minimum-prefix-length 1
        company-selection-wrap-around t)
  :hook (after-init . global-company-mode))

(use-package markdown-mode
  :straight t
  :mode (("\.md$" . markdown-mode)))

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Project organization
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/git/")
	projectile-switch-project-action 'neotree-projectile-action
	projectile-indexing-method 'alien
	projectile-use-git-grep 1))

;; Incremental completions
(use-package helm
  :straight t
  :diminish
  :bind (("C-h a"   . helm-apropos)
         ("C-x b"   . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-m" . helm-M-x)
         ("C-x m"   . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x r l" . helm-filtered-bookmarks)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x i"   . helm-imenu)
         ("M-y"     . helm-show-kill-ring)
         ("M-i"     . helm-swoop-without-pre-input)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("C-z"   . helm-select-action))
  :config
  (setq helm-ff-transformer-show-only-basename nil
        helm-external-programs-associations '(("zip" . "unzip")
                                              ("mp4" . "smplayer")
                                              ("mkv" . "smplayer")
                                              ("docx" . "libreoffice"))
        helm-completion-style 'emacs
        helm-yank-symbol-first                 t
        helm-move-to-line-cycle-in-source      t
        helm-buffers-fuzzy-matching            t
        helm-ff-auto-update-initial-value      t
        helm-imenu-fuzzy-match                 t
        helm-buffer-max-length                 50
        helm-ff-candidate-number-limit         200
        ;; helm-display-function                  'helm-display-buffer-in-own-frame
        helm-display-buffer-width              90
        helm-display-function                  'helm-default-display-buffer
        helm-display-buffer-reuse-frame        t
        helm-use-undecorated-frame-option      t
        helm-show-completion-display-function #'helm-show-completion-default-display-function)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-hist-mode-map
                          [remap eshell-previous-matching-input-from-input]
                          'helm-eshell-history))))

(use-package magit
  :straight t
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
        magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package forge
  :straight t
  :after magit)

(use-package git-timemachine
  :straight t
  :after magit)

(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :config
  (unless (default-value 'persp-mode)
    (persp-mode +1))
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-state-default-file "~/.emacs.d/persp.state"))

(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind (("C-\\". 'neotree-toggle))
  )

;; Which key helper to show keyboard options
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq vc-follow-symlinks t)

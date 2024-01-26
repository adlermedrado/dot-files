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

;; Remove menus
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
  ;;(doom-themes-neotree-config)
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
  :bind (("C-1". 'neotree-toggle));; JetBrains toggle file explorero key
  )

;; Which key helper to show keyboard options
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq vc-follow-symlinks t)

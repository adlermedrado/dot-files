;;; init.el --- Adler's Emacs init file

;;; Commentary:
;;
;; Adler Medrado' Emacs Config.
;;

;;; Code:

;; set load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

;; Install use-package
(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("nongnu"     . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"       . "https://orgmode.org/elpa/") t)

(unless package--initialized (package-initialize t))

(eval-when-compile
  (require 'use-package))

;; Remove menus, setup GUI
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq-default message-log-max 10000)

;; window size
(add-to-list 'default-frame-alist '(height . 26))
(add-to-list 'default-frame-alist '(width . 89))

;; window modes
(ido-mode 1)
(cua-mode 1)
(display-time-mode 1)
(winner-mode 1)

;; set shell env
(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;; Theming
(use-package solarized-theme
  :ensure t)

(load-theme 'solarized-light t)

;; macOS settings
(when (eq system-type 'darwin)
  (message "Setting up keybings for macOS")
  (setq mac-command-modifier 'super ;⌘=super-key (but can't use s-SPACE,TAB)
	mac-right-command-modifier 'meta ; meta-f/b are hard to reach otherwise
	mac-option-modifier 'meta	 ;alt=meta=option
	mac-right-option-modifier nil ;retain compose characters, düde
	mac-right-control-modifier 'hyper
	delete-by-moving-to-trash t
	browse-url-browser-function 'browse-url-default-macosx-browser
	trash-directory (expand-file-name ".Trash" (getenv "HOME")))

  (bind-keys ("s-s" . save-buffer)
	     ("s-a" . mark-whole-buffer)
	     ("s-c" . kill-ring-save)
	     ("s-m" . suspend-frame)
	     ("s-t" . (lambda (arg) (interactive "p")
			(let ((mac-frame-tabbing t))
			  (if (not (eq arg 4))
			      (make-frame)
			    (call-interactively #'find-file-other-frame)))))
	     ("s-x" . kill-region)
	     ("s-v" . yank)
	     ("s-w" . delete-frame)
	     ("s-n" . make-frame-command))
)

;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/code/"))
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-rg)
         ("C-c f" . counsel-recentf)
         ("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Terminal emulator
(use-package vterm
  :ensure t)

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

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)
(setq org-directory "~/org")

;; Font
(set-face-attribute 'default nil :font "JetbrainsMono Nerd Font" :height 150)

;; Flex buffer
;; ibuffer defaults to C-x C-b
(defalias 'list-buffers 'ibuffer-other-window)

;; Enable line numbers
(global-display-line-numbers-mode 1)
;; (add-hook 'mu4e-headers-mode-hook (lambda () (display-line-numbers-mode 0)))
;; (add-hook 'mu4e-main-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Remove welcome buffer
(setq inhibit-startup-message t
      inhibit-startup-screen t
      initial-buffer-choice  nil
      initial-scratch-message nil
      auto-save-default nil
      make-backup-files nil)

;; Update buffers
(global-auto-revert-mode t)

(use-package markdown-mode
  :mode (("\.md$" . markdown-mode)))

(use-package all-the-icons
  :ensure t)

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status))
  :config
  ;; Exibe o buffer Magit em uma janela dedicada, ocupando a tela inteira
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  
  ;; Fecha automaticamente o buffer Magit ao pressionar 'q'
  (setq magit-bury-buffer-function #'magit-mode-quit-window))

;; Projectile + magit
(define-key projectile-mode-map (kbd "C-c p g") 'magit-status)

(use-package forge
  :after magit)

(use-package git-timemachine
  :after magit)

;; Which key helper to show keyboard options
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; flycheck syntax checker
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package tree-sitter
  :ensure t
  :hook ((python-mode . tree-sitter-mode)
         (js-mode . tree-sitter-mode)
      	 (markdown-mode . tree-sitter-mode)
	 (emacs-lisp-mode . tree-sitter-mode))
  :config
  (require 'tree-sitter-langs))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Modeline
(use-package doom-modeline
  :ensure t
  :after flycheck
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  :config
  (setq doom-modeline-check-simple-format t))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t)
  :bind
  (("C-x t t" . treemacs)
   ("C-x t p" . treemacs-projectile)))

;;(global-set-key (kbd "C-x t t") 'abm/treemacs-project-toggle)

;; Abre Treemacs no diretório do projeto atual
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :config
  (setq treemacs-follow-mode t))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1)
  (define-key lsp-mode-map (kbd "C-c l t") 'lsp-treemacs-symbols))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
  :config
  (setq treemacs-git-mode 'deferred))

(use-package treemacs-all-the-icons
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         )
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-idle-delay 0.500))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-fontify 'always))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  :hook (prog-mode . company-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; email config
;; load mu4e from the installation path.
;; yours might differ check with the Emacs installation
;; (use-package mu4e
;;   :load-path  "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e/")

;; for sending mails
;;(require 'smtpmail)

;; we installed this with homebrew
;; (unless (executable-find "mu")
;;   (warn! "Couldn't find mu command. Mu4e requires this to work."))

;; (setq mu4e-mu-binary (executable-find "mu"))

;; this is the directory we created before:
;; (setq mu4e-maildir "~/.maildir")

;; this command is called to sync imap servers:
;; (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -aV"))
;; how often to call it in seconds:
;; (setq mu4e-update-interval 300)

;; save attachment to desktop by default
;; or another choice of yours:
;; (setq mu4e-attachment-dir "~/Desktop")

;; rename files when moving - needed for mbsync:
;; (setq mu4e-change-filenames-when-moving t)

;; list of your email adresses:
;; (setq mu4e-user-mail-address-list '("adlerbmedrado@icloud.com"))

;; Format mu4e header date
;; (setq mu4e-date-format "%y-%m-%d")

;; (setq mu4e-maildir-shortcuts
;;         '(("/icloud/INBOX" . ?i)
;;           ("/icloud/Sent Messages" . ?I)
;; 	  ("/icloud/Drafts" . ?F)
;; 	  ("/icloud/Archive". ?A)
;; 	  ("/icloud/Junk" . ?J)
;; 	  ("/icloud/Deleted Messages" . ?D)
;; 	  ("/gmail/INBOX" . ?g)
;;           ("/gmail/[Gmail]/Sent Mail" . ?S)
;; 	  ("/gmail/Drafts" . ?D)
;; 	  ("/gmail/Archive" . ?V)
;; 	  ("/gmail/Trash" . ?R)))

;; (setq mu4e-contexts
;;       `(,(make-mu4e-context
;;           :name "icloud"
;;           :enter-func
;;           (lambda () (mu4e-message "Enter adlerbmedrado@icloud.com context"))
;;           :leave-func
;;           (lambda () (mu4e-message "Leave adlerbmedrado@icloud.com context"))
;;           :match-func
;;           (lambda (msg)
;;             (when msg
;;               (mu4e-message-contact-field-matches msg
;;                                                   :to "adlerbmedrado@icloud.com")))
;;           :vars '((user-mail-address . "adler@adlermedrado.com.br" )
;;                   (user-full-name . "Adler Medrado")
;;                   (mu4e-drafts-folder . "/icloud/Drafts")
;;                   (mu4e-refile-folder . "/icloud/Archive")
;;                   (mu4e-sent-folder . "/icloud/Sent Messages")
;; 		  (mu4e-junk-folder . "/icloud/Junk")
;;                   (mu4e-trash-folder . "/icloud/Deleted Messages")))
;;        ,(make-mu4e-context
;;           :name "gmail"
;;           :enter-func
;;           (lambda () (mu4e-message "Enter adlermedrado@gmail.com context"))
;;           :leave-func
;;           (lambda () (mu4e-message "Leave adlermedrado@gmail.com context"))
;;           :match-func
;;           (lambda (msg)
;;             (when msg
;;               (mu4e-message-contact-field-matches msg
;;                                                   :to "adlermedrado@gmail.com")))
;;           :vars '((user-mail-address . "adlermedrado@gmail.com")
;;                   (user-full-name . "Adler Medrado")
;;                   (mu4e-drafts-folder . "/gmail/Drafts")
;;                   (mu4e-refile-folder . "/gmail/Archive")
;;                   (mu4e-sent-folder . "/gmail/Sent")
;;                   (mu4e-trash-folder . "/gmail/Trash")))))
;;
;; (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
;; (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

;; show all messages
;; (setq mu4e-headers-include-related nil)
;; (setq mu4e-headers-results-limit 500)

;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
;;(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
;;(setq message-sendmail-envelope-from 'header)

;;(defun abm/set-msmtp-account ()
;;  (if (message-mail-p)
;;      (save-excursion
;;        (let*
;;            ((from (save-restriction
;;                     (message-narrow-to-headers)
;;                     (message-fetch-field "from")))
;;             (account
;;              (cond
;;               ((string-match "adlerbmedrado@icloud.com" from) "icloud"))))
;;          (setq message-sendmail-extra-arguments (list '"-a" account))))))

;;(add-hook 'message-send-mail-hook 'abm/set-msmtp-account)

;; mu4e cc & bcc
;; this is custom as well
;; (add-hook 'mu4e-compose-mode-hook
;;           (defun abm/add-cc-and-bcc ()
;;             "My Function to automatically add Cc & Bcc: headers.
;;     This is in the mu4e compose mode."
;;             (save-excursion (message-add-header "Cc:\n"))
;;             (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
;; (add-hook 'mu4e-compose-mode-hook 'company-mode)

;; store link to message if in header view, not to header query:
;; (setq org-mu4e-link-query-in-headers-mode nil)
;; don't have to confirm when quitting:
;; (setq mu4e-confirm-quit nil)
;; number of visible headers in horizontal split view:
;; (setq mu4e-headers-visible-lines 20)
;; don't show threading by default:
;; (setq mu4e-headers-show-threads nil)
;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
;; (setq mu4e-hide-index-messages t)
;; customize the reply-quote-string:
;; (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
;; M-x find-function RET message-citation-line-format for docs:
;; (setq message-citation-line-function 'message-insert-formatted-citation-line)
;; by default do not show related emails:
;; (setq mu4e-headers-include-related nil)
;; by default do not show threads:
;; (setq mu4e-headers-show-threads nil)

;;; mu4e bookmarks
;; (customize-set-variable
;;  'mu4e-bookmarks
;;  '((:name "iCloud - Unread"
;;           :query "maildir:/icloud/INBOX flag:unread"
;;           :hide t
;;           :key ?I)
;;    (:name "iCloud"
;;           :query "maildir:/icloud/INBOX"
;;           :key ?i)
;;    (:name "Spam - All"
;;           :query "maildir:/icloud/Junk"
;;           :key ?s)
;;    (:name "Trash - All"
;;           :query "maildir:\"/icloud/Deleted Messages\""
;;           :key ?b)
;;    (:name "Unread messages"
;;           :query "flag:unread AND NOT flag:trashed"
;;           :key ?u)
;;    (:name "Today's messages" :query "date:today..now" :key ?d)
;;    (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
;;    (:name "Messages with images" :query "mime:image/*" :key ?p)
;;    (:name "Messages with videos" :query "mime:video/*" :key ?v)
;;    (:name "Messages with audios" :query "mime:audio/*" :key ?a)))
;;
;; (add-to-list 'mu4e-header-info-custom
;;              '(:empty . (:name "Empty"
;;                          :shortname ""
;;                          :function (lambda (msg) "  "))))
;; (setq mu4e-headers-fields '((:empty         .    2)
;;                             (:human-date    .   12)
;;                             (:flags         .    6)
;;                             (:mailing-list  .   10)
;;                             (:from          .   22)
;;                             (:subject       .   nil)))
;;
;; (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
;; (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq vc-follow-symlinks t)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages
   '(doom-modeline which-key vterm solarized-theme shrink-path projectile nerd-icons flycheck exec-path-from-shell centaur-tabs all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

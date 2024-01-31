;; init file tips/examples:
;; https://github.com/ebellani/Emacs.d/blob/master/init.el
;; https://github.com/dunossauro/dotfiles/blob/main/.emacs.d/init.el
;; https://macowners.club/posts/email-emacs-mu4e-macos/

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
(add-hook 'mu4e-headers-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'mu4e-main-mode-hook (lambda () (display-line-numbers-mode 0)))

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
      ido-enable-flex-matching t)

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

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t 
        doom-themes-enable-italic t)
  (load-theme 'doom-monokai-classic t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
    (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

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
                                              ("mp4" . "mpv")
                                              ("mkv" . "mpv")
                                              ("docx" . "libreoffice"))
        helm-completion-style 'emacs
        helm-yank-symbol-first                 t
        helm-move-to-line-cycle-in-source      t
        helm-buffers-fuzzy-matching            t
        helm-ff-auto-update-initial-value      t
        helm-imenu-fuzzy-match                 t
        helm-buffer-max-length                 50
        helm-ff-candidate-number-limit         200
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

;; email config
;; load mu4e from the installation path.
;; yours might differ check with the Emacs installation
(use-package mu4e
  :load-path  "/opt/homebrew/Cellar/mu/1.10.8/share/emacs/site-lisp/mu/mu4e/")

;; for sending mails
(require 'smtpmail)

;; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))

;; this is the directory we created before:
(setq mu4e-maildir "~/.maildir")

;; this command is called to sync imap servers:
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;; how often to call it in seconds:
(setq mu4e-update-interval 300)

;; save attachment to desktop by default
;; or another choice of yours:
(setq mu4e-attachment-dir "~/Desktop")

;; rename files when moving - needed for mbsync:
(setq mu4e-change-filenames-when-moving t)

;; list of your email adresses:
(setq mu4e-user-mail-address-list '("adlerbmedrado@icloud.com"))

;; Format mu4e header date
(setq mu4e-date-format "%y-%m-%d")
(setq mu4e-headers-date-format "%Y-%m-%d")

(setq mu4e-maildir-shortcuts
        '(("/icloud/INBOX" . ?i)
          ("/icloud/Sent Messages" . ?I)
	  ("/icloud/Drafts" . ?F)
	  ("/icloud/Archive". ?A)
	  ("/icloud/Junk" . ?J)
	  ("/icloud/Deleted Messages" . ?D)))

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "icloud"
          :enter-func
          (lambda () (mu4e-message "Enter adlerbmedrado@icloud.com context"))
          :leave-func
          (lambda () (mu4e-message "Leave adlerbmedrado@icloud.com context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "adlerbmedrado@icloud.com")))
          :vars '((user-mail-address . "adler@adlermedrado.com.br" )
                  (user-full-name . "Adler Medrado")
                  (mu4e-drafts-folder . "/icloud/Drafts")
                  (mu4e-refile-folder . "/icloud/Archive")
                  (mu4e-sent-folder . "/icloud/Sent Messages")
		  (mu4e-junk-folder . "/icloud/Junk") 
                  (mu4e-trash-folder . "/icloud/Deleted Messages")))))

(setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
(setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

;; show all messages
(setq mu4e-headers-include-related nil)
(setq mu4e-headers-results-limit 10000)

;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)

(defun timu/set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "adlerbmedrado@icloud.com" from) "icloud"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun timu/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

;; store link to message if in header view, not to header query:
(setq org-mu4e-link-query-in-headers-mode nil)
;; don't have to confirm when quitting:
(setq mu4e-confirm-quit nil)
;; number of visible headers in horizontal split view:
(setq mu4e-headers-visible-lines 20)
;; don't show threading by default:
(setq mu4e-headers-show-threads nil)
;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
(setq mu4e-hide-index-messages t)
;; customize the reply-quote-string:
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
;; M-x find-function RET message-citation-line-format for docs:
(setq message-citation-line-function 'message-insert-formatted-citation-line)
;; by default do not show related emails:
(setq mu4e-headers-include-related nil)
;; by default do not show threads:
(setq mu4e-headers-show-threads nil)

;;; mu4e bookmarks
(customize-set-variable
 'mu4e-bookmarks
 '((:name "iCloud - Unread"
          :query "maildir:/icloud/INBOX flag:unread"
          :hide t
          :key ?I)
   (:name "iCloud"
          :query "maildir:/icloud/INBOX"
          :key ?i)
   (:name "Spam - All"
          :query "maildir:/icloud/Junk"
          :key ?s)
   (:name "Trash - All"
          :query "maildir:\"/icloud/Deleted Messages\""
          :key ?b)
   (:name "Unread messages"
          :query "flag:unread AND NOT flag:trashed"
          :key ?u)
   (:name "Today's messages" :query "date:today..now" :key ?d)
   (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
   (:name "Messages with images" :query "mime:image/*" :key ?p)
   (:name "Messages with videos" :query "mime:video/*" :key ?v)
   (:name "Messages with audios" :query "mime:audio/*" :key ?a)))

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;; (add-to-list 'load-path "packages") ; facultative when installed with make install
;; (require 'mu4e-thread-folding)
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

;; (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
;; (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
;; (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq vc-follow-symlinks t)

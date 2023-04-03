;; emacs config -*- lexical-binding: t; -*-

;; Stop customize from adding stuff to this file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; My custom lisp scripts
(add-to-list 'load-path (concat user-emacs-directory
        (convert-standard-filename "lisp/")))

;; Install straight.el  
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

;; Install use-package
(straight-use-package 'use-package)
;; And configure it to always use straight
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package meow
  :config
  (require 'meow-config)
  (meow-setup)
  (meow-global-mode 1))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init (vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)))
			 
(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
	      ("M-h" . vertico-directory-delete-word)))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :bind (("C-x C-b" . consult-buffer)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu
  :init (global-corfu-mode)
  :config
  (setq corfu-cycle t
	corfu-auto t
	corfu-auto-prefix 2
	corfu-auto-delay 0.0)
  :bind (:map corfu-map
	      ("C-j" . corfu-next)
	      ("C-k" . corfu-previous)))

(use-package beancount
  :straight (:host github
	   :repo "beancount/beancount-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode)))

;; TODO create a custom keybind to show balances

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode "pylsp"))
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package magit)

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(use-package pyvenv)

(use-package tidal
  :mode ("\\.tidal\\’" . tidal-mode)
  :config (setq tidal-interpreter "/usr/bin/ghci"
                haskell-process-type 'auto
		tidal-boot-script-path "~/.cabal/share/x86_64-linux-ghc-8.10.7/tidal-1.9.4/BootTidal.hs"))

;; Appearance
(add-to-list 'default-frame-alist
	     '(font . "Iosevka Comfy-11"))

(use-package ef-themes
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (setq ef-themes-to-toggle '(ef-frost ef-night))
  (ef-themes-select 'ef-night)
  (global-unset-key (kbd "C-t"))
  :bind
  ("C-t t" . 'ef-themes-toggle)
  ("C-t i" . text-scale-increase)
  ("C-t d" . text-scale-decrease)
  ("C-t r" . (lambda() (interactive) (text-scale-set 0)))))

;; Save recent files
(recentf-mode 1)
(setq recentf-max-saved-items 25)

;; Comp warnings on startup are annoying
(setq native-comp-async-report-warnings-errors 'silent)

;; Reduce screen clutter
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Reduce file clutter
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups/")))))

;; Shut up
(setq-default ring-bell-function 'ignore)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Reload files changed outside emacs
(global-auto-revert-mode 1)

;; Org config
(setq org-agenda-files
      '("inbox.org" "nextActions.org" "projects.org"))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

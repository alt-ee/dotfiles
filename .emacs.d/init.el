;; emacs config -*- lexical-binding: t; -*-

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

;; Install use-package, and configure it to always use straight
(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

(use-package meow
  :config
  (require 'meow-config)
  (meow-setup)
  (meow-global-mode 1))

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package nano-theme
  :straight (:host github
	     :repo "rougier/nano-theme")
  :config
  (setq nano-fonts-use t)
  (load-theme 'nano-dark t))

(use-package eglot)

(use-package magit)

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

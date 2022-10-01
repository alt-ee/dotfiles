;;; example-config.el -- Example Crafted Emacs user customization file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Crafted Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Crafted Emacs configuration directory as an example.
;;
;; In your configuration you can set any Emacs configuration variable, face
;; attributes, themes, etc as you normally would.
;;
;; See the README.org file in this repository for additional information.
;;
;; Things future me might want to know
;; C-M-i: complete text at point (cursor)
;; C-M-x: evaluate expression at point
;;
;; Help functions, C-h followed by a key
;; k: tell me what function is bound to a key
;; w: tell me what key(s) a function is bound to (not really necessary with vertico stack)
;; f: tell me what a function does
;; v: tell me what a variable does

;;; Code:
;; Modules:

(require 'crafted-defaults)  
(require 'crafted-ui)        
(require 'crafted-evil)      
(require 'crafted-ide)
(require 'crafted-python)
(require 'crafted-completion)
(require 'crafted-updates)

(crafted-package-install-package 'doom-themes)
(load-theme 'doom-gruvbox t)

(setq-default ring-bell-function 'ignore)

(crafted-package-install-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(crafted-package-install-package 'magit)

;; To not load `custom.el' after `config.el', uncomment this line.
(setq crafted-load-custom-file nil)


;;; example-config.el -- Example Rational Emacs user customization file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rational Emacs supports user customization through a `config.el' file
;; similar to this one.  You can copy this file as `config.el' to your
;; Rational Emacs configuration directory as an example.
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
;; w: tell me what key(s) a function is bound to
;; f: tell me what a function does
;; v: tell me what a variable does

;;; Code:
;; Modules:

(require 'rational-defaults)  
(require 'rational-ui)        
(require 'rational-evil)      
(require 'rational-ide)
(require 'rational-python)
(require 'rational-completion)

;; Set further font and theme customizations
(custom-set-variables
   '(rational-ui-default-font
     '(:font "Iosevka Comfy" :weight normal :height 150)))

(rational-package-install-package 'doom-themes)
(load-theme 'doom-gruvbox t)

(rational-package-install-package 'evil-escape)
(evil-escape-mode)
(setq-default evil-escape-key-sequence "kj")

;; To not load `custom.el' after `config.el', uncomment this line.
;; (setq rational-load-custom-file nil)

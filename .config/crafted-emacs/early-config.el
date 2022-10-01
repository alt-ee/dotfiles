;;; early-config.el --- Early init customizations for crafted emacs  -*- lexical-binding: t; -*-

(custom-set-faces (backquote (default ((t (:family "Iosevka" :height 130))))))

(setf default-frame-alist (assq-delete-all 'background-color default-frame-alist))
(setf default-frame-alist (assq-delete-all 'foreground-color default-frame-alist))

(provide 'early-config)

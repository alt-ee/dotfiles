(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/bin/clangd" "-log=verbose"))
(setq-default c-basic-offset 4)

(provide 'crafted-c)

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "/usr/bin/clangd" "-log=verbose"))

(provide 'crafted-c)

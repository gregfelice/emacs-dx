


;;
;; Built-in packages that this extra configures:
;; - [Eglot](https://github.com/joaotavora/eglot) ([Language Server Protocol (LSP) client](https://microsoft.github.io/language-server-protocol/))
;; - Treesit ([Tree-Sitter](https://github.com/tree-sitter) support)
;;
;;Both of these packages are new in Emacs 29. **Be sure to run `M-x treesit-install-language-grammar` to install the
;; language grammar you'll need before editing a file the respective language for the first time!** This is a quirk of how the built-in tree-sitter works
;; packages like [treesit-auto](https://github.com/renzmann/treesit-auto) can help with this if it becomes too much of an annoyance.

(message "Loading Python Developer feature")


(use-package company
  :ensure t
  :demand t
  :hook (python-ts-mode . company-mode)  ;; Enable company-mode in python-mode
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :config
  (add-to-list 'company-backends 'company-yasnippet))  ;; Add company-yasnippet once


(use-package yasnippet
  :ensure t
  :demand t
  :hook (python-ts-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets/python-mode")))


(require 'eglot)

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)


(provide 'python-developer)  ;; This defines the 'python-developer feature
(message "Loaded Python Developer feature")

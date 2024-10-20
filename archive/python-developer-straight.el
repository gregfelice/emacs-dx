


;;
;; Built-in packages that this extra configures:
;; - [Eglot](https://github.com/joaotavora/eglot) ([Language Server Protocol (LSP) client](https://microsoft.github.io/language-server-protocol/))
;; - Treesit ([Tree-Sitter](https://github.com/tree-sitter) support)
;;
;;Both of these packages are new in Emacs 29. **Be sure to run `M-x treesit-install-language-grammar` to install the
;; language grammar you'll need before editing a file the respective language for the first time!** This is a quirk of how the built-in tree-sitter works
;; packages like [treesit-auto](https://github.com/renzmann/treesit-auto) can help with this if it becomes too much of an annoyance.



;; Install straight.el if you haven't already
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

;; Configure tree-sitter (using :demand t to ensure it's loaded)
(use-package tree-sitter
  :straight t
  :demand t
  :config
  (global-tree-sitter-mode)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  ;; Install the Python tree-sitter grammar using straight.el
  (straight-use-package 'tree-sitter-langs)

  (tree-sitter-require 'python))

;; Language Server Protocol setup
(use-package lsp-mode
  :straight t
  :hook (python-mode . lsp)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-eldoc-enable nil))

;; Integrates the pyright LSP server
(use-package lsp-pyright
  :straight t
  :ensure t
  :defer t
  :hook (python-mode . lsp-deferred)
  :config
  (setq lsp-pyright-use-black t))  ;; Enable Black formatting

(require 'lsp-mode)

(use-package company
  :straight t
  :ensure t
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (global-company-mode t)
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets/python-mode"))
  (add-to-list 'company-backends 'company-yasnippet))

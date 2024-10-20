;;
;; todo
;;   keybindings for al
;;   quickmenu
;;   shell script for LSP server install

;;
;; emacs-dx python developer profile
;;
;; supports the following capabilities:
;;
;; | Capability             | Solution                                         | Supported for Python?                                             |  
;; |------------------------|--------------------------------------------------|-------------------------------------------------------------------|
;; | Syntax Checking        | lsp-mode                                         |                                                                   | 
;; | At-Point Documentation | lsp-mode                                         |                                                                   | 
;; | Diagnostic Annotations | lsp-mode                                         |                                                                   |
;; | Identifier Definitions | lsp-mode, via Xref                               | no xref configs needed, should work with lsp-mode automatically   | 
;; | Buffer Navigation      | lsp-mode, via IMenu                              |                                                                   |
;; | At-Point Completion    | lsp-mode                                         |                                                                   |
;; | Automatic Formatting   | lsp-mode                                         |                                                                   |  
;; | Code Completion        | lsp-mode, Company Mode                           |                                                                   |
;; | Code Insertion         | lsp-mode, lsp-snippets                           |                                                                   |
;; | At-Point Documentation | lsp-mode                                         |                                                                   |
;;
;; overall design
;;   lsp-mode serves as foundation - LSP client for emacs enables several different coding assist actions (diagnostics, completions, formatting, etc)
;;   pyright is the back end LSP server - lsp-mode serves as a client within emacs.
;;   company-mode was chosen for code completion. company-box enhances the company-mode visual presentation.
;;
;; what else was considered in this design, but ruled out?
;;   lsp-mode was chosen over eglot - it is considered more feature rich, and has higher adoption.
;;   flycheck was chosen over flymake - it is considered more modern and feature rich
;;   flycheck was eliminataed from the design - the lsp server SHOULD provide syntax checking without it - we can add it if needed
;;   lsp-snippets was chosen over yasnippet, as the pyright should be sufficient for code completion.
;;   eldoc is disabled in favor of lsp mode for documentation in the echo area. lsp-mode should be able to handle without eldoc.  
;;   treesiter is really cool, but is ruled out for this first implementation so we can get things stable first.



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


;; Use straight.el to install tree-sitter and lsp-mode
(straight-use-package 'tree-sitter)
(straight-use-package 'lsp-mode)


;; Configure tree-sitter (using :demand t to ensure it's loaded)
(use-package tree-sitter
  :straight t
  :demand t
  :config
  (global-tree-sitter-mode)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  ;; Install the Python tree-sitter grammar using straight.el
  (straight-use-package 'tree-sitter-langs)

  ;; Add the correct path to tree-sitter-load-path
  (add-to-list 'tree-sitter-load-path
               (expand-file-name "straight/repos/tree-sitter-langs/repos" user-emacs-directory))

  (tree-sitter-require 'python))


;; Language Server Protocol setup
(use-package lsp-mode
  :straight t  ;; Tell use-package to use straight.el
  :hook (python-mode . lsp)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-eldoc-enable nil))  ;; Disable lsp-mode's eldoc integration

;; Integrates the pyright LSP server
(use-package lsp-pyright
  :ensure t  ;; It's okay to keep :ensure t for lsp-pyright
  :defer t
  :hook (python-mode . lsp-deferred))


(require 'lsp-mode)  ;; Load lsp-mode

(use-package company
  :straight t  ;; Use straight.el for company
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

;; Black integration with lsp-mode
(use-package lsp-pyright
  :ensure t  ;; It's okay to keep :ensure t for lsp-pyright
  :defer t
  :hook (python-mode . lsp-deferred)
  :config
  (setq lsp-pyright-use-black t))  ;; Enable Black formatting

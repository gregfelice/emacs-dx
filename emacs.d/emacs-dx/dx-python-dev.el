(message "Loading Python Developer feature")

(use-package emacs
  :config
  ;; Treesitter config
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package eglot
  :ensure t
  :custom
  (eglot-send-changes-idle-time 0.2)
  (eglot-extend-to-xref t)
  (message "in eglot custom")
  :config
  (fset #'jsonrpc--log-event #'ignore)
  )

(use-package company
  :ensure t
  :hook (python-ts-mode . company-mode)  ;; Enable company-mode in python-mode
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :config
  (add-to-list 'company-backends 'company-yasnippet))  ;; Add company-yasnippet once


(use-package yasnippet
  :ensure t
  :hook (python-ts-mode . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets/python-mode")))

(require 'eglot)

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)


;;
;;     https://protesilaos.com/emacs/denote
;;
(use-package denote
  :ensure t
  :config
  (denote-rename-buffer-mode)
  (require 'denote-journal-extras))


(provide 'dx-python-dev)  ;; This defines the 'python-developer feature
(message "Loaded Python Developer feature")

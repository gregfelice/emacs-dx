(message "Loading Python Developer feature")

;; https://slinkp.com/python-emacs-lsp-20231229.html
;; https://github.com/emacs-lsp/lsp-treemacs
;; https://github.com/emacs-lsp/lsp-ui
;; https://gist.github.com/seanhagen/8421498ea363a2143afd5e73006266b0
;; https://emacs-lsp.github.io/dap-mode/page/python-poetry-pyenv/


(use-package highlight-indent-guides
  :hook
  (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  )

(use-package markdown-mode)

(use-package treemacs
  :bind
  (("C-c t" . treemacs)
   ("M-0" . treemacs-select-window))
  :config
  (setq treemacs-no-png-images t)
  (setq treemacs-indentation 2)
  (setq treemacs-width 30) ; Adjust the width of Treemacs window
  (setq treemacs-text-scale -1)
  (setq treemacs-show-hidden-files nil) ; Hide hidden files in Treemacs
  (setq treemacs-no-load-time-warnings t)
  (treemacs-fringe-indicator-mode 'only-when-focused)

  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter)
  )

(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        "bbl"
        "log"
        "blg"
        "bcf"
        "xml"
        "gz"
        "thm"
        "out"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))

(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))


(use-package expand-region
  :bind
  (
   ("C-=" . er/expand-region)
   ;;("C-s-SPC" . er/expand-region)
   ;; ("s-," . er/mark-LaTeX-math)
   ;; ("s-." . er/mark-LaTeX-inside-environment)
   ))

(use-package surround
  :bind-keymap ("C-c s" . surround-keymap))

;; (use-package smartparens
;;   :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
;;   :config
;;   ;; load default config
;;   (require 'smartparens-config))

(use-package nerd-icons)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq mode-line-right-align-edge 'right-fringe)
  )


;;(package-vc-install '(combobulate :url "https://github.com/mickeynp/combobulate"))
;; (use-package combobulate
;;   :preface
;;   (setq combobulate-key-prefix "C-c o")

;;   ;; :hook ((python-ts-mode . combobulate-mode)
;;   ;;        (js-ts-mode . combobulate-mode)
;;   ;;        (css-ts-mode . combobulate-mode)
;;   ;;        (yaml-ts-mode . combobulate-mode)
;;   ;;        (json-ts-mode . combobulate-mode)
;;   ;;        (typescript-ts-mode . combobulate-mode)
;;   ;;        (tsx-ts-mode . combobulate-mode))
;;   ;; ;; Amend this to the directory where you keep Combobulate's source
;;   ;; ;; code.
;;   ;; :load-path ("path-to-git-checkout-of-combobulate"))
;;   )

(use-package impatient-mode)

(defconst pw/jetbrains-ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" nil nil "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" nil "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" nil ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode pw/jetbrains-ligatures)
  (ligature-set-ligatures 'org-mode pw/jetbrains-ligatures)
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;; Python
;; ------

;; To conisder: py-isort, pyimport, python-pytest

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package python-mode
  :hook
  (python-base-mode . eglot-ensure)
  (python-base-mode . eldoc-mode)
  :bind
  (:map python-mode-map
        ("C-c e n" . flymake-goto-next-error)
        ("C-c e p" . flymake-goto-prev-error)
        )
  :init
  (setq python-indent-guess-indent-offset-verbose nil)

  :config
  (er/enable-mode-expansions 'python-ts-mode 'er/add-python-mode-expansions)
  )

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package jupyter
  :config
  (setq jupyter-repl-echo-eval-p t)
  )

(use-package code-cells
  :hook
  (python-mode . code-cells-mode-maybe)
  :bind
  (:map python-mode-map
        ("C-c C-e" . code-cells-eval)
        ("M-=" . code-cells-eval)
        ("M-<up>" . code-cells-backward-cell)
        ("M-<down>" . code-cells-forward-cell)
        ("M-+" . (lambda () (interactive)
                   (call-interactively 'code-cells-eval)
                   (call-interactively 'code-cells-forward-cell)))
        )
  )

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  )

;; (use-package numpydoc
;;   :bind
;;   (:map python-mode-map
;;         ("C-c C-n" . numpydoc-generate)
;;         )

;;   :init
;;   (setq numpydoc-insert-examples-block nil)
;;   (setq numpydoc-template-long nil)
;;   )

;; (use-package python-pytest)
;; (use-package python-isort)

;; (use-package pyvenv
;;   :bind
;;   (:map pyvenv-mode-map
;;         ("C-c p a" . pyvenv-activate)
;;         ("C-c p d" . pyvenv-deactivate)
;;         ("C-c p w" . pyvenv-workon)
;;         )

;;   :hook
;;   (python-mode . pyvenv-mode)
;;   (python-mode . pyvenv-tracking-mode)

;;   :init
;;   (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)
;;   (setq pyvenv-default-virtual-env-name "venv")
;;   )

;; (use-package anaconda-mode
;;   :hook
;;   (python-mode . anaconda-mode))

;; (use-package python-docstring
;;   :delight
;;   :hook (python-base-mode . python-docstring-mode))

;; (use-package python-insert-docstring
;;   :config
;;   (add-hook 'python-base-mode-hook
;;             (lambda ()
;;               (local-set-key
;;                (kbd "C-c I")
;;                'python-insert-docstring-with-google-style-at-point))))

;; (use-package sphinx-doc
;;   :delight
;;   :hook (python-base-mode . sphinx-doc-mode))






(provide 'dx-python-dev)  ;; This defines the 'python-developer feature
(message "Loaded Python Developer feature")



;;; General minibuffer settings


(use-package embrace
      :bind (:map TeX-mode-map
             ("M-s a" . embrace-add)
             ("M-s c" . embrace-change)
             ("M-s d" . embrace-delete)))

(use-package corfu
  :bind
  ("C-<tab>" . completion-at-point)
  :config
  (setq completion-cycle-threshold 1)
  (setq tab-always-indent 'complete)
  :init
  (global-corfu-mode)
  )

(use-package which-key
  :config
  (which-key-mode))

(use-package visual-fill-column)

(use-package popper
  :bind (("M-`"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc .+\\*"
          "\\*eldoc\\*"
          "\\*TeX Help\\*"
          help-mode
          compilation-mode))
  (popper-mode 1)
  ;; (popper-echo-mode -1)
  )

;; (use-package solaire-mode
;;   :init
;;   (solaire-global-mode +1)
;;   )

(use-package git-gutter-fringe
  :init
  (global-git-gutter-mode))


(use-package adaptive-wrap)

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package vertico
  :config
  (setq vertico-count 8)
  :init
  (vertico-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("s-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :bind (
         ("C-x b" . consult-buffer)
         ("s-m" . consult-imenu))
  )

(provide 'dx-ui)
(message "Loaded dx-ui")

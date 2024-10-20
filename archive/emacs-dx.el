
(require 'package)

;; Add package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

;; Prioritize archives (optional)
;; This example prioritizes GNU ELPA over MELPA
(setq package-archive-priorities '(("elpa" . 1)
                                   ("melpa" . 2))) 

;; Initialize the package system
(package-initialize)

;; Refresh the package list (optional but recommended)
(package-refresh-contents)



;; python developer support
(load-file "~/development/emacs-dx/profiles/python-developer/python-developer.el") 


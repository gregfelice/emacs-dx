
;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
	("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
	("melpa" . 2)
	("nongnu" . 1)))

(setopt inhibit-splash-screen t)                ; If you want to turn off the welcome screen

(setq find-file-visit-truename t)                  ; follow symlinks, don't ask

(defun std--backup-file-name (fpath)               ; Don't litter file system with *~ backup files
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'std--backup-file-name)

(use-package emacs
  :config
  (load-theme 'wombat t))

(add-to-list 'load-path "~/.emacs.d/emacs-dx/")
(require 'dx-ui)
(require 'dx-python-dev)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c"
     "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7"
     "058ba0ed929f801fc4077617e816797654c7775382943520875642d5507d8696"
     "877ef827500a2903a2415cec604c15259588aade629bd83f4cfab8703e2fde4b"
     "f2e885fc2a2d9bf2ca730058c2ebb835ba87e98df18ef9fc5c0f143a619d4c10"
     "621b6ed5126672e48eb261bbb86dd36bf1f584a9c34dc6e7b3d087e289c4f091"
     "4f1e4cadfd4f998cc23338246bae383a0d3a99a5edea9bcf26922ef054671299"
     "e7ce09ff7426c9a290d06531edc4934dd05d9ea29713f9aabff834217dbb08e4"
     "a898625737b309036b223f6667edea08b6d028ab1fe1654bc61542c1cd2ad053"
     "98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (or std--initial-gc-threshold 800000))

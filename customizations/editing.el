;;;;
;; editing.el - Whole editor configuration for editing customizations
;;;;

;; Downcase region is disabled by default but I find occasionaly useful
(put 'downcase-region 'disabled nil)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Enable rainbow editing for all programming
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Not sure why this is disabled
(electric-indent-mode nil)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Disable creating auto-save files, which clutter when I abandon edits
(setq auto-save-default nil)

;; For programming remove trailing whitespace from every line, and end of the file
(add-hook 'before-save-hook #'delete-trailing-whitespace)

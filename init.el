;;;;
;; init.el - main configuration file entry point for Emacs 28.1
;;;;

;; Location for specialized customization files
(defconst customizations-directory (concat user-emacs-directory "customizations/"))
(add-to-list 'load-path customizations-directory)

;; File where Emacs saves and restores Customizations made interactively
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file (concat customizations-directory "emacs-custom.el"))

;; Define package repositories, and packages pinned to repositories
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package system
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Load the saved interactive customizations system (Can contain package loads)
(load custom-file)

;; Load global user interface changes
(load "ui.el")

;; Load global navigation changes
(load "navigation.el")

;; Load global editing changes
(load "editing.el")

;; Load Clojure customizations
(load "setup-clojure.el")

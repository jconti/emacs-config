;;;;
;; init.el - main configuration file entry point for Emacs 28.1
;;;;

;; Define package repositories, and packages pinned to repositories
;; Customizations might load packages so do this first
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-pinned-packages
             '("cider" . "melpa-stable") t)

;; Define the location where all the customization files are found
;; there can be multiple sets used for different versions of emacs
(defconst customizations-directory (concat user-emacs-directory "customizations/"))

;; Set the file where Emacs can save and restore Customizations made interactively
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file (concat customizations-directory "emacs-custom.el"))

;; Load the saved state of the Emacs customizations system
;; They can contain package definitions needed first
(load custom-file)

;; Automatically load files from the customizations-dir
(add-to-list 'load-path customizations-directory)

;; Load global user interface changes
(load "ui.el")

;; Load global navigation changes
(load "navigation.el")

;; Load global editing changes
(load "editing.el")

;; Load Clojure customizations
(load "setup-clojure.el")

;; Load SQL connection defs
(load "setup-sql.el")


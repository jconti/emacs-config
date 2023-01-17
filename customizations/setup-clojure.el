;;;;
;; setup-clojure.el - customizations for the development of Clojure code
;;;;

;; Enable paredit mode for Clojure development
(add-hook 'clojure-mode-hook 'paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; The minor mode electric-indent-mode interferes with paredit, this code
;; was suggested https://stackoverflow.com/questions/21182550
(add-hook 'clojure-mode-hook (lambda () (electric-indent-local-mode -1)))

;;;;
;; Cider
;;;;

;; Workaround for RET not running code in Cider's REPL
;; See https://paredit.org/cgit/paredit/plain/NEWS
(add-hook 'cider-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "RET") nil)
            (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)()
            (electric-indent-local-mode -1)))

;; Turn off inspiration message at start of REPL session
(setq cider-connection-message-fn nil)
(setq cider-repl-display-help-banner nil)

;; change the default prefix to add more space for readibility
(setq cider-eval-result-prefix " ;;  => ")

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

;; A function to go back to the user namespace
(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

;; Customize Cider keymap
(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)))

;; Run cljfmt on each file before save
(add-hook 'before-save-hook 'cider-format-buffer t t)

;;;;
;; JDK Selection
;;;;

;; Inspiration from https://blog.brunobonacci.com/2020/07/02/switching-between-multiple-jdk-in-emacs/

;; location where Intel arch homebrew installs Java
(setq JAVA_BASE "/usr/local/opt")

(defun java-home--versions ()
  "Return the list of installed JDKs."
  (directory-files JAVA_BASE t "openjdk@[[:digit:]]+"))

(defun java-home--switch ()
  "List installed JDKs and switch JAVA_HOME to the one chosen."
  (interactive)
  (let ((ver-path (completing-read
                   "Which JDK: "
                   (seq-map-indexed
                    (lambda (e i) (list e i)) (java-home--versions))
                   nil t "")))
    (setenv "JAVA_HOME" ver-path)))

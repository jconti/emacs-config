;;;;
;; setup-clojure.el - customizations for the development of Clojure code
;;;;

;; add environment variable to slow down lein ancient timeouts for artifactory
;; uncertain if this works and what the reference is, does it also affect clj tools wagon?
(setenv "http_timeout" "20000")

;;;;
;; JDK Selection
;;;;

;; Inspiration from https://blog.brunobonacci.com/2020/07/02/switching-between-multiple-jdk-in-emacs/

;; location where Intel arch homebrew installs Java
(setq JAVA_BASE "/opt/homebrew/opt")

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

;; default to the oldest JVM
(setenv "JAVA_HOME" (car (java-home--versions)))

;; Should select 20221127.1452 version from melpa
(use-package paredit
  :ensure t
  :pin melpa)

;; provisional setup
(use-package lsp-mode
  :defer t
  :hook ((clojure-mode  . lsp))
  :bind (:map lsp-mode-map
              ("C-M-." . lsp-find-references))
  :config

  ;; use cider for symbol completion
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/#completion
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-enable-xref t))

;; I get the following warning, separate clj vs. cljs configs might be needed or useful?
;;
;;;  [WARNING] Something in your configuration activated ‘clojure-mode’ instead of ‘clojurescript-mode’ in this buffer.
;;;  This could cause problems.
;;;  (See ‘clojure-verify-major-mode’ to disable this message.)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj$"  . clojure-mode)
         ("\\.edn$"  . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurescript-mode))
  :config


  ;; Cloudpermit standard format settings, more at
  ;; https://github.com/cloudpermit/vaahtera/blob/develop/docs/editors.md#emacs-cider
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-binding-forms
        '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
          "doseq" "for" "with-open" "with-local-vars" "with-redefs"
          "r/with-let" "p/if-all-let" "test-seq/seq-tx"))
  (setq singly-indented-functions '(testit/fact
                                    testit/facts
                                    facts
                                    fact
                                    testit.core/fact
                                    page/html5
                                    rf/reg-event-fx
                                    chain/reg-chain
                                    rf/reg-sub
                                    rf/reg-event-db
                                    rc/reg-chain
                                    futil/for-all
                                    futil/for-frag
                                    for-frag
                                    for-all
                                    u/for-all
                                    not-join
                                    r/with-let
                                    p/if-all-let
                                    test-seq/seq-tx
                                    or-join))
  (dolist (expr singly-indented-functions)
    (put-clojure-indent expr 1))

  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (add-hook 'clojure-mode-hook 'subword-mode)

  ;; Enable paredit mode for Clojure development
  (add-hook 'clojure-mode-hook 'paredit-mode)

  ;; The minor mode electric-indent-mode interferes with paredit, this code
  ;; was suggested https://stackoverflow.com/questions/21182550
  (add-hook 'clojure-mode-hook (lambda () (electric-indent-local-mode -1)))

  ;; this is new, does it work, I think I wanted this before!
  (add-hook 'clojure-mode-hook #'eldoc-mode))

;;;;
;; Cider
;;;;

(use-package cider
  :ensure t
  :config

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
  ;(setq cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; enable paredit in your REPL
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

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

  ;; vaahtera path in one profile does not work for both clj and cljs
  ;; so repls must be started seperately and this joins them together
  ;; See https://github.com/cloudpermit/vaahtera/blob/77b5645a5eb2810aae982a80bcf08bab783641a9/docs/editors.md#emacs-cider
  (setq cider-merge-sessions 'host)

  ;; When I do cider-jack-in-cljs as the above page advises it complains that there is already another session with the same
  ;; connection parameters and it suggests I use cider-connect-sibling-cljs
  ;; If I chose 'y' to connect anyway, I need to then select `shadow-cljs` and not `lein` to get it to start

  ;; set the internal request timeout for evaluation
  (setq nrepl-sync-request-timeout 30))

;;;;
;; Copilot
;;;;

;(use-package copilot-chat)

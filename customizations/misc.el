;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Load asciidoc all the time
(require 'adoc-mode)

;; Load live-code-talks.el
(require 'live-code-talks)

;; markdown-mode settings
(setq markdown-command "grip --user=jconti --pass5075fbd43526fcba47fe08e93c1bd1621938bbf8 --export -")

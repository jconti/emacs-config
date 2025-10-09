;;;;
;; setup-plantuml.el - plantuml mode with local preview
;;;;

;; uses https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
	:ensure t
	:mode (("\\.puml$" . plantuml-mode))
	:config

        ;; use env var in future? HOMEBREW_CELLAR=/opt/homebrew/Cellar
        (setq plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2025.7/libexec/plantuml.jar")
        (setq plantuml-default-exec-mode 'jar))

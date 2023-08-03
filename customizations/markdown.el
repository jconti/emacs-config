;;;;
;; markdown.el - customizations for markdown mode with local preview
;;;;

;; uses https://github.com/markedjs/marked
(use-package markdown-mode
	:ensure t
	:mode ("\\.md$'" . gfm-mode)
	:config (setq markdown-command "marked --no-mangle --no-headerIds --gfm"))

;;;;
;; markdown.el - customizations for markdown mode with local preview using marked
;;;;

(use-package markdown-mode
	:ensure t
	:mode ("\\.md$'" . gfm-mode)
	:config (setq markdown-command "marked"))

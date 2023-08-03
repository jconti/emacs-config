;;;;
;; shell-integration.el - handles unusual issues with environment on Macos
;;;;

(use-package exec-path-from-shell
  :ensure t
  :config
  ;; https://github.com/purcell/exec-path-from-shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH")))
  )

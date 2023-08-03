;;;;
;; emacs-custom.el - customizations set with the editor's customization system
;;;;

;; Single expression which allows the editor to alter customizations
;; Since packages can be added, needs to get loaded early in init sequence
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages
   '(markdown-mode cider clojure-mode paredit rainbow-delimiters smex ido-completing-read+ use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

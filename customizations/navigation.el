;;;;
;; navigation.el - customize the way files, buffers and other navigation is done
;;;;

;; browse-at-remote enables browsing git forge sites (like github, gitlab, etc.)
;; (require 'browse-at-remote)
;; (global-set-key (kbd "C-c g g") 'browse-at-remote)

;; Turn on IDO mode
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
(use-package ido

	     ;; Built in package, do not install
	     :ensure nil

	     :init
	     ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
	     (setq ido-enable-flex-matching t)

	     ;; Turn this behavior off because it's annoying
	     (setq ido-use-filename-at-point nil)

	     ;; Don't try to match file across all "work" directories; only match files
	     ;; in the current directory displayed in the minibuffer
	     (setq ido-auto-merge-work-directories-length -1)

	     :config
	     ;; Configure IDO for all buffer and file reading
	     (ido-mode t)
	     (ido-everywhere 1))


(use-package ido-completing-read+
	     :config
	     (ido-ubiquitous-mode 1))

;; Provide a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
	     :init
	     (setq smex-save-file (concat user-emacs-directory ".smex-items"))
	     :config
	     (smex-initialize)
	     (global-set-key (kbd "M-x") 'smex))

;; Create a command that puts the output of shell commands into diff-mode
;; https://emacs.stackexchange.com/questions/20734/how-to-properly-capture-diff-output-in-buffer-without-first-opening-a-file
;; A different recipe that might be useful too is https://stackoverflow.com/questions/47997194/emacs-elisp-switch-to-buffer-and-follow
(defun diff-generic(command)
  (interactive "sCommand: ")
  (let ((buffer (generate-new-buffer "*diff-generic*")))
    (with-current-buffer buffer (diff-mode))
    (call-process-shell-command command nil buffer 0)
    (switch-to-buffer buffer)))

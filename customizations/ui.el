;;;;
;; ui.el - configuration of UI elements of the whole editor
;;;;

;; Turn off the graphical tool bar
;; https://superuser.com/questions/127420/how-can-i-hide-the-tool-bar-in-emacs-persistently
(if (display-graphic-p nil)
    (tool-bar-mode -1))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Overriding image.el function image-type-available-p for 28.X
;; https://emacs.stackexchange.com/questions/74289
(defun image-type-available-p (type)
  "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (if (eq 'svg type)
      nil
    (and (fboundp 'init-image-library)
         (init-image-library type))))

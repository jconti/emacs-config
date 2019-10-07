;;; live-code-talks.el --- Support for slides with live code in them  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: docs, multimedia
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (narrowed-page-navigation "0.1"))
;; Version: 0.2.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode for formatting an Emacs buffer
;; as slides. This package relies on `narrowed-page-navigation-mode'
;; to actually navigate from slide to slide, and instead provides
;; syntax for comments that are rendered as slide elements.
;;
;; The syntax comes pre-configured for Idris or Haskell. For other
;; languages, set `live-code-talks-title-regexp',
;; `live-code-talks-image-regexp', and
;; `live-code-talks-comment-regexp', preferably as file variables.
;; For your presentation, consider also overriding
;; `face-remapping-alist' to get the proper fonts for your screen.
;;; Code:

(require 'cl-lib)
(require 'linum)
(require 'narrowed-page-navigation)

(defgroup live-code-talks ()
  "Settings for live code talks"
  :group 'multimedia)

(defface live-code-talks-title-face
  '((t (:height 2.0 :foreground "green" :weight bold)))
  "Face for showing slide titles"
  :group 'live-code-talks)

(defvar live-code-talks-title-bullet "")
(make-variable-buffer-local 'live-code-talks-title-bullet)

;; "green2"
(defface live-code-talks-subtitle-face
  '((t (:inherit live-code-talks-title-face
                 :height 0.75 :foreground "khaki2" :weight normal)))
  "Face for showing slide titles"
  :group 'live-code-talks)

(defvar live-code-talks-subtitle-bullet " => ")
(make-variable-buffer-local 'live-code-talks-subtitle-bullet)

;; "green3"
(defface live-code-talks-subsubtitle-face
  '((t (:inherit live-code-talks-subtitle-face
                 :height 0.8 :foreground "goldenrod" :slant normal)))
  "Face for showing slide titles"
  :group 'live-code-talks)

(defvar live-code-talks-subsubtitle-bullet "  ~ ")
(make-variable-buffer-local 'live-code-talks-subsubtitle-bullet)

(defface live-code-talks-button-face
  '((t (:inherit custom-button
                 :slant normal
                 :foreground "black")))
  "Face for clickable buttons in presentations"
  :group 'live-code-talks)

(defface live-code-talks-comment-face
  '((t (:height 0.9 :foreground "DarkOliveGreen3")))
  "Face used for stripped-out comments"
  :group 'live-code-talks)

(defvar live-code-talks-comment-regexp
  "^\\s-*\\;+\\(.*\\)$"
  "The regexp for non-title comments. Match group 1 will be highlighted.")
(make-variable-buffer-local 'live-code-talks-comment-regexp)

(defvar live-code-talks-title-regexp "^\\s-*\\;\\;\\s-*#\\([^#].*\\)$"
  "The regexp to match for slide titles.  The contents of match group 1 will be highlighted.")
(make-variable-buffer-local 'live-code-talks-title-regexp)

(defvar live-code-talks-subtitle-regexp "^\\s-*\\;\\;\\s-*##\\s-*\\([^#].*\\)$"
  "The regexp to match for slide subtitles.  The contents of match group 1 will be highlighted.")
(make-variable-buffer-local 'live-code-talks-title-regexp)

(defvar live-code-talks-subsubtitle-regexp "^\\s-*\\;\\;\\s-*###\\s-*\\([^#].*\\)$"
  "The regexp to match for slide subsubtitles.  The contents of match group 1 will be highlighted.")
(make-variable-buffer-local 'live-code-talks-title-regexp)

(defun live-code-talks-highlight-titles (regexp face bullet &optional buffer)
  "Use REGEXP to find titles, and highlight them with FACE, placing highlighting on all titles in BUFFER, or the current buffer if nil. To change the format used for titles, set `live-code-talks-title-regexp'."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let ((comment-area-overlay (make-overlay (match-beginning 0)
                                                    (match-beginning 1)))
                (title-overlay (make-overlay (match-beginning 1)
                                             (match-end 1))))
            (overlay-put title-overlay 'live-code-talks 'title)
            (overlay-put title-overlay 'face face)
            (overlay-put title-overlay 'display t)
            (overlay-put comment-area-overlay 'live-code-talks 'title)
            (overlay-put comment-area-overlay 'face face)
            (overlay-put comment-area-overlay 'display bullet)
            ;(overlay-put comment-area-overlay 'invisible t)
            ))))))

(defun live-code-talks-unhighlight (what &optional buffer)
  "Delete all WHAT highlighting in BUFFER, or the current buffer if nil.

 WHAT can be `title', `image', `comment', `button' or `hidden'."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-excursion
        (let ((overlays (overlays-in (point-min) (point-max))))
          (cl-loop for overlay in overlays
                   when (eq (overlay-get overlay 'live-code-talks) what)
                   do (delete-overlay overlay)))))))

(defvar live-code-talks-image-regexp
  "^\\s-*\\;\\;\\s-*\\[\\[\\[\\([^]]+\\)\\]\\]\\]\\s-*$"
  "A regexp to determine which images should be shown.  Group 1 should be an image specification, which will be made relative to the current buffer.")


(defun live-code-talks-show-images (&optional buffer)
  "Replace images matching `live-code-talks-image-regexp' with the actual image in BUFFER, or the current buffer if nil."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward live-code-talks-image-regexp nil t)
          (let* ((image-fname (read (match-string 1)))
                 (image (create-image image-fname))
                 (overlay (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put overlay 'display image)
            (overlay-put overlay 'live-code-talks 'image)))))))

(defun live-code-talks-highlight-comments (&optional buffer)
  "Place highlighting on normal comments in BUFFER, or the current buffer if nil.  To change the format used for comments, set `live-code-talks-comment-regexp'."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward live-code-talks-comment-regexp nil t)
          (let ((comment-overlay (make-overlay (match-beginning 1)
                                               (match-end 1))))
            (overlay-put comment-overlay 'live-code-talks 'comment)
            (overlay-put comment-overlay 'face 'live-code-talks-comment-face)
            (overlay-put comment-overlay 'display t)))))))

(defvar live-code-talks-begin-hide-regexp "\\;\\; *{hide} *"
  "Regexp beginning regions that should be invisible in slide mode.")

(defvar live-code-talks-end-hide-regexp "\\;\\; *{show} *"
  "Regexp ending regions that should be invisible in slide mode.")

(defun live-code-talks-hide-junk (&optional buffer)
  "Don't display hidden regions in BUFFER, or current buffer if nil."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let (hide-start hide-end overlay)
          (while (re-search-forward live-code-talks-begin-hide-regexp nil t)
            (setq hide-start (match-beginning 0))
            (when (re-search-forward live-code-talks-end-hide-regexp nil t)
              (setq hide-end (match-end 0))
              (setq overlay (make-overlay hide-start hide-end))
              (overlay-put overlay 'live-code-talks 'hidden)
              (overlay-put overlay 'display "")
              (overlay-put overlay 'priority 10))))))))

(defun live-code-talks-in-comment-p (&optional pos)
  "Determine whether POS is in a comment or not."
  (save-excursion (nth 4 (syntax-ppss pos))))

(defvar live-code-talks-button-regexp "{{{\\(.+\\)|||\\(.+\\)}}}"
  "Regexp describing how to find clickable buttons. Matching
group 1 contains the button text and matching group 2 contains
the Lisp expresion to evaluate.")

(defun live-code-talks-make-buttons (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward live-code-talks-button-regexp nil t)
          (when (and (live-code-talks-in-comment-p (match-beginning 0))
                     (live-code-talks-in-comment-p (match-end 0)))
            (let* ((text (match-string 1))
                   (expr (match-string 2))
                   (button (make-button (match-beginning 0) (match-end 0)
                                        'action (read expr)
                                        'display (progn (set-text-properties 0 (length text) nil text)
                                                        text)
                                        'face 'live-code-talks-button-face)))
              (overlay-put button 'priority 10)
              (overlay-put button 'live-code-talks 'button))))))))



(defvar live-code-talks-restore-linum nil
  "Whether to re-enable linum on exit from slide mode.")
(make-variable-buffer-local 'live-code-talks-restore-linum)

;;;###autoload
(define-minor-mode live-code-talks-mode
  "A minor mode for presenting a code buffer as slides."
  nil "Talk" nil
  (if live-code-talks-mode
      (progn
        (setq live-code-talks-restore-linum linum-mode)
        (linum-mode -1)
        (live-code-talks-highlight-titles live-code-talks-title-regexp
                                          'live-code-talks-title-face
                                          live-code-talks-title-bullet)
        (live-code-talks-highlight-titles live-code-talks-subtitle-regexp
                                          'live-code-talks-subtitle-face
                                          live-code-talks-subtitle-bullet)
        (live-code-talks-highlight-titles live-code-talks-subsubtitle-regexp
                                          'live-code-talks-subsubtitle-face
                                          live-code-talks-subsubtitle-bullet)
        (live-code-talks-show-images)
        (live-code-talks-hide-junk)
        (live-code-talks-highlight-comments)
        (live-code-talks-make-buttons)
        (narrow-to-page)
        (narrowed-page-navigation-mode 1))
    (progn
      (when live-code-talks-restore-linum (linum-mode 1))
      (widen)
      (narrowed-page-navigation-mode -1)
      (remove-images (point-min) (point-max))
      (cl-loop for what in '(title image comment hidden button)
               do (live-code-talks-unhighlight what)))))


(provide 'live-code-talks)
;;; live-code-talks.el ends here

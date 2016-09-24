;;; DEMO-IT-PRESENT --- Summary
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created: 23 September 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Functions for displaying an org-mode file as a presentation using
;;  the `org-tree-slide' project (see ).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Starting an ORG Presentation
;;
;;    Since I often have an org-mode file on the side of the screen to
;;    demonstrate an outline of what I will be demoing, I made it a
;;    function.

;;    Uses org-tree-slide if available.
;;    See https://github.com/takaxp/org-tree-slide

(defvar demo-it--presentation-file "")
(defvar demo-it--presentation-buffer nil)
(defvar demo-it--presentation-prev-settings (make-hash-table))

(defun demo-it-presentation (file &optional size style section)
  "Load FILE (org-mode?) as presentation.  Start org-tree-slide
if available.  SIZE specifies the text scale, and defaults to 2
steps larger. STYLE can either be :variable for variable pitch of
the font, :blocks for diminished headers on org-blocks, or :both
for both features.

The SECTION is the name of an org-mode header to specify as the
first section to display."
  (find-file file)
  (setq demo-it--presentation-file file)
  (setq demo-it--presentation-buffer (buffer-name))

  (when (fboundp 'org-tree-slide-mode)
    (setq org-tree-slide-heading-emphasis t)
    (org-tree-slide-mode)
    (when section
      (demo-it--presentation-section section)))

  (when (fboundp 'flyspell-mode)
    (flyspell-mode -1))
  (setq cursor-type nil)

  ;; Style things up correctly...
  (make-local-variable 'demo-it--presentation-prev-settings)
  (puthash :emphasis-markers org-hide-emphasis-markers demo-it--presentation-prev-settings)
  (setq org-hide-emphasis-markers t)

  (when (or (eq style :variable) (eq style :both))
    (variable-pitch-mode 1))

  ;; Make the display of the org-mode file more presentable
  (when (or (eq style :block) (eq style :both))
    (demo-it--presentation-display-set))

  (demo-it-hide-mode-line)
  (if size (text-scale-set size)
    (text-scale-set 2))

  (when (fboundp 'org-bullets-mode)
    (org-bullets-mode 1)))

(defun demo-it--presentation-display-set ()
  "Change some typical `org-mode' display values to make more
presentation-friendly.  Store the changed values in a hashtable.
See `demo-it--presentation-display-restore'."
  ;; Save everything that is interesting into a hash table:
  (puthash :restore t demo-it--presentation-prev-settings)
  ;; Allow us to resize our images:
  (setq org-image-actual-width nil)
  (let* ((backgd (face-attribute 'default :background))
         (border (list (list :foreground backgd :background backgd :height 1))))
    (cl-flet ((set-attr (attr values) (puthash attr
                                               (face-remap-add-relative attr values)
                                               demo-it--presentation-prev-settings)))
      (set-attr 'org-block-begin-line border)
      (set-attr 'org-block-end-line   border)
      (set-attr 'org-meta-line        border)
      (set-attr 'org-special-keyword  border)
      (set-attr 'org-block            '((:family "monospace")))
      (set-attr 'org-verbatim         '((:family "monospace")))
      (set-attr 'org-code             '((:family "monospace")))
      (set-attr 'org-table            '((:family "monospace")))
      (set-attr 'org-special-keyword  '((:family "monospace"))))))

(defun demo-it--presentation-display-restore ()
  "After `demo-it--presentation-display-set', call to restore previous settings."
  (setq org-hide-emphasis-markers
        (gethash :emphasis-markers demo-it--presentation-prev-settings))
  (when (gethash :restore demo-it--presentation-prev-settings)
    (remhash :restore demo-it--presentation-prev-settings)
    (cl-flet ((rest-attr (attr) (face-remap-remove-relative
                                 (gethash attr demo-it--presentation-prev-settings))))
      (mapcar #'rest-attr (list 'org-block-begin-line 'org-block-end-line 'org-block 'org-meta-line
                                'org-verbatim 'org-code 'org-table 'org-special-keyword)))))

;; Specify a section in the presentation

(defun demo-it--presentation-section (section)
  "Moves the displayed presentation to a SECTION header."
  (interactive "s")
  (when demo-it--presentation-buffer
    (switch-to-buffer demo-it--presentation-buffer)
    (org-tree-slide-content)
    (goto-char (point-min))
    (re-search-forward (format "^\*+ +%s" section))
    (org-tree-slide-move-next-tree)))

;; Jumping Back to the Presentation
;;
;;    In this case, we've been doing some steps, and the screen is
;;    "messed up", calling this function returns back to the
;;    presentation.

(defun demo-it-presentation-return-noadvance ()
  "Return to the presentation buffer and delete other windows."
  (when demo-it--presentation-buffer
    (switch-to-buffer demo-it--presentation-buffer))
  (delete-other-windows))

(defun demo-it-presentation-return ()
  "Return to the presentation buffer, delete other windows, and advance to the next 'org-mode' section."
  (when demo-it--presentation-buffer
    (demo-it-presentation-return-noadvance)
    (when (fboundp 'org-tree-slide-move-next-tree)
      (org-tree-slide-move-next-tree))))

(defun demo-it-single-presentation (file &optional size style section)
  "Demonstration that presents on `org-mode' FILE as a full-screen presentation."
  (interactive "fPresentation File: ")
  (cl-flet ((present-it ()
                        (demo-it-frame-fullscreen)
                        (delete-other-windows)
                        (demo-it-presentation file size style section)))
    (demo-it-start (list #'present-it) t)))

;; Advance Presentation without Changing Focus
;;
;;    Advances the org-mode presentation, but after popping into that
;;    presentation buffer, returns to the window where our focus was
;;    initially.

(defun demo-it-presentation-advance ()
  "Advance the presentation to the next frame (if the buffer is an 'org-mode' and 'org-tree-slide' is available), but doesn't change focus or other windows.  Only useful if using the org-tree-slide mode for the presentation buffer."
  (interactive)
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer)))
      (switch-to-buffer demo-it--presentation-buffer)
      (when (fboundp 'org-tree-slide-move-next-tree)
        (org-tree-slide-move-next-tree))
      (switch-to-buffer orig-window))))

(defun demo-it--presentation-highlight-phrase (phrase &optional color)
  "Highlight a PHRASE (based on a regular expression) in the
presentation buffer. This is useful to highlight bullet point
items while executing appropriate code."
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer))
          (hilite-color (if (null color) 'hi-green-b color)))
      (switch-to-buffer demo-it--presentation-buffer)
      (hi-lock-unface-buffer t)
      (hi-lock-face-phrase-buffer phrase hilite-color)
      (switch-to-buffer orig-window))))

(defun demo-it--presentation-unhighlight-all ()
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer)))
      (switch-to-buffer demo-it--presentation-buffer)
      (hi-lock-unface-buffer t)
      (switch-to-buffer orig-window))))

;; Clean up the Presentation
;;
;;    The org-presentation-start function alters the way an org-mode file
;;    is displayed. This function returns it back to a normal, editable
;;    state.

(defun demo-it-presentation-quit ()
  "Undo display settings made to the presentation buffer."
  (interactive)
  (demo-it--setq-restore)
  (when demo-it--presentation-buffer
    (switch-to-buffer demo-it--presentation-buffer)
    (when (fboundp 'org-tree-slide-mode)
      (org-tree-slide-mode -1))
    (when (fboundp 'flyspell-mode)
      (flyspell-mode t))
    (setq cursor-type t)
    (demo-it--presentation-display-restore)  ; Restore previous changes
    (variable-pitch-mode nil)
    (demo-it-show-mode-line)
    (text-scale-set 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-present.el ends here

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
;;    Since I often have an org-mode file on the side of the screen to
;;    demonstrate an outline of what I will be demoing, I made it a
;;    function.
;;
;;    The file contains functions for displaying an org-mode file as a
;;    presentation using the `org-tree-slide' project (see
;;    https://github.com/takaxp/org-tree-slide).
;;
;;    If `org-tree-slide' is not available, it simply shows the
;;    org-mode file. We really need to look at a way to make the
;;    display configurable.
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

(require 'cl)

(declare-function org-tree-slide-content "ext:org-tree-slide")
(declare-function org-tree-slide-move-next-tree "ext:org-tree-slide")
(declare-function face-remap-add-relative "face-remap.el")
(declare-function face-remap-remove-relative "face-remap.el")

(defvar org-image-actual-width)
(defvar org-tree-slide-heading-emphasis)
(defvar org-hide-emphasis-markers)

(defvar demo-it--text-scale)
(defvar demo-it--presentation-hide-mode-line)
(defvar demo-it--presentation-hide-org-markers)
(defvar demo-it--presentation-variable-width)
(defvar demo-it--presentation-hide-org-blocks)

(declare-function demo-it--get-text-scale "demo-it-custom.el")
(declare-function demo-it--presentation-variable-width-p "demo-it-custom.el")
(declare-function demo-it--presentation-hide-blocks-p "demo-it-custom.el")
(declare-function demo-it-hide-mode-line "demo-it.el")
(declare-function demo-it-show-mode-line "demo-it.el")
(declare-function demo-it-disable-mode "demo-it.el")

(declare-function demo-it-create "demo-it.el")
(declare-function demo-it-start "demo-it.el")
(declare-function demo-it--setq-restore "demo-it.el")

;; ----------------------------------------------------------------------

(defvar demo-it--presentation-file "")
(defvar demo-it--presentation-buffer nil)
(defvar demo-it--presentation-prev-settings (make-hash-table))


(defun demo-it-presentation (file &optional size style section)
  "Load FILE (org-mode?) as presentation.  Start `org-tree-slide'
if available.  SIZE specifies the text scale, and defaults to the
value set in `demo-it--text-scale'.

 STYLE can either be `:variable' for variable font pitch,
`:blocks' for diminished headers on org-blocks, or `:both' for
both features. If nil, defaults to the customize variables,
`demo-it--presentation-variable-width' and
`demo-it--presentation-hide-org-blocks'.

The SECTION is the name of an org-mode header to specify as the
first section to display."
  (find-file file)
  (setq demo-it--presentation-file file)
  (setq demo-it--presentation-buffer (buffer-name))

  (when (fboundp 'org-tree-slide-mode)
    (setq org-tree-slide-heading-emphasis t)
    (org-tree-slide-mode))

  (when section
    (demo-it--presentation-section section))

  ;; Style things up correctly...
  (make-local-variable 'demo-it--presentation-prev-settings)
  (puthash :emphasis-markers org-hide-emphasis-markers demo-it--presentation-prev-settings)
  (setq org-hide-emphasis-markers demo-it--presentation-hide-org-markers)

  ;; Make the display of the org-mode file more presentable
  (when (demo-it--presentation-hide-blocks-p style)
    (demo-it--presentation-display-set))
  (when (demo-it--presentation-variable-width-p style)
    (variable-pitch-mode 1))
  (when demo-it--presentation-hide-mode-line
    (demo-it-hide-mode-line))
  (text-scale-set (demo-it--get-text-scale size))

  (when (fboundp 'org-bullets-mode)
    (org-bullets-mode 1))
  (when (fboundp 'flyspell-mode)
    (flyspell-mode -1))
  (setq cursor-type nil))

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
    (cl-flet ((set-attr (attr values)
                        (puthash attr
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

    (when (fboundp 'org-tree-slide-mode)
      (org-tree-slide-content))

    (goto-char (point-min))
    (re-search-forward (format "^\*+ +%s" section))

    (when (fboundp 'org-tree-slide-mode)
      (org-tree-slide-move-next-tree))))

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
  "Return to the presentation buffer, delete other windows, and
advance to the next 'org-mode' section."
  (when demo-it--presentation-buffer
    (demo-it-presentation-return-noadvance)
    (when (fboundp 'org-tree-slide-move-next-tree)
      (org-tree-slide-move-next-tree))))

;; Advance Presentation without Changing Focus
;;
;;    Advances the org-mode presentation, but after popping into that
;;    presentation buffer, returns to the window where our focus was
;;    initially.

(defun demo-it-presentation-advance ()
  "Advance the presentation to the next frame (if the buffer is
an `org-mode' and `org-tree-slide' is available), but doesn't
change focus to the window."
  (interactive)
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer)))
      (switch-to-buffer demo-it--presentation-buffer)
      (when (fboundp 'org-tree-slide-move-next-tree)
        (org-tree-slide-move-next-tree))
      (switch-to-buffer orig-window))))

(defun demo-it-presentation-highlight-phrase (phrase &optional color)
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

(define-obsolete-function-alias 'demo-it--presentation-highlight-phrase
  'demo-it-presentation-highlight-phrase "2016-Oct")

(defun demo-it-presentation-unhighlight-all ()
  "Remove all highlighted values in a presentation previously set
by a call to `demo-it-presentation-highlight-phrase'."
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer)))
      (switch-to-buffer demo-it--presentation-buffer)
      (hi-lock-unface-buffer t)
      (switch-to-buffer orig-window))))

(define-obsolete-function-alias 'demo-it--presentation-unhighlight-all
  'demo-it-presentation-unhighlight-all "2016-Oct")

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
    (variable-pitch-mode 0)
    (demo-it-show-mode-line)
    (text-scale-set 0)))

;; ----------------------------------------------------------------------

;;;###autoload
(defun demo-it-single-presentation (file &optional size style section)
  "Demonstration that presents an `org-mode' FILE as a
full-screen presentation. SIZE is the text scaling size, and STYLE is the presentation "
  (interactive "fPresentation File: ")
  (demo-it-create (demo-it-presentation file size style section))
  (demo-it-start)

  ;; Now that the presentation is going, let's change the mode:
  (demo-it-disable-mode)
  (demo-it-mode-pres t))

(define-minor-mode demo-it-mode-pres "Pressing 'space' advances demo."
  :lighter " demo-p"
  :require 'demo-it
  :global t
  :keymap '((" "               . demo-it-presentation-advance)
            (""              . demo-it-presentation-advance)
            ("q"               . demo-it-disable-mode)
            ("Q"               . demo-it-end)))

(provide 'demo-it-present)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-present.el ends here

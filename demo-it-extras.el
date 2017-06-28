;;; DEMO-IT-EXTRAS --- Demo Functions that depend on other packages
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created: 23 September 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  A collection of functions that depend on other packages available
;;  from MELPA. While the functions check to see if the package is
;;  available and loaded, it does not actually do the loading (or the
;;  installing of the package).
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

(declare-function demo-it--get-section "demo-it")
(declare-function demo-it-load-file "demo-it")

(declare-function fancy-narrow-to-region "ext:fancy-narrow")
(declare-function fancy-narrow-to-defun "ext:fancy-narrow")

;; ----------------------------------------------------------------------
;; FANCY HIGHLIGHTING
;;
;;   A function useful for interactive work or as part of a
;;   demonstration script, can use the tres cool highlighting feature
;;   of `fancy-narrow' (see https://github.com/Malabarba/fancy-narrow)

(defun demo-it-highlight-dwim (&optional type-or-fn start end)
  "Highlights or narrows to a particular 'section'.

If TYPE-OR-FN is a string, it specifies the name of a function to
highlight. If it is :line, then START and END specifies the
beginning or ending lines. If TYPE-OR-FN is :char, then START and
END are buffer positions. Finally, if TYPE-OR-FN is :regex, then
START and END are regular expressions that refers to a match in
the buffer.

If TYPE-OR-FN is `nil', (or running interactively), then if a
section is currently highlighted/narrowed, it is
unhighlighted/widened. If the region is active, use, the region,
otherwise, select the function the point is currently in.

If the `fancy-narrow' package is installed, we'll call
`fancy-narrow-to-region' or `fancy-narrow-to-defun', otherwise,
we narrow to it."
  (interactive)

  (cl-flet ((hi-defun ()   ;; Highlight the current function
                      (if (fboundp 'fancy-narrow-to-defun)
                          (fancy-narrow-to-defun)
                        (narrow-to-defun)))

            (hi-region (start end)
                       (if (fboundp 'fancy-narrow-to-region)
                           (fancy-narrow-to-region start end)
                         (narrow-to-region start end))
                       (if (region-active-p)
                           (deactivate-mark)))
            (active-p ()
                      (if (fboundp 'fancy-narrow-active-p)
                          (fancy-narrow-active-p)
                        (buffer-narrowed-p)))
            (unhighlight ()
                         (if (fboundp 'fancy-narrow-to-region)
                             (ignore-errors
                               (fancy-widen))
                           (widen))))
    (let ((should-highlight (not (active-p))))
      (unhighlight)
      (cond
       ;; Type is null, we assume we are interactive:
       ((null type-or-fn) (when should-highlight
                            (if (region-active-p)
                                (hi-region (region-beginning) (region-end))
                              (hi-defun))))

       ;; With a specified type, we aren't interactive, and if we got a
       ;; symbol, then we can use `demo-it--get-section' for our bounds.
       ((symbolp type-or-fn)
        (let ((positions (demo-it--get-section type-or-fn start end)))
          (goto-char (car positions))
          (hi-region (car positions) (cdr positions))))

       ;; With string, use `imenu' for matching:
       ((stringp type-or-fn)   (imenu type-or-fn)
        (hi-defun))))))


(defun demo-it-load-fancy-file (file type &optional start end side size)
  "Splits window and loads FILE in another window, and use fancy
narrow to highlight part of the buffer.

If TYPE is a string that specifies a function name (available via
the `imenu' call), then it highlights that function.

If TYPE is :char or 'char, START and END refers to specific
character positions, but if TYPE is :line or 'line, this selects
the point positions as if START and END are line numbers.

If `fancy-narrow' is not installed, then simply narrows to the area."
  (demo-it-load-file file side size)
  (demo-it-highlight-dwim type start end))


(provide 'demo-it-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-extras.el ends here

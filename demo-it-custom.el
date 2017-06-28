;;; DEMO-IT-CUSTOM --- The project's custom variables
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created: 19 September 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Two ways to set default properties and demonstration settings.
;; First, by setting the value on customized values, and
;; Second, by passing in a mnemonic value with `demo-it-create',
;;    See the `demo-it--set-property' for the mapping.
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

(defgroup demo-it nil
  "Customizations and behaviors for demonstrations."
  :prefix "demo-it-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/howardabrams/demo-it"))

(defcustom demo-it--keymap-mode-style :simple-mode
  "The keymap-specific minor mode to use when a demonstration
starts. Should either be :simple-mode for using the space to
advance to next step and `q' to exit the demonstration,
or :advanced-mode, where <F12> advances."
  :type '(choice (const :tag "simple"   :simple-mode)
                 (const :tag "advanced" :advanced-mode))
  :group 'demo-it)

(defcustom demo-it--shell-or-eshell :eshell
  "When opening up a shell, should this run the `shell' or `eshell' command."
  :type '(choice (const :tag "shell"  :shell)
                 (const :tag "eshell" :eshell))
  :group 'demo-it)

(defcustom demo-it--open-windows :right
  "When opening side windows, split the frame on a particular
side, like `:below' or on the `:right'."
  :type '(choice (const :tag "above" :above)
                 (const :tag "below" :below)
                 (const :tag "left"  :left)
                 (const :tag "right" :right))
  :group 'demo-it)

(defcustom demo-it--open-windows-size 80
  "The size of the window to open. This is the width if the
  window is opened on one of the sides (:left or :right), or the
  height if the window is opened :above or :below."
  :type '(integer))

(defcustom demo-it--text-scale 2
  "Sets the default text scale when opening files."
  :type '(choice integer
                 (const :tag "small" -1)
                 (const :tag "normal" 0)
                 (const :tag "medium" 1)
                 (const :tag "large" 2)
                 (const :tag "x-large" 3)
                 (const :tag "xx-large" 4)
                 (const :tag "huge" 5))
  :group 'demo-it)

(defcustom demo-it--start-fullscreen nil
  "If non-nil, start the demonstration with the frame in fullscreen mode."
  :type '(boolean)
  :group 'demo-it)

(defcustom demo-it--start-single-window t
  "If non-nil, delete other windows to start the demonstration with a single buffer window."
  :type '(boolean)
  :group 'demo-it)

(defcustom demo-it--presentation-hide-mode-line t
  "If non-nil, shows the mode-line during a presentation,
otherwise the mode-line is hidden from view."
  :type '(boolean))

(defcustom demo-it--presentation-hide-org-markers t
  "If non-nil, shows the surrounding asterisks, underlines and
slashes that define an `org-mode' textual formats, otherwise
these characters hidden, even though the effects of bolding and
italics are shown."
  :type '(boolean))

(defcustom demo-it--presentation-variable-width nil
  "If non-nil, uses a variable-width font for `org-mode' presentation files,
otherwise the continues to use the standard fixed-width font."
  :type '(boolean))

(defcustom demo-it--presentation-hide-org-blocks t
  "If non-nil, shows the `#+BEGIN_SRC' and `#+END_SRC' code
blocks in an `org-mode' presentation file, otherwise these lines
are hidden, but the contents within the blocks are still shown."
  :type '(boolean))

(defun demo-it--presentation-variable-width-p (&optional style)
  "Predicate return `t' if STYLE is either `:variable' or `:both'
or the customization setting,
`demo-it--presentation-variable-width' is true. Otherwise, STYLE
can be `:fixed'."
  (if (eq style :fixed)
      nil
    (when (or (eq style :variable)
              (eq style :both)
              demo-it--presentation-variable-width)
      t)))

(defun demo-it--presentation-hide-blocks-p (&optional style)
  "Predicate return `t' if STYLE is either `:block' or `:both'
or the customization setting, `demo-it--presentation-hide-org-blocks'
is non-nil."
  (when (or (eq style :block) (eq style :blocks)
            (eq style :both)
            demo-it--presentation-hide-org-blocks)
    t))

(defcustom demo-it--insert-text-speed :medium
  "The speed at which some functions insert text into the shell
and other place. This can be :instant for instantaneous,
or :slow, :medium or :fast.

This can also be a tuple of two integer values for the random
number of milliseconds between inserting each character."
  :type '(choice (const :tag "fast"    :fast)
                 (const :tag "faster"  :faster)
                 (const :tag "medium"  :medium)
                 (const :tag "slow"    :slow)
                 (const :tag "instant" :instant))
  :group 'demo-it)

(defun demo-it--get-insert-text-speed (&optional speed)
  "The tuple of the lower and upper limits for the insert speed
based on the symbol stored in `demo-it--insert-text-speed'.
The optional value for SPEED override the default value."
  (let ((requested-speed (or speed demo-it--insert-text-speed)))
    (pcase requested-speed
      ((or :faster :insert-faster)        '(1 . 10))
      ((or :fast   :insert-fast)          '(10 . 100))
      ((or :medium :insert-medium)        '(30 . 500))
      ((or :slow   :insert-slow)          '(200 . 1000))
      (_                                  requested-speed))))

(defun demo-it--get-text-scale (&optional size)
  "Returns SIZE if SIZE is an integer, otherwise, returns an
integer matching the symbol specified by SIZE, e.g. `:large'."
  (let ((requested-size (or size demo-it--text-scale)))
    (pcase requested-size
      ((or :small    :text-small)    -1)
      ((or :normal   :text-normal)    0)
      ((or :medium   :text-medium)    1)
      ((or :large    :text-large)     2)
      ((or :x-large  :text-x-large)   3)
      ((or :xx-large :text-xx-large)  4)
      ((or :huge     :text-huge)      5)
      (_                              requested-size))))

(defun demo-it--set-property (prop)
  "Sets a particular property, specified by keyword, PROP to t."
  (pcase prop
    (:simple-mode      (setq demo-it--keymap-mode-style :simple-mode))
    (:advance-mode     (setq demo-it--keymap-mode-style :advanced-mode))
    (:advanced-mode    (setq demo-it--keymap-mode-style :advanced-mode))

    (:use-eshell       (setq demo-it--shell-or-eshell   :eshell))
    (:use-shell        (setq demo-it--shell-or-eshell   :shell))
    (:eshell           (setq demo-it--shell-or-eshell   :eshell))
    (:shell            (setq demo-it--shell-or-eshell   :shell))

    (:windows-on-side  (setq demo-it--open-windows      :right))
    (:windows-on-right (setq demo-it--open-windows      :right))
    (:windows-on-left  (setq demo-it--open-windows      :left))
    (:windows-below    (setq demo-it--open-windows      :below))
    (:windows-above    (setq demo-it--open-windows      :above))

    (:fullscreen       (setq demo-it--start-fullscreen    t))
    (:full-screen      (setq demo-it--start-fullscreen    t))
    (:single-window    (setq demo-it--start-single-window t))

    (:text-small       (setq demo-it--text-scale -1))
    (:text-normal      (setq demo-it--text-scale 0))
    (:text-medium      (setq demo-it--text-scale 1))
    (:text-large       (setq demo-it--text-scale 2))
    (:text-x-large     (setq demo-it--text-scale 3))
    (:text-xx-large    (setq demo-it--text-scale 4))
    (:text-huge        (setq demo-it--text-scale 5))

    (:insert-quickly   (setq demo-it--insert-text-speed :instant))
    (:insert-faster    (setq demo-it--insert-text-speed :faster))
    (:insert-fast      (setq demo-it--insert-text-speed :fast))
    (:insert-medium    (setq demo-it--insert-text-speed :medium))
    (:insert-slow      (setq demo-it--insert-text-speed :slow))

    (:show-mode-line   (setq demo-it--presentation-hide-mode-line nil))
    (:hide-mode-line   (setq demo-it--presentation-hide-mode-line t))
    (:show-org-markers (setq demo-it--presentation-hide-org-markers nil))
    (:hide-org-markers (setq demo-it--presentation-hide-org-markers t))
    (:variable-width   (setq demo-it--presentation-variable-width t))
    (:fixed-width      (setq demo-it--presentation-variable-width nil))
    (:show-block-headers (setq demo-it--presentation-hide-org-blocks nil))
    (:hide-block-headers (setq demo-it--presentation-hide-org-blocks t))))

(provide 'demo-it-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-custom.el ends here

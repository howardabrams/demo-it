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
  "Customizations and behaviors for demonstrations.")

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

(defcustom demo-it--side-windows :side
  "When opening side windows, should this be `:below' or `:side'."
  :type '(choice (const :tag "side"  :side)
                 (const :tag "below" :below))
  :group 'demo-it)

(defcustom demo-it--text-scale 2
  "Sets the default text scale when opening files."
  :type '(integer)
  :group 'demo-it)

(defcustom demo-it--insert-text-speed :medium
  "The speed at which some functions insert text into the shell
and other place. This can be :instant for instantaneous,
or :slow, :medium or :fast.

This can also be a tuple of two integer values for the random
number of milliseconds between inserting each character."
  :type '(choice (const :tag "fast"    :fast)
                 (const :tag "medium"  :medium)
                 (const :tag "slow"    :slow)
                 (const :tag "instant" :instant))
  :group 'demo-it)

(defun demo-it--get-insert-text-speed ()
  "The tuple of the lower and upper limits for the insert speed
based on the symbol stored in `demo-it--insert-text-speed'."
  (pcase demo-it--insert-text-speed
    (:fast   '(10 . 100))
    (:medium '(30 . 500))
    (:slow   '(200 . 1000))
    (_       demo-it--insert-text-speed)))


(defun demo-it--set-property (prop)
  "Sets a particular property, specified by keyword, PROP to t."
  (pcase prop
    (:simple-mode      (setq demo-it--keymap-mode-style :simple-mode))
    (:advance-mode     (setq demo-it--keymap-mode-style :advanced-mode))
    (:advanced-mode    (setq demo-it--keymap-mode-style :advanced-mode))

    (:use-eshell       (setq demo-it--shell-or-eshell   :eshell))
    (:use-shell        (setq demo-it--shell-or-eshell   :shell))

    (:windows-to-side  (setq demo-it--side-windows      :side))
    (:windows-below    (setq demo-it--side-windows      :below))

    (:insert-fast      (setq demo-it--insert-text-speed :fast))
    (:insert-medium    (setq demo-it--insert-text-speed :medium))
    (:insert-slow      (setq demo-it--insert-text-speed :slow))))

(provide 'demo-it-customization)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-custom.el ends here

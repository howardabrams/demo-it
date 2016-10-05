;;; EXAMPLE-FILES --- Demo of `demo-it' showing files
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created:  3 October 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;      This demonstration shows some of the ways we can load and show
;;      files, like images and source code.
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

(require 'demo-it)

(ignore-errors
  (kill-buffer "example.py"))

(defun dit-switch-and-load-two ()
  (demo-it-presentation-return)
  (demo-it-compare-files "example-code.py" "example-code.rb"))

(demo-it-create :full-screen :single-window :hide-block-headers
                (demo-it-presentation "example-files.org")
                demo-it-presentation-return
                (demo-it-load-fancy-file "example-code.el" :line 27 36)
                dit-switch-and-load-two
                demo-it-presentation-return
                (demo-it-show-image "pdx-emacs.png" :below nil 16)
                demo-it-presentation-return)

(demo-it-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example-files.el ends here

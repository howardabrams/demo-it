;;; EXAMPLE-SHELL --- Demonstrates Shell Features
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created:  1 October 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Demonstrates the features of running commands in a shell.
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

(demo-it-create :use-eshell ;; Change this to :use-shell
                :single-window
                :windows-on-right
                :insert-fast
                :text-large

                (demo-it-presentation "example-shell.org")

                ;; This step opens up a shell window:
                (demo-it-start-shell)

                ;; Press 'Space' to run each of these commands:
                (demo-it-run-in-shell "pwd")
                (demo-it-run-in-shell "hostname" nil :slow)

                ;; Closes the window and goes to next presentation section
                (demo-it-presentation-return)

                ;; Passing nil for first parameter uses current dir
                ;; Connect to default shell if third param is nil
                (demo-it-start-shell nil "ls" nil :below :large 10))

(demo-it-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example-shell.el ends here

;;; DEMO-IT-TESTS --- Basic tests to help verify demo-it functions
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created: 22 September 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Obviously testing specific functions is easy with ERT, but
;;  attempting to verify a _visual demonstration_ is a lot more
;;  complicated.
;;
;;  However, we'll do what we can.
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

(require 'ert)

(load-file "demo-it.el")
(defvar demo-it--shell-or-eshell)
(defvar demo-it--keymap-mode-style)
(defvar demo-it--insert-text-speed)
(defvar demo-it--side-windows)

(declare-function demo-it-create "demo-it")
(declare-function demo-it-start "demo-it")
(declare-function demo-it-end "demo-it")
(declare-function demo-it--set-property "demo-it-custom")
(declare-function demo-it--get-insert-text-speed "demo-it-custom")


(defvar demo-it-tests--state 0)
(defun demo-it-tests--step-1 (&optional state)
  "Just an initial step in a demonstration"
  (setq demo-it-tests--state (if state state 1)))
(defun demo-it-tests--step-2 ()
  "Another step in a demonstration"
  (setq demo-it-tests--state 2))

;; ----------------------------------------------------------------------

(ert-deftest basic-validation ()
  "Validate first step executes, but not the second."
  (setq demo-it-tests--state 0)
  (demo-it-create demo-it-tests--step-1
                  demo-it-tests--step-2)
  (demo-it-start)
  (demo-it-end)
  (should (= 1 demo-it-tests--state)))

(ert-deftest basic-form-validation ()
  "Validate first form executes, but not the second."
  (demo-it-create (demo-it-tests--step-1 5)
                  demo-it-tests--step-2)
  (setq demo-it-tests--state 0)
  (demo-it-start)
  (demo-it-end)
  (should (= 5 demo-it-tests--state)))

;; ----------------------------------------------------------------------
;;  Let's test some of the property values

(ert-deftest test-demo-it--get-insert-text-speed ()
  (demo-it--set-property :insert-fast)
  (should (and (= 10 (car (demo-it--get-insert-text-speed)))
               (= 100 (cdr (demo-it--get-insert-text-speed)))))

  (demo-it--set-property :insert-medium)
  (should (and (= 30 (car (demo-it--get-insert-text-speed)))
               (= 500 (cdr (demo-it--get-insert-text-speed)))))

  (demo-it--set-property :insert-slow)
  (should (and (= 200 (car (demo-it--get-insert-text-speed)))
               (= 1000 (cdr (demo-it--get-insert-text-speed))))))

(ert-deftest test-demo-it--set-property ()
  (demo-it--set-property :use-shell)
  (should (eq :shell demo-it--shell-or-eshell))
  (demo-it--set-property :use-eshell)
  (should (eq :eshell demo-it--shell-or-eshell))

  (demo-it--set-property :windows-below)
  (should (eq :below demo-it--side-windows))
  (demo-it--set-property :windows-to-side)
  (should (eq :side  demo-it--side-windows))

  (demo-it--set-property :simple-mode)
  (should (eq :simple-mode demo-it--keymap-mode-style))
  (demo-it--set-property :advance-mode)
  (should (eq :advanced-mode demo-it--keymap-mode-style))
  (demo-it--set-property :advanced-mode)
  (should (eq :advanced-mode demo-it--keymap-mode-style)))

(ert-deftest test-demo-it--get-section ()
  ;; The :char type should return the values given:
  (let* ((start 71)
         (end   212)
         (tuple (demo-it--get-section :char start end)))
    (should (and (= (car tuple) start)
                 (= (cdr tuple) end))))

  ;; The :line type should return the points of the starting and
  ;; ending lines... don't change the comment section at the top of
  ;; this file without resetting the magic values:
  (let* ((line-1 3)
         (line-2 6)
         (tuple (demo-it--get-section :line line-1 line-2)))
    (should (and (= (car tuple) 71)
                 (= (cdr tuple) 212)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; demo-it-tests.el ends here

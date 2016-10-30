;;; A-DEMO --- Demonstration of demo-it
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created: 27 October 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    A demonstration and a bit of a live tutorial of the demo-it
;;    project... yeah, seriously meta.
;;
;;    Keep in mind, this is a bit of an 'advanced demonstration' where
;;    many of the steps are actually functions that perform multiple
;;    actions. Tis also a little more fun to craft.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'demo-it)

;; Let's pretend our demonstration is really code, and define a few
;; variables that we'll use when referring to the source code files in
;; the 'examples' directory:

(setq a-demo/example-dir (expand-file-name "../example"))
(setq a-demo/python-buffer "example.py")
(setq a-demo/python-source
      (format "%s/%s" a-demo/example-dir a-demo/python-buffer))
(setq a-demo/ruby-buffer "example.rb")
(setq a-demo/ruby-source
      (format "%s/%s" a-demo/example-dir a-demo/ruby-buffer))


;; The following is the steps to this demonstration, keep in mind that
;; many of the 'steps' are calls to functions defined below.
;;
;; Note: The keywords may override some of your default settings of
;; our customized variables.

(demo-it-create :fullscreen :single-window :advanced-mode
                :use-shell :variable-width

                (demo-it-presentation "a-demo.org")
                demo-it-presentation-advance
                demo-it-presentation-advance

                ;; Mini demonstration ...
                (find-file a-demo/ruby-source)
                (demo-it-start-shell a-demo/example-dir)
                (demo-it-run-in-shell "ruby ./example.rb")
                "M-p"
                (demo-it-run-in-shell " Everyone")

                ;; And the rest of the demonstration
                demo-it-presentation-return   ; Demo It Functions
                demo-it-presentation-advance
                a-demo/show-file
                a-demo/show-part-file
                a-demo/show-fancy-file
                a-demo/show-compare-files
                demo-it-presentation-return   ; Running Code
                demo-it-presentation-advance  ; Presenting

                demo-it-presentation-advance  ; Misc
                (demo-it-show-image
                 (format "%s/pdx-emacs.png" a-demo/example-dir)
                 :below 0 18)
                demo-it-presentation-return-noadvance

                demo-it-presentation-advance  ; Getting Started
                a-demo/show-customizations
                a-demo/show-info-mode
                demo-it-presentation-return-noadvance)

;; The following functions are a collection of multi-action steps

(defun a-demo/show-file ()
  "An example of a multi-function 'step' where we write a number
of sommands that will be executed with a single action. In this
case, we first highlight a presentation phrase and then display
the source code.

We also make sure we have killed the source code to display, so
that any settings we may have added to it go away."
  (demo-it-presentation-highlight-phrase "demo-it-load-file")
  (ignore-errors  ; Make sure we load a fresh copy
    (kill-buffer a-demo/python-buffer))
  (demo-it-load-file a-demo/python-source))

(defun a-demo/show-part-file ()
  (demo-it-presentation-return-noadvance)
  (ignore-errors   ; File shouldn't be loaded, but just in case:
    (kill-buffer a-demo/python-buffer))
  (demo-it-presentation-highlight-phrase "demo-it-load-part-file")
  (demo-it-load-part-file a-demo/python-source :line 5 12))

(defun a-demo/show-fancy-file ()
  (demo-it-presentation-return-noadvance) ; (kill-buffer-and-window)
  (kill-buffer a-demo/python-buffer)
  (demo-it-presentation-highlight-phrase "demo-it-load-fancy-file")
  (demo-it-load-fancy-file a-demo/python-source :line 5 12))

(defun a-demo/show-compare-files ()
  (demo-it-presentation-return-noadvance) ; (kill-buffer-and-window)
  (kill-buffer a-demo/python-buffer)
  (demo-it-presentation-highlight-phrase "demo-it-compare-files")
  (demo-it-compare-files a-demo/python-source a-demo/ruby-source))

(defun a-demo/show-customizations ()
  "Simply opens up the customize-group in side window"
  (demo-it-presentation-highlight-phrase "Customize the package")
  (split-window-horizontally)
  (customize-group-other-window "demo-it"))

(defun a-demo/show-info-mode ()
  "Opens side window with the Info to demo-it"
  (demo-it-presentation-highlight-phrase "Read the.*$")
  (info "demo-it"))

(defun a-demo/clean-up ()
  "Cleans up the files it has loaded, et. al."
  (interactive)
  (ignore-errors
    (kill-bufer "Shell"))
  (ignore-errors
    (kill-buffer a-demo/python-buffer))
  (ignore-errors
    (kill-buffer a-demo/ruby-buffer)))

(a-demo/clean-up)

(demo-it-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a-demo.el ends here

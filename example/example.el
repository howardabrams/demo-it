;;; example --- Demonstrates the demo-it project using demo-it

;;; Commentary:

;; This is a simple demonstration that shows off a few of the
;; functions and options of the demo-it system.  Evaluating this
;; buffer starts off the presentation, and hitting F6 steps through
;; the demonstration.

;;; Code:

(load-library "demo-it")

;; ----------------------------------------------------------------------
;;  Create each function that represents a "presentation frame"

(defun dit-show-title ()
  "Display a title screen to kick off the presentation."
  (demo-it-frame-fullscreen)
  (demo-it-title-screen "example-title.org"))

(defun dit-load-presentation ()
  "Display example.org (an 'org-mode' file) as a presentation."
  (demo-it-presentation "example.org"))

(defun dit-load-source-code ()
  "Load some source code in a side window."
  (demo-it-load-fancy-file "example.py" 'line 5 12 t)
  (demo-it-presentation-advance))

(defun dit-run-code ()
  "Execute our source code in an Eshell buffer."
  (demo-it-run-in-eshell "~/Other/demo-it" "python example.py Snoopy" "Python Code" 'below)
  (demo-it-presentation-advance))

(defun dit-cleanup ()
  "Cleans up the mess of the presentation."
  (insert "exit")                     ;; Delete and close the Eshell window
  (eshell-send-input)
  (demo-it-end))

;; ----------------------------------------------------------------------
;; Demonstration and/or Presentation Order

(defun dit-start-presentation ()
  "Demonstrates the demo-it project using demo-it."
  (interactive)
  (demo-it-start (list
                  'dit-show-title                ;; Frame 1
                  'dit-load-presentation         ;; Frame 2
                  'dit-load-source-code          ;; Frame 3
                  'dit-run-code                  ;; Frame 4
                  'dit-cleanup                   ;; Done
                  )))

;; ----------------------------------------------------------------------
;; Start the presentation whenever this script is evaluated. Good idea?

(dit-start-presentation)

;;; example.el ends here

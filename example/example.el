;;; EXAMPLE --- Demonstrate a demo-it demonstration

;;; Commentary:

;; This is a simple demonstration that shows off a few of the
;; functions and options of the demo-it system.  Evaluating this
;; buffer starts off the presentation, and hitting SPC steps through
;; the demonstration.

;;; Code:

(require 'demo-it)

;; ----------------------------------------------------------------------
;;  Create some demonstration helper functions...

(defun dit-load-source-code ()
  "Load some source code in a side window."
  (demo-it-presentation-advance)
  (demo-it-load-fancy-file "example.py" :line 5 12 :side))

(defun dit-run-code ()
  "Execute our source code in an Eshell buffer."
  ;; Close other windows and advance the presentation:
  (demo-it-presentation-return)

  (ignore-errors
    (demo-it-start-shell))
  (demo-it-run-in-shell "python example.py Snoopy"))

;; ----------------------------------------------------------------------
;; Demonstration creation and the ordering of steps...

(demo-it-create :single-window :insert-fast
                (demo-it-title-screen "example-title.org")
                (demo-it-presentation "example.org")
                dit-load-source-code ;; Step 3
                dit-run-code)        ;; Step 4

;; ----------------------------------------------------------------------
;; Start the presentation whenever this script is evaluated. Good idea?

(demo-it-start)

;;; example.el ends here

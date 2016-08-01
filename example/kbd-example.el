;;; kbd-example.el --- Demonstration of step by keyboard macro  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple demonstration that shows off keyboard macro
;; of the demo-it system.  Evaluating this buffer starts off the
;; presentation, and hitting SPC steps through the demonstration.

;;; Code:

(demo-it-start
 '((lambda () (message "Show example.el"))
   "C-x C-f example.el "
   "M-<"
   "M->"))

(provide 'kbd-example)
;;; kbd-example.el ends here

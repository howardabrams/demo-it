;;; error-example.el --- Demo error example          -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple demonstration that shows off an error handling of
;; the demo-it system.  Evaluating this buffer starts off the
;; presentation, and hitting SPC steps through the demonstration.

;;; Code:

(setq a 0)
(demo-it-start
 '((lambda ()
     (demo-it-setq a 2))
   (lambda ()
     (run-with-timer 3 nil (lambda () (message "a=%s (after the error demo)" a)))
     (message "a=%s" a))
   (lambda ()
     (error "an error"))
   (lambda ()
     (message "not reached"))))

(provide 'error-example)
;;; error-example.el ends here

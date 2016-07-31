;;; setq-example.el --- Demonstration of Demonstration of demo-it-setq  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a simple demonstration that shows off temporary variables
;; of the demo-it system.  Evaluating this buffer starts off the
;; presentation, and hitting SPC steps through the demonstration.

;;; Code:

(demo-it-start
 '((lambda ()
     (setq a 1)
     (makunbound 'v)
     (message "a=%d v=%s boundp=%s" a (bound-and-true-p v) (boundp 'v)))
   (lambda ()
     (demo-it-setq a 10 v 0)
     (message "a=%d v=%s boundp=%s" a (bound-and-true-p v) (boundp 'v))
     (run-at-time 2 nil (lambda () (message "After demo:  a=%d v=%s boundp=%s" a (bound-and-true-p v) (boundp 'v)))))))

(provide 'setq-example)
;;; setq-example.el ends here

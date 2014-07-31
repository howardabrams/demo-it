;;; blah.el --- Utility functions for creating demonstrations within Emacs

;; Copyright (C) 2014  Howard Abrams

;; Author: Howard Abrams <howard.abrams@workday.com>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;   When making demonstrations of new products, technologies and other
;;   geekery, I love the versatility of using Emacs to demonstrate the
;;   trifecta of sprint reviews, including:

;;   - Presentations explaining the technologies
;;   - Source code ... correctly highlighted
;;   - Executing the code in Eshell ... or similar demonstration

;;   However, I don't want to fat-finger, mentally burp, or even delay
;;   the gratification while I type, so I predefine each "step" as an
;;   Elisp function, and then have =demo-it= execute each function when I
;;   hit the F6 key.

;;   Using the library is a three step process:

;;   1. Load the library in your own Elisp source code file
;;   2. Create a collection of functions that "do things".
;;   3. Call the =demo-it-start= function with the ordered list of
;;      functions.

;;   For instance:

;;   (load-library "demo-it")   ;; Load this library of functions

;;   (defun my-demo/step-1 ()
;;     (delete-other-windows)
;;     (demo/org-presentation "~/presentations/emacs-demo/emacs-demo-start.org"))

;;   (defun my-demo/step-2 ()
;;     (demo-it-load-side-window "~/Work/my-proj/src/my-proj.py")
;;     (demo-it-org-presentation-return))

;;   (defun my-demo ()
;;      "My fabulous demonstration."
;;      (interactive)
;;      (demo-start (list
;;                      'my-demo/step-1
;;                      'my-demo/step-2
;;                      ;; ...
;;                    )))

;;   (my-demo) ;; Optionally start the demo when file is loaded.

;;   Each "step" is a series of Elisp functions that "do things".
;;   While this package has a collection of helping functions, the steps
;;   can use any Elisp command to show off a feature.

;;   I recommend installing these other Emacs packages:

;;   - https://github.com/takaxp/org-tree-slide
;;   - https://github.com/sabof/org-bullets
;;   - https://github.com/magnars/expand-region.el
;;   - https://github.com/Bruce-Connor/fancy-narrow

;;; Code:

;;   To begin, we need a "global" variable (shudder) that keeps track of
;;   the current state of the demonstration.

(defvar demo-it-step 0  "Stores the current demo 'step' function.")

(defvar demo-it-steps '() "The list of functions to be executed in order.")

;; Starting a Demonstration
;;
;;   When we start a demonstration, we would pass in a list of functions
;;   to call for each step, and then call =demo-step= to execute the
;;   first one on the list.

(defun demo-it-start (steps)
   "Start the current demonstration and kick off the first step.
STEPS is a list of functions to execute."
   (setq demo-it-step 0)          ;; Reset the step to the beginning
   (setq demo-it-steps steps)     ;; Store the steps.
   (demo-it-step))

;; Next Step
;;
;;   Hitting the <F6> key should be bound to triggering the next step in
;;   the demonstration.

(defun demo-it-step (&optional step)
  "Execute the next step in the current demonstration.  Just to a particular STEP if the optional parameter is given, i.e. C-6 <F6> to run the 6th step."
  (interactive "P")
    (if step
        (setq demo-it-step step)    ;; Changing Global state, yay!
      (setq demo-it-step (1+ demo-it-step)))
    (let
        ;; At this point, step is 1-based, and I need it 0-based
        ;; and f-step is the function to call for this step...
        ((f-step (nth (1- demo-it-step) demo-it-steps)))
      (if f-step
          (progn
            (funcall f-step)
            (message "  %d" demo-it-step))
        (message "Finished the entire demonstration."))))

;; Bind the =demo-it-step= function to the F6 key:

(global-set-key (kbd "<f6>") 'demo-it-step)

;; Position or advance the slide? Depends...

(defun demo-it-set-mouse-or-advance (evt)
  "Advances to the next step if clicked on the right side of any window, otherwise, it position the point as expected.  With EVT, function can be bound to the mouse click."
  (interactive "e")
  (if (posn-area (event-start evt))  ;; Clicked in special area?
      (demo-it-step)
    (let ((col (car (posn-col-row (event-start evt))))
          (wid (window-width (posn-window (event-start evt)))))
      (if (> col (- wid 4))
          (demo-it-step)
        (mouse-set-point evt)))))

(defun demo-it-ignore-event (evt)
  "Empty function that absorbs the EVT parameter to keep demonstration from flpping out."
  (interactive "P")
  (message ""))

(global-set-key (kbd "<mouse-1>") 'demo-it-set-mouse-or-advance)
(global-set-key [nil mouse-1] 'demo-it-step)
(global-set-key [nil wheel-up] 'demo-it-ignore-event)
(global-set-key [nil wheel-down] 'demo-it-ignore-event)
(global-set-key [nil wheel-left] 'demo-it-ignore-event)
(global-set-key [nil wheel-right] 'demo-it-ignore-event)

;; Auto Loading of Available Features
;;
;;    The following "supporting functions" often depend on other packages
;;    from ELPA, but we don't want to simply 'require' something that
;;    hasn't been installed. This function can be used to look up
;;    packages that can be loaded without barfing.

(defun demo-it--autofeaturep (feature)
  "For a FEATURE like 'foo, return true if the feature can be required.
The result is equivalent to: (or (featurep 'foo-autoloads) (featurep 'foo))"
  (catch 'result
    (let ((feature-name (symbol-name feature)))
      (unless (string-match "-autoloads$" feature-name)
        (let ((feature-autoloads (intern-soft (concat feature-name "-autoloads"))))
          (when (and feature-autoloads (featurep feature-autoloads))
            (throw 'result t))))
      (featurep feature))))

;; Fancy Region Highlighting
;;
;;    When talking about a single function or area, we use the
;;    =expand-region= project along with the =fancy-narrow=:

(when (demo-it--autofeaturep 'expand-region)
  (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(when (demo-it--autofeaturep 'fancy-narrow)
  (require 'fancy-narrow)
  (global-set-key (kbd "M-C-=") 'highlight-section)
  (global-set-key (kbd "M-C-+") 'fancy-widen))

;; While sometimes I want highlight some code, it is usually a
;;    function, so instead of remembering two key combinations, let's
;;    just have the =C-+= narrow to the region if active, otherwise,
;;    narrow to the function:

(defun highlight-section ()
  "If the region is active, call 'fancy-narrow-to-region on it, otherwise, call 'fancy-narrow-to-defun, and see what happens."
  (interactive)
  (if (region-active-p)
      (fancy-narrow-to-region (region-beginning) (region-end))
    (fancy-narrow-to-defun)))

;; Hiding the Modeline
;;
;;    Call the =hidden-mode-line= when displaying images and org-mode
;;    files displayed as "presentations"...or just not
;;    wanting to be bothered by the sight of the mode. This code was
;;    graciously lifted from [[http://bzg.fr/emacs-hide-mode-line.html][here]].

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (progn
        (setq hide-mode-line mode-line-format
              mode-line-format nil)
        (linum-mode -1))
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  (set-window-buffer nil (current-buffer))

  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode RET to make the mode-line appear."))))

;; Making a Side Window
;;
;;    Typically, we make a side window that is large enough to have some
;;    fun in, as the main window would serve as little more than an
;;    outline.

(defun demo-it-make-side-window ()
  "Splits the window horizontally and puts point on right side window."
  (split-window-horizontally)
  (other-window 1))

;; Load a File in the Side Window
;;
;;    Splits the window and loads a file on the right side of the screen.

(defun demo-it-load-side-file (file &optional size)
  "Splits window and load FILE on the right side of the screen.  The SIZE can be used to scale the text font, which defaults to 1 step larger."
  (demo-it-make-side-window)
  (find-file file)
  (if size (text-scale-set size)
           (text-scale-set 1)))

;; Load a File and Fancily Highlight Some Lines
;;
;;    Would be nice to load up a file and automatically highlight some
;;    lines.

(defun demo-it-load-fancy-side-file (file type line1 line2 &optional side size)
  "Load FILE and use fancy narrow to highlight part of the buffer.  If TYPE is 'char, LINE1 and LINE2 are position in buffer, otherwise LINE1 and LINE2 are start and ending lines to highlight.  If SIDE is t, the buffer is placed in a new side window, and SIZE is the text scale, which defaults to 1."
  (if side (demo-it-make-side-window))
  (find-file file)
  (if size (text-scale-set size)
           (text-scale-set 1))
  (let ((start line1)
        (end line2))
    (unless (eq type 'char)
      (goto-char (point-min)) (forward-line (1- line1))  ;; Heh: (goto-line line1)
      (setq start (point))
      (goto-char (point-min)) (forward-line line2)
      (setq end (point)))
    (fancy-narrow-to-region start end)))

;; Start an Eshell and Run Something
;;
;;    This function assumes you want an Eshell instance running in the
;;    lower half of the window. Changes to a particular directory, and
;;    automatically runs something.

(defun demo-it-run-in-eshell (directory &optional shell-line name size)
   "Start Eshell instance, and change to DIRECTORY to execute SHELL-LINE.  NAME optionally labels the buffer, and SIZE specifies the text scale, which defaults to 1 level larger."
   (let ((title (if name (concat "Shell: " name) "Shell")))
     (split-window-vertically)
     (other-window 1)
     (eshell "new")
     (rename-buffer title)
     (if size (text-scale-set size)
              (text-scale-set 1))

     (insert (concat "cd " directory))
     (eshell-send-input)
     (erase-buffer)
     (eshell-send-input)

     (when shell-line
       (insert shell-line)
       (eshell-send-input))))

;; Title Display
;;
;;    Create a file to serve as a "title" as it will be displayed with a
;;    larger-than-life font.

(defun demo-it-title-screen (file)
  "Use FILE to serve as a presentation title, as it will be displayed with a larger-than-life font."
  (delete-other-windows)
  (fringe-mode '(0 . 0))

  (find-file file)
  (show-all)
  (hidden-mode-line-mode)
  (setq cursor-type nil)
  (flyspell-mode -1)
  (variable-pitch-mode 1)
  (text-scale-set 5)

  (message "%s" "† This presentation is running within Emacs."))

;; Starting an ORG Presentation
;;
;;    Since I often have an org-mode file on the side of the screen to
;;    demonstrate an outline of what I will be demoing, I made it a
;;    function.

;;    Uses org-tree-slide if available.
;;    See https://github.com/takaxp/org-tree-slide

(defvar demo-it-org-presentation-file "")
(defvar demo-it-org-presentation-buffer "")

(defun demo-it-org-presentation (file &optional size)
  "Load FILE (org-mode?) as presentation.  Start org-tree-slide if available.  SIZE specifies the text scale, and defaults to 2 steps larger."
  (find-file file)
  (setq demo-it-org-presentation-file file)
  (setq demo-it-org-presentation-buffer (buffer-name))

  (when (demo-it--autofeaturep 'org-tree-slide)
    (require 'org-tree-slide)
    (setq org-tree-slide-heading-emphasis t)
    (org-tree-slide-mode))

  (flyspell-mode -1)
  (setq cursor-type nil)
  (variable-pitch-mode 1)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (hidden-mode-line-mode)
  (if size (text-scale-set size)
           (text-scale-set 2))

  (when (demo-it--autofeaturep 'org-bullets-mode)
    (org-bullets-mode 1)))

;; Jumping Back to the Presentation
;;
;;    In this case, we've been doing some steps, and the screen is
;;    "messed up", calling this function returns back to the
;;    presentation.

(defun demo-it-org-presentation-return-noadvance ()
  "Return to the presentation buffer and delete other windows."
  (switch-to-buffer demo-it-org-presentation-buffer)
  (delete-other-windows))

(defun demo-it-org-presentation-return ()
  "Return to the presentation buffer, delete other windows, and advance to the next 'org-mode' section."
  (demo-it-org-presentation-return-noadvance)
  (when (demo-it--autofeaturep 'org-tree-slide)
     (org-tree-slide-move-next-tree)))

;; Advance Presentation without Changing Focus
;;
;;    Advances the org-mode presentation, but after popping into that
;;    presentation buffer, returns to the window where our focus was
;;    initially.

(defun demo-it-org-presentation-advance ()
  "Advance the presentation to the next frame (if the buffer is an 'org-mode' and 'org-tree-slide' is available), but doesn't change focus or other windows.  Only useful if using the org-tree-slide mode for the presentation buffer."
  (let ((orig-window (current-buffer)))
    (switch-to-buffer demo-it-org-presentation-buffer)
    (when (demo-it--autofeaturep 'org-tree-slide)
      (org-tree-slide-move-next-tree))
    (switch-to-buffer orig-window)))

;; Clean up the Presentation
;;
;;    The org-presentation-start function alters the way an org-mode file
;;    is displayed. This function returns it back to a normal, editable
;;    state.

(defun demo-it-org-presentation-quit ()
  "Undo display settings made to the presentation buffer."
  (when (demo-it--autofeaturep 'org-tree-slide)
    (org-tree-slide-mode -1))

  (flyspell-mode t)
  (setq cursor-type t)
  (variable-pitch-mode nil)
  (hidden-mode-line-mode nil)
  (text-scale-set 0))

;; Display an Image on the Side

(defun demo-it-show-an-image (image-file)
  "Load IMAGE-FILE as image (or any other special file) in buffer on right side without a mode line."
  (split-window-horizontally)
  (other-window 1)
  (find-file image-file)
  (hidden-mode-line-mode))

;; Switch Framesize
;;
;;    During a demonstration, it might be nice to toggle between
;;    full screen and "regular window" in a programmatic way:

(defun demo-it-toggle-fullscreen ()
  "Toggle the frame between full screen and normal size."
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; We can force the window to be full screen:

(defun demo-it-frame-fullscreen ()
  "Set the frame window to cover the full screen."
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;; Let's make a right-side frame window:

(defun demo-it-frame-leftside ()
  "Set the window frame to be exactly half the physical display screen, and place it on the left side of the screen.  This can be helpful when showing off some other application."
  (interactive)
  (let* ((full-pixels (- (x-display-pixel-width) 16))
         (full-width  (/ full-pixels (frame-char-width)))
         (dest-width (/ full-width 2)))
    (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'width dest-width)
    (set-frame-parameter nil 'left 0)))

;;   As a final harrah, we need to let other files know how to include
;;   this bad child.

(provide 'demo-it)

;;; demo-it ends here

;;; demo-it.el --- Utility functions for creating demonstrations and tests with eyes

;; Copyright (C) 2014  Howard Abrams

;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Keywords: demonstration presentation test

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
;;
;;   - Presentations explaining the technologies
;;   - Source code ... correctly highlighted
;;   - Executing the code in Eshell ... or similar demonstration
;;   - Test a new feature
;;
;;   However, I don't want to fat-finger, mentally burp, or even delay
;;   the gratification while I type, so I predefine each "step" as an
;;   Elisp function or keyboard macro, and then have =demo-it= execute
;;   each function when I hit either the SPACE key or the F12 key
;;   (advanced minor mode).
;;
;;   Using the library is a three step process:
;;
;;   1. Load the library in your own Elisp source code file
;;   2. Create a collection of functions that "do things".
;;   3. Call the =demo-it-start= function with the ordered list of
;;      functions.
;;
;;   For instance:
;;
;;   (load-library "demo-it")   ;; Load this library of functions
;;
;;   (defun my-demo/step-1 ()
;;     (delete-other-windows)
;;     (demo/org-presentation "~/presentations/my-demo/demo-start.org"))
;;
;;   (defun my-demo/step-2 ()
;;     (demo-it-load-file "~/Work/my-proj/src/my-proj.py")
;;     (demo-it-presentation-return))
;;
;;   (defun my-demo ()
;;      "My fabulous demonstration."
;;      (interactive)
;;      (demo-it-start (list
;;                      'my-demo/step-1
;;                      'my-demo/step-2
;;                      ;; ...
;;                    )))
;;
;;   (my-demo) ;; Optionally start the demo when file is loaded.
;;
;;   Each "step" is a series of Elisp functions that "do things".
;;   While this package has a collection of helping functions, the steps
;;   can use any Elisp command to show off a feature.
;;
;;   I recommend installing these other Emacs packages:
;;
;;   - https://github.com/takaxp/org-tree-slide
;;   - https://github.com/sabof/org-bullets
;;   - https://github.com/magnars/expand-region.el
;;   - https://github.com/Bruce-Connor/fancy-narrow
;;
;;   See http://github.com/howardabrams/demo-it for more details and
;;   better examples.  You will want to walk through the source code
;;   for all the utility functions.

;;; Code:

(require 'cl-lib)

;;   Predefined necessary external functions:
(declare-function face-remap-remove-relative "face-remap.el")

;;   To begin, we need a "global" variable (shudder) that keeps track of
;;   the current state of the demonstration.

(defvar demo-it--step 0  "Stores the current demo 'step' function.")
(defvar demo-it--steps '() "List of functions to be executed in order.")

;; The following functions come from other projects I like to use
(declare-function fancy-narrow-to-region "ext:fancy-narrow")
(declare-function fancy-narrow-to-defun "ext:fancy-narrow")
(declare-function eshell-send-input "ext:eshell")
(declare-function show-all "ext:eshell.c")
(defvar org-tree-slide-heading-emphasis)
(defvar org-hide-emphasis-markers)

;; Starting a Demonstration
;;
;;   When we start a demonstration, we would pass in a list of functions
;;   to call for each step, and then call =demo-step= to execute the
;;   first one on the list.
(defvar demo-it-start-winconf nil
  "Window configuration when starting demo.")

;;;###autoload
(defun demo-it-start (steps &optional advanced-mode)
  "Start the current demonstration and kick off the first step.
STEPS is a list of functions or keystrokes to execute.
If non-nil, the optional ADVANCED-MODE turns on keybindings where
<F12> advances the steps instead of Space.  This mode is better
for more interactive demonstrations."
  (when (or demo-it-mode demo-it-mode-adv)
    (error "Do not start new demonstrations DURING demonstration"))
  (setq demo-it-start-winconf (current-window-configuration))
  (setq demo-it--step 0)      ;; Reset the step to the beginning
  (setq demo-it--steps steps) ;; Store the steps.
  (delete-other-windows)
  (if (not advanced-mode)
      (demo-it-mode t)            ;; Turn on global keymapping mode
    (demo-it-mode-adv t))
  (demo-it-step))

(defun demo-it-end ()
  "End the current demonstration by resetting the values
inflicted on the presentation buffer as well as closing other
windows."
  (interactive)
  (demo-it-disable-mode)
  (demo-it-presentation-return-noadvance) ;; Close other windows
  (demo-it-presentation-quit)
  (set-window-configuration demo-it-start-winconf))

;; Next Step
;;
;;   Hitting the <F12> key should be bound to triggering the next step in
;;   the demonstration.

(defun demo-it-step (&optional step)
  "Execute the next step in the current demonstration.  Jump to a
particular STEP if the optional parameter is given, i.e. C-6 <F12>
to run the 6th step."
  (interactive "P")
  (if step
      (setq demo-it--step step)    ;; Changing Global state, yay!
    (setq demo-it--step (1+ demo-it--step)))
  (let
      ;; At this point, step is 1-based, and I need it 0-based
      ;; and f-step is the function to call for this step...
      ((f-step (nth (1- demo-it--step) demo-it--steps)))
    (if f-step
        (demo-it--execute-step f-step)
      (read-event "Finished the entire demonstration. Hit any key to return.")
      (demo-it-end))))

(defun demo-it-restep ()
  "Execute the previous step in the current demonstration.

Useful when the previous step failed, and you want to redo it."
  (interactive)
  (let
      ;; At this point, step is 1-based, and I need it 0-based
      ;; and f-step is the function to call for this step...
      ((f-step (nth (1- demo-it--step) demo-it--steps)))
    (if f-step
        (demo-it--execute-step f-step)
      (message "Finished the entire demonstration."))))

(defun demo-it--execute-step (f-step)
  (condition-case err
      (cond ((functionp f-step) (funcall f-step))
            ((stringp f-step)   (execute-kbd-macro (kbd f-step)))
            (t                  (error "invaid step: %s" f-step)))
    (error (read-event (format "Abort the demonstration because of error. Hit any key to return.\n%S" err))
           (demo-it-end))))

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

(defun demo-it-show-step ()
  "Display the expected function to be run during the next step."
  (interactive)
  (let ((func  (nth demo-it--step demo-it--steps)))
    (message "Step: %d - Going to run: %s" demo-it--step func)))

(defun demo-it-ignore-event (evt)
  "Empty function that absorbs the EVT parameter to keep demonstration from flpping out."
  (interactive "P")
  (message ""))


;; Fancy Region Highlighting
;;
;; While sometimes I want highlight some code, it is usually a
;;    function, so instead of remembering two key combinations, let's
;;    just have the =C-+= narrow to the region if active, otherwise,
;;    narrow to the function:

(defun demo-it-highlight-section ()
  "If the region is active, call 'fancy-narrow-to-region on it, otherwise, call 'fancy-narrow-to-defun, and see what happens."
  (interactive)
  (when (fboundp 'fancy-narrow-to-region)
    (if (region-active-p)
        (fancy-narrow-to-region (region-beginning) (region-end))
      (fancy-narrow-to-defun))))

;; Hiding the Modeline
;;
;;    Call the demo-it-hide-mode-line when displaying images and
;;    org-mode files displayed as "presentations", so that we aren't
;;    bothered by the sight of the mode.

(defvar demo-it--old-mode-line nil)
(make-variable-buffer-local 'demo-it--old-mode-line)

(defun demo-it-hide-mode-line ()
  "Hide mode line for a particular buffer."
  (interactive)
  (when mode-line-format
    (setq demo-it--old-mode-line mode-line-format)
    (setq mode-line-format nil)))

(defun demo-it-show-mode-line ()
  "Show mode line for a particular buffer, if it was previously hidden with 'demo-it--hide-mode-line."
  (interactive)
  (if demo-it--old-mode-line
      (setq mode-line-format demo-it--old-mode-line)))

;; Making a Side Window
;;
;;    Typically, we make a side window that is large enough to have some
;;    fun in, as the main window would serve as little more than an
;;    outline.

(defun demo-it-make-side-window (&optional side)
  "Splits the window horizontally and puts point on right side window.  SIDE is either 'below or 'side (for the right side)."
  (when side
    (if (eq side 'below)
        (split-window-vertically)
      (split-window-horizontally)))
  (other-window 1))

;; Load a File in the Side Window
;;
;;    Splits the window and loads a file on the right side of the screen.

(defun demo-it-load-file (file &optional side size)
  "Splits window and load FILE on the right side of the screen.  If SIDE is non-nil, the source code file is place in a window either 'below or to the 'side.  The SIZE can be used to scale the text font, which defaults to 1 step larger.  This function is called with source code since the mode line is still shown."
  (if side
      (demo-it-make-side-window side))
  (find-file file)
  (if size (text-scale-set size)
           (text-scale-set 1)))

;; Load a File and Fancily Highlight Some Lines
;;
;;    Would be nice to load up a file and automatically highlight some
;;    lines.

(defun demo-it-load-fancy-file (file type line1 line2 &optional side size)
  "Load FILE and use fancy narrow to highlight part of the buffer.  If TYPE is 'char, LINE1 and LINE2 are position in buffer, otherwise LINE1 and LINE2 are start and ending lines to highlight.  If SIDE is non-nil, the buffer is placed in a new side window, either 'below or to the 'side, and SIZE is the text scale, which defaults to 1."
  (demo-it-load-file file side size)

  ; If fancy-narrow hasn't been installed, this behaves
  ; just like demo-it-load-file
  (when (fboundp 'fancy-narrow-to-region)
    (let ((start line1)
          (end line2))
      (unless (eq type 'char)
        (goto-char (point-min)) (forward-line (1- line1))  ;; Heh: (goto-line line1)
        (setq start (point))
        (goto-char (point-min)) (forward-line line2)
        (setq end (point)))
      (fancy-narrow-to-region start end))))


;; Display an Image (or other non-textual scaled file) on the Side

(defun demo-it-show-image (file &optional side)
  "Load FILE as image (or any other special file) replacing the current buffer.  If SIDE is non-nil, the image is shown in another window, either 'below or to the 'side."
  (demo-it-load-file file side)
  (fringe-mode '(0 . 0))
  (demo-it-hide-mode-line))


;; Compare and Contrast Files
;;
;;   Places two files next to each other so that you can diff
;;   or at least visually compare them. I suppose that after
;;   they are loaded, you can switch to them with something like:
;;      (pop-to-buffer "example.py")
;;   To further manipulate them.

(defun demo-it-compare-files (file1 file2 &optional side size)
  "Load FILE1 and FILE2 as either two windows on top of each other on the right side of the screen, or two windows below (depending on the value of SIDE).  The SIZE specifies the text scaling of both buffers."
  (if (eq side 'below)
      (progn
        (demo-it-load-file file1 'below size)
        (demo-it-load-file file2 'side size))
    (progn
      (demo-it-load-file file1 'side size)
      (demo-it-load-file file2 'below size))))


;; Start an Eshell and Run Something
;;
;;    This function assumes you want an Eshell instance running in the
;;    lower half of the window. Changes to a particular directory, and
;;    automatically runs something.

(defun demo-it-start-eshell (&optional directory command name side size)
  "Start Eshell instance, and change to DIRECTORY to execute COMMAND.  NAME optionally labels the buffer.  SIDE can be either 'below or to the 'side, and SIZE specifies the text scale, which defaults to 1 level larger."
  (let ((title (demo-it--eshell-buffer-name name)))
    (demo-it-make-side-window side)
    (eshell "new")
    (rename-buffer title)
    (if size (text-scale-set size)
      (text-scale-set 1))

    (when directory
      (insert (concat "cd " directory))
      (eshell-send-input))
    (erase-buffer)
    (eshell-send-input)

    (when command
      (insert command)
      (eshell-send-input))))

(defun demo-it--eshell-buffer-name (name)
  "Return the buffer NAME for the EShell window."
  (if name
      (concat "Shell: " name)
    "Shell"))

(defun demo-it-show-eshell (&optional name side)
  "Call if the shell window of a given NAME has been
hidden. Optionally specify the SIDE (either 'below or 'side)."
  (demo-it-make-side-window side)
  (switch-to-buffer (demo-it--eshell-buffer-name name)))

(defun demo-it-run-in-eshell (command &optional name)
  "Run shell command COMMAND in a previously initialized Eshell.
If NAME is not specified, it defaults to `Shell'."
  (switch-to-buffer (demo-it--eshell-buffer-name name))
  (insert command)
  (eshell-send-input))

(defun demo-it-type-in-eshell (command &optional name)
  "Type slowly and run shell command COMMAND in a previously initialized Eshell.
If NAME is not specified, it defaults to `Shell'."
  (switch-to-buffer (demo-it--eshell-buffer-name name))
  (demo-it-insert-typewriter command)
  (eshell-send-input))

;; Title Display
;;
;;    Create a file to serve as a "title" as it will be displayed with a
;;    larger-than-life font.

(defun demo-it-title-screen (file &optional size)
  "Use FILE to serve as a presentation title, as it will be displayed with a larger-than-life font.  SIZE specifies the text scale, which defaults to 5x."
  (delete-other-windows)
  (fringe-mode '(0 . 0))

  (find-file file)
  (show-all)
  (demo-it-hide-mode-line)
  (setq cursor-type nil)
  (if (fboundp 'flyspell-mode)
      (flyspell-mode -1))
  (variable-pitch-mode 1)
  (if size (text-scale-set size)
    (text-scale-set 5))

  (message "%s" "† This presentation is running within Emacs."))

;; Starting an ORG Presentation
;;
;;    Since I often have an org-mode file on the side of the screen to
;;    demonstrate an outline of what I will be demoing, I made it a
;;    function.

;;    Uses org-tree-slide if available.
;;    See https://github.com/takaxp/org-tree-slide

(defvar demo-it--presentation-file "")
(defvar demo-it--presentation-buffer nil)
(defvar demo-it--presentation-prev-settings (make-hash-table))

(defun demo-it-presentation (file &optional size style section)
  "Load FILE (org-mode?) as presentation.  Start org-tree-slide
if available.  SIZE specifies the text scale, and defaults to 2
steps larger. STYLE can either be :variable for variable pitch of
the font, :blocks for diminished headers on org-blocks, or :both
for both features.

The SECTION is the name of an org-mode header to specify as the
first section to display."
  (find-file file)
  (setq demo-it--presentation-file file)
  (setq demo-it--presentation-buffer (buffer-name))

  (when (fboundp 'org-tree-slide-mode)
    (setq org-tree-slide-heading-emphasis t)
    (org-tree-slide-mode)
    (when section
      (demo-it--presentation-section section)))

  (when (fboundp 'flyspell-mode)
    (flyspell-mode -1))
  (setq cursor-type nil)

  ;; Style things up correctly...
  (make-local-variable 'demo-it--presentation-prev-settings)
  (puthash :emphasis-markers org-hide-emphasis-markers demo-it--presentation-prev-settings)
  (setq org-hide-emphasis-markers t)

  (when (or (eq style :variable) (eq style :both))
    (variable-pitch-mode 1))

  ;; Make the display of the org-mode file more presentable
  (when (or (eq style :block) (eq style :both))
    (demo-it--presentation-display-set))

  (demo-it-hide-mode-line)
  (if size (text-scale-set size)
    (text-scale-set 2))

  (when (fboundp 'org-bullets-mode)
    (org-bullets-mode 1)))

(defun demo-it--presentation-display-set ()
  "Change some typical `org-mode' display values to make more
presentation-friendly.  Store the changed values in a hashtable.
See `demo-it--presentation-display-restore'."
  ;; Save everything that is interesting into a hash table:
  (puthash :restore t demo-it--presentation-prev-settings)
  ;; Allow us to resize our images:
  (setq org-image-actual-width nil)
  (let* ((backgd (face-attribute 'default :background))
         (border (list (list :foreground backgd :background backgd :height 1))))
    (cl-flet ((set-attr (attr values) (puthash attr
                                               (face-remap-add-relative attr values)
                                               demo-it--presentation-prev-settings)))
      (set-attr 'org-block-begin-line border)
      (set-attr 'org-block-end-line   border)
      (set-attr 'org-meta-line        border)
      (set-attr 'org-special-keyword  border)
      (set-attr 'org-block            '((:family "monospace")))
      (set-attr 'org-verbatim         '((:family "monospace")))
      (set-attr 'org-code             '((:family "monospace")))
      (set-attr 'org-table            '((:family "monospace")))
      (set-attr 'org-special-keyword  '((:family "monospace"))))))

(defun demo-it--presentation-display-restore ()
  "After `demo-it--presentation-display-set', call to restore previous settings."
  (setq org-hide-emphasis-markers
        (gethash :emphasis-markers demo-it--presentation-prev-settings))
  (when (gethash :restore demo-it--presentation-prev-settings)
    (remhash :restore demo-it--presentation-prev-settings)
    (cl-flet ((rest-attr (attr) (face-remap-remove-relative
                                 (gethash attr demo-it--presentation-prev-settings))))
      (mapcar #'rest-attr (list 'org-block-begin-line 'org-block-end-line 'org-block 'org-meta-line
                                'org-verbatim 'org-code 'org-table 'org-special-keyword)))))

;; Specify a section in the presentation

(defun demo-it--presentation-section (section)
  "Moves the displayed presentation to a SECTION header."
  (interactive "s")
  (when demo-it--presentation-buffer
    (switch-to-buffer demo-it--presentation-buffer)
    (org-tree-slide-content)
    (goto-char (point-min))
    (re-search-forward (format "^\*+ +%s" section))
    (org-tree-slide-move-next-tree)))

;; Jumping Back to the Presentation
;;
;;    In this case, we've been doing some steps, and the screen is
;;    "messed up", calling this function returns back to the
;;    presentation.

(defun demo-it-presentation-return-noadvance ()
  "Return to the presentation buffer and delete other windows."
  (when demo-it--presentation-buffer
    (switch-to-buffer demo-it--presentation-buffer))
  (delete-other-windows))

(defun demo-it-presentation-return ()
  "Return to the presentation buffer, delete other windows, and advance to the next 'org-mode' section."
  (when demo-it--presentation-buffer
    (demo-it-presentation-return-noadvance)
    (when (fboundp 'org-tree-slide-move-next-tree)
      (org-tree-slide-move-next-tree))))

(defun demo-it-single-presentation (file &optional size style section)
  "Demonstration that presents on `org-mode' FILE as a full-screen presentation."
  (interactive "fPresentation File: ")
  (cl-flet ((present-it ()
                        (demo-it-frame-fullscreen)
                        (delete-other-windows)
                        (demo-it-presentation file size style section)))
    (demo-it-start (list #'present-it) t)))

;; Advance Presentation without Changing Focus
;;
;;    Advances the org-mode presentation, but after popping into that
;;    presentation buffer, returns to the window where our focus was
;;    initially.

(defun demo-it-presentation-advance ()
  "Advance the presentation to the next frame (if the buffer is an 'org-mode' and 'org-tree-slide' is available), but doesn't change focus or other windows.  Only useful if using the org-tree-slide mode for the presentation buffer."
  (interactive)
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer)))
      (switch-to-buffer demo-it--presentation-buffer)
      (when (fboundp 'org-tree-slide-move-next-tree)
        (org-tree-slide-move-next-tree))
      (switch-to-buffer orig-window))))

(defun demo-it--presentation-highlight-phrase (phrase &optional color)
  "Highlight a PHRASE (based on a regular expression) in the
presentation buffer. This is useful to highlight bullet point
items while executing appropriate code."
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer))
          (hilite-color (if (null color) 'hi-green-b color)))
      (switch-to-buffer demo-it--presentation-buffer)
      (hi-lock-unface-buffer t)
      (hi-lock-face-phrase-buffer phrase hilite-color)
      (switch-to-buffer orig-window))))

(defun demo-it--presentation-unhighlight-all ()
  (when demo-it--presentation-buffer
    (let ((orig-window (current-buffer)))
      (switch-to-buffer demo-it--presentation-buffer)
      (hi-lock-unface-buffer t)
      (switch-to-buffer orig-window))))

;; Clean up the Presentation
;;
;;    The org-presentation-start function alters the way an org-mode file
;;    is displayed. This function returns it back to a normal, editable
;;    state.

(defun demo-it-presentation-quit ()
  "Undo display settings made to the presentation buffer."
  (interactive)
  (demo-it--setq-restore)
  (when demo-it--presentation-buffer
    (switch-to-buffer demo-it--presentation-buffer)
    (when (fboundp 'org-tree-slide-mode)
      (org-tree-slide-mode -1))
    (when (fboundp 'flyspell-mode)
      (flyspell-mode t))
    (setq cursor-type t)
    (demo-it--presentation-display-restore)  ; Restore previous changes
    (variable-pitch-mode nil)
    (demo-it-show-mode-line)
    (text-scale-set 0)))

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

;; Temporary variables
;;
;;    Set variables during a demonstration.
;;    They are restored after the demonstration.

(defvar demo-it--setq-tempvars (make-hash-table))
(defvar demo-it--setq-voidvars nil)
(defvar demo-it--setq-nilvars nil)
(defun demo-it--setq (l)
  (cl-case (length l)
    (0)
    (1
     (error "Argument length is odd"))
    (t
     (let ((name (car l))
           (var  (eval (cadr l))))
       (cond ((and (boundp name) (symbol-value name))
              (puthash name (symbol-value name) demo-it--setq-tempvars))
             ((boundp name)
              (push name demo-it--setq-nilvars))
             (t
              (push name demo-it--setq-voidvars)))
       (set name var)
       (demo-it--setq (nthcdr 2 l))))))

(defmacro demo-it-setq (&rest list)
  "Like `setq', but the values are restored to original after the demo.
Actually restored by `demo-it--setq-restore'."
  `(demo-it--setq '(,@list)))

(defun demo-it--setq-restore ()
  "Restore values of setting by `demo-it-setq'."
  (cl-loop for name being the hash-keys in demo-it--setq-tempvars using (hash-values value)
           do (set name value))
  (dolist (name demo-it--setq-nilvars) (set name nil))
  (mapc 'makunbound demo-it--setq-voidvars)
  (clrhash demo-it--setq-tempvars)
  (setq demo-it--setq-nilvars nil
        demo-it--setq-voidvars nil))

;; Helper Functions

(defvar demo-it-text-entries (make-hash-table)
  "Collection of insertable text and keys for `demo-it-insert-text'.
Strings to insert Assign a collection of characters as keys and
strings, and call the `C-c i` to insert the text string as if you
were typing it.

For instance:
   (setq demo-it-text-entries #s(hash-table data
                         (?1 \"How about that?\"
                          ?2 \"Nah, this really ain't it.\")))")

(defun demo-it-insert-text (key)
  "Insert text into the current buffer based on a single character KEY.

The text is inserted as if you were typing it.  Make sure the
`demo-it-text-entries' hash-table has been initialized with a
character to be used as a key, and the text to insert."
  (interactive "cInsert text from which key?")
  (demo-it-insert-typewriter (gethash key demo-it-text-entries)))

(defun demo-it-insert-typewriter (str)
  "Insert STR into the current buffer as if you were typing it by hand."
  (interactive "s")
  (dolist (ch (string-to-list str))
    (insert ch)
    (sit-for (/ 1.0 (+ 10 (random 100))) nil)))


(defun demo-it-message-keybinding (key command)
  "Display message showing the KEY keybinding and its COMMAND."
  (interactive)
  (message "Typed: '%s' Command: '%s'" key command))

;; Demo Mode
;;
;;   Allows us to advance to the next step by pressing the
;;   space bar or return. Press the 'q' key to stop the mode.

(defun demo-it-disable-mode ()
  "Called when 'q' pressed to disable the 'demo-it-mode'."
  (interactive)
  (demo-it-mode -1)
  (demo-it-mode-adv -1))

(define-minor-mode demo-it-mode "Pressing 'space' advances demo."
  :lighter " demo"
  :require 'demo-it
  :global t
  :keymap '((" "               . demo-it-step)
            (""              . demo-it-step)
            ("[down]"          . demo-it-step)
            ("[mouse-1]"       . demo-it-set-mouse-or-advance)
            ([nil mouse-1]     . demo-it-step)
            ([nil wheel-up]    . demo-it-ignore-event)
            ([nil wheel-down]  . demo-it-ignore-event)
            ([nil wheel-left]  . demo-it-ignore-event)
            ([nil wheel-right] . demo-it-ignore-event)
            ("q"               . demo-it-disable-mode)
            ("Q"               . demo-it-end)))

(define-minor-mode demo-it-mode-adv "Pressing '<f12>' advances demo."
  :lighter " demo-adv"
  :require 'demo-it
  :global  t
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd   "<f12>") 'demo-it-step)
             (define-key map (kbd "s-<f12>") 'demo-it-insert-text)
             (define-key map (kbd "A-<f12>") 'demo-it-insert-text)
             (define-key map (kbd "M-<f12>") 'demo-it-end)
             map))

;; New Keybindings
;;
;;   I have found the following keybindings quite useful, but your mileage may vary.

(defun demo-it-keybindings ()
  "Add a few global keybindings if some other packages are installed.
You probably want to look at the source, and create
your own version of this, but it does the following:

- C-=  Selects or increases the region using expand-region
- M-C-=  Highlights the region (dimming the rest) using fancy-narrow
- M-C-+  Unhighlights buffer (by colorizing entire buffer) using fancy-narrow

This function should never had been written, as it is better to
just add your own keybindings to the demo-it-mode instead, as in:

   (define-key demo-it-mode-map (kbd \"C-<f12>\") 'demo-it-presentation-advance)"
  (declare (obsolete "Add keybinds to demo-it-mode directly." "Dec 2015"))
  (interactive)

  (when (fboundp 'er/expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region))
  (when (fboundp 'fancy-widen)
    (global-set-key (kbd "M-C-=") 'highlight-section)
    (global-set-key (kbd "M-C-+") 'fancy-widen)))

;;   As a final harrah, we need to let other files know how to include
;;   this bad child.

(provide 'demo-it)

;;; demo-it.el ends here

;;; demo-it.el --- Create demonstrations
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright (C) 2014  Howard Abrams
;; Keywords: demonstration presentation test
;; Version: 2.0.0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
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
;;   Using the library is a four step process:
;;
;;   1. Load the library in your own Elisp source code file
;;   2. Create a collection of functions that "do things".
;;   3. Create the ordered list of functions/steps with `demo-it-create'
;;   4. Start the demonstration with `demo-it-start'
;;
;;   For instance:
;;
;;       (require 'demo-it)   ;; Load this library of functions
;;
;;       (defun dit-load-source-code ()
;;         "Load some source code in a side window."
;;         (demo-it-presentation-advance)
;;         (demo-it-load-fancy-file "example.py" :line 5 12 :side))
;;
;;       (defun dit-run-code ()
;;         "Execute our source code in an Eshell buffer."
;;         ;; Close other windows and advance the presentation:
;;         (demo-it-presentation-return)
;;         (demo-it-start-shell)
;;         (demo-it-run-in-shell "python example.py Snoopy"))
;;
;;       (demo-it-create :single-window :insert-slow :full-screen
;;                       (demo-it-title-screen "example-title.org")
;;                       (demo-it-presentation "example.org")
;;                        dit-load-source-code
;;                        dit-run-code
;;                       (demo-it-run-in-shell "exit" nil :instant))
;;
;;       (demo-it-start)
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
;;
;;; Code:

(require 'cl-lib)

;; Predefined necessary external functions:
(defvar org-image-actual-width)

;; And functions from other projects I like to use...
(declare-function eshell-send-input "ext:eshell")
(declare-function show-all "ext:eshell.c")

;; Load our 'modules' from other files:
(require 'demo-it-custom)

;; And specify the customization variables set in that module:
(defvar demo-it--shell-or-eshell)
(defvar demo-it--keymap-mode-style)
(defvar demo-it--insert-text-speed)
(defvar demo-it--open-windows)
(defvar demo-it--open-windows-size)
(defvar demo-it--text-scale)
(defvar demo-it--start-fullscreen)
(defvar demo-it--start-single-window)
(declare-function demo-it--get-insert-text-speed "demo-it-custom.el")
(declare-function demo-it--get-text-scale "demo-it-custom.el")
(declare-function demo-it--set-property "demo-it-custom.el")


;; ----------------------------------------------------------------------
;; MINOR MODES
;;
;;   We define two styles of minor modes for dealing with special keys
;;   to advance the demonstration along ... a simple uses space and
;;   return, and an 'advanced' mode that requires a special function
;;   key...

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
             (define-key map (kbd "C-<f12>") 'demo-it-highlight-dwim)
             (define-key map (kbd "M-<f12>") 'demo-it-end)
             map))

;; Demo Mode
;;
;;   Allows us to advance to the next step by pressing the
;;   space bar or return. Press the 'q' key to stop the mode.

(defun demo-it-disable-mode ()
  "Called when 'q' (or similar key) pressed to disable either the
simple or advanced version of `demo-it-mode'."
  (interactive)
  (demo-it-mode -1)
  (demo-it-mode-adv -1))

;; ----------------------------------------------------------------------
;; DEMONSTRATION MAKER
;;
;;   When we start a demonstration, we would pass in a list of functions
;;   to call for each step, and then call =demo-step= to execute the
;;   first one on the list.

;; A "global" variable (shudder) to track state of the demonstration:
(defvar demo-it--step 0  "Stores the current demo 'step' function.")
(defvar demo-it--steps '() "List of functions to be executed in order.")
(defvar demo-it-start-winconf nil
  "Window configuration when starting demo. Used to restore
  buffer positions after demonstration is complete.")

;;;###autoload
(defun demo-it-start (&optional steps advanced-mode)
  "Start the current demonstration and kick off the first step.
STEPS is a list of functions or keystrokes to execute.
If nil, the STEPS must be specified by a call to `demo-it-create'.

The optional ADVANCED-MODE turns on keybindings where <F12>
advances the steps instead of Space.  This mode is better for
more interactive demonstrations."
  (interactive)
  (when (or demo-it-mode demo-it-mode-adv)
    (error "Do not start new demonstrations DURING demonstration"))

  (setq demo-it-start-winconf (current-window-configuration))
  (setq demo-it--step 0)      ;; Reset the step to the beginning
  (when steps
    (setq demo-it--steps steps)) ;; Store the steps.

  (if (or advanced-mode (eq demo-it--keymap-mode-style :advanced-mode))
      (demo-it-mode-adv t)
    (demo-it-mode t))

  (if demo-it--start-fullscreen
      (demo-it-frame-fullscreen))
  (if demo-it--start-single-window
      (delete-other-windows))

  (demo-it-step))

;;;###autoload
(defmacro demo-it-create (&rest forms)
  "Create and store an ordered list of steps and configuration
values. The FORMS can be either function names, expressions or
keywords, like `:advanced-mode' and `:variable-width'."
  `(progn
     (demo-it--set-properties (cl-remove-if-not 'keywordp '(,@forms)))
     (setq demo-it--steps     (cl-remove-if     'keywordp '(,@forms)))))

(defun demo-it--set-properties (l)
  "Sets a series of single property values from list, L."
  ;; First, save all the customization properties...
  (demo-it-setq-save demo-it--keymap-mode-style
                     demo-it--shell-or-eshell
                     demo-it--open-windows
                     demo-it--text-scale
                     demo-it--start-fullscreen
                     demo-it--start-single-window
                     demo-it--insert-text-speed)
  ;; Second, set all the new customization properties:
  (mapc 'demo-it--set-property l))

(defun demo-it-end ()
  "End the current demonstration by resetting the values
inflicted on the presentation buffer as well as closing other
windows."
  (interactive)
  (demo-it-disable-mode)
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
      (read-event "Demonstration finished. Hit any key to return.")
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
  "Executes F-STEP depending on its type, e.g. expression, function, etc."
  (condition-case err
      (cond ((functionp f-step) (funcall f-step))
            ((listp     f-step) (eval f-step))     ; An expression
            ((stringp   f-step) (execute-kbd-macro (kbd f-step)))
            ((keywordp  f-step) (demo-it--set-property f-step))
            (t                  (error "invaid step: %s" f-step)))
    (error (read-event (format "Abort the demonstration because of error. Hit any key to return.\n%S" err))
           (demo-it-end))))

;; Position or advance the slide? Depends...

(defun demo-it-set-mouse-or-advance (evt)
  "Advances to the next step if clicked on the right side of any
window, otherwise, it position the point as expected.  With EVT,
function can be bound to the mouse click."
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
  "Empty function that absorbs the EVT parameter to keep
demonstration from flipping out."
  (interactive "P")
  (message ""))


;; ----------------------------------------------------------------------
;; HIDE MODELINE
;;
;;    Call the `demo-it-hide-mode-line' when displaying images and
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


;; ----------------------------------------------------------------------
;; SIDE WINDOWS and HELPER FILES
;;
;;    Typically, we make a side window that is large enough to have some
;;    fun in, as the main window would serve as little more than an
;;    outline or displaying a presentation.
;;
;;    The `demo-it--make-side-window' function is a helper and
;;    probably won't be called directly, but instead is called by
;;    functions that load a file, image or start a shell...

(defun demo-it--make-side-window (&optional side size)
  "Opens window on the side of the current frame and selects that window.

SIDE is either `:above', `:below', `:left', `:right' or `:side'
and defaults to the value of `demo-it--open-windows' (A value of
`:none' acts as a no-op and doesn't open a window).

SIZE specifies the width of the window if new window is on the
side, or the height if the window is either `:above' or
`:below'."
  ;; Since many functions call this, we need a way to actually /not/
  ;; split the screen, so passing in a `:none' overrides the default.
  (when (not (or (eq side 'none) (eq side :none)))

    (select-window (demo-it--new-root-window
                    (or side demo-it--open-windows)
                    (or size demo-it--open-windows-size)))))

;; Since the `make-side-window' shouldn't be called, it has two
;; dashes, but to maintain backward compatibility, we make an alias:
(define-obsolete-function-alias 'demo-it-make-side-window
  'demo-it--make-side-window "2016-Oct")

(defun demo-it--new-root-window (direction size)
  "Split the main window in the DIRECTION where DIRECTION is a
symbol with possible values of 'right, 'left, 'above or 'below
and SIZE is the final size of the windows, if the window is split
horizontally (i.e. DIRECTION 'below or 'above) SIZE is assumed to
be the target height otherwise SIZE is assumed to be target
width."
  (let* ((dir (pcase direction   ;; Create synonyms!?
                (:left  'left)
                (:right 'right)
                (:side  'right)
                (:below 'below)
                (:above 'above)
                (_      direction)))
         (new-window (split-window (frame-root-window) nil dir))
         (horizontal (member dir '(right left))))
    (save-excursion
      (select-window new-window)
      (enlarge-window (- size (if horizontal
                                  (window-width)
                                (window-height)))
                      horizontal))
    new-window))

(defun demo-it-load-file (file &optional side size width)
  "Splits root frame into a window and load FILE into it.

SIDE can be `:above' or `:below' (for vertical split) or `:left'
or `:right' (`:side' is a synomym for `:right'), or `:none' for
load the file in the current buffer. This defaults to the
customized value of `demo-it--open-windows'.

SIZE can specify the text font scale, and if `nil', it uses the
value of `demo-it--text-scale'.  The WIDTH specifies the size of
this new window, which is either the width of a side window, or
the height if the window is `:above' or `:below'."
  (demo-it--make-side-window side width)
  (find-file file)
  (text-scale-set (demo-it--get-text-scale size)))

(defun demo-it--get-section (type &optional start end)
  "Return tuple of beginning and end of a section of buffer.

If TYPE is :char or 'char, START and END refers to specific
character positions, but if TYPE is :line or 'line, this returns
the point positions as if START and END are line numbers."

  (cond
   ;; LINE ... resets the START and END positions to be character
   ;; positions based on the line numbers given.
   ((or (eq type :line) (eq type 'line))
    (save-excursion
      ;; Re-implementing `goto-line' for the win!
      (goto-char (point-min)) (forward-line (1- start))
      (setq start (point))
      (goto-char (point-min)) (end-of-line end)
      (setq end (point))))

   ;; CHAR ... start and end should already be the positions. Unless
   ;; START or END are null, in which case, we can assume we are
   ;; looking at the end of the buffer.
   ((or (eq type :char) (eq type 'char))
    (when (null start)
      (setq start (point-min)))
    (when (null end)
      (setq end (point-max))))

   ;; REGEXP
   ((or (eq type :regex) (eq type :regexp))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward start)
      (setq start (match-beginning 0))
      (re-search-forward end)
      (setq end (match-end 0))))

   ;; STRING
   ((or (eq type :str) (eq type :string))
    (save-excursion
      (goto-char (point-min))
      (search-forward start)
      (setq start (match-beginning 0))
      (search-forward end)
      (setq end (match-end 0)))))

  (cons start end))

(defun demo-it-load-part-file (file type start end &optional side size width)
  "Splits window and loads FILE, but narrow to particular region.

If TYPE is set to :line, then START and END refers to the first
and last lines to narrow. If TYPE is set to :char, then START and
END refer to specific character positions.

See `demo-it-load-file' for an explanation of SIDE and SIZE.
Also see `demo-it-load-fancy-file' for an alternative version."
  (demo-it-load-file file side size width)
  (let ((positions (demo-it--get-section type start end)))
    (narrow-to-region (car positions) (cdr positions))))

(defun demo-it-show-image (file &optional side size width)
  "Load FILE as image (or any other special file) in another
window without a mode line or fringe.  SIDE can be either :side
or :below, and if `nil', the default is to use the value of
`demo-it--open-windows'."
  (demo-it-load-file file side size width)
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
  "Load FILE1 and FILE2 as either two windows on top of each
other on the side of the screen, or two windows below (depending
on the value of SIDE).  The SIZE specifies the text scaling of
both buffers."
  (if (null side)
      (setq side demo-it--open-windows))
  (if (null size)
      (setq size demo-it--text-scale))

  (cl-flet ((open-other-window (file side size)
               (if (or (eq side 'below) (eq side :below))
                   (split-window-horizontally)
                 (split-window-below))
               (find-file file)
               (text-scale-set (demo-it--get-text-scale size))))
    (demo-it-load-file file2 side size)
    (open-other-window file1 side size)))

;; ----------------------------------------------------------------------
;; SHELL WORK
;;
;;    Kick off a shell in another window, change to a particular
;;    directory, and automatically run something.

(defun demo-it--erase-shell-buffer ()
  "Uses comint-clear-buffer in Emacs >=25, otherwise erase buffer. Better
compatibility with read-only shell-mode prompts"
  (let ((inhibit-read-only t))
    (if (and (fboundp 'comint-clear-buffer)
             (derived-mode-p 'comint-mode))
        (progn
          (comint-clear-buffer)
          t)
      (erase-buffer))))

(defun demo-it-start-shell (&optional directory command name
                                      side size width)
  "Start a shell or eshell instance, and change to DIRECTORY to
execute COMMAND.  NAME optionally labels the buffer, and defaults
to `Shell'.

SIDE can be `:above' or `:below' (for vertical split) or `:left'
or `:right' (`:side' is a synomym for `:right'), or `:none' for
load the file in the current buffer. This defaults to the
customized value of `demo-it--open-windows'.

SIZE can specify the text font scale, and if `nil', it uses the
value of `demo-it--text-scale'.  The WIDTH specifies the size of
this new window, which is either the width of a side window, or
the height if the window is `:above' or `:below'."
  (let* ((title (demo-it--shell-buffer-name name))
         (des-buf (get-buffer title)))

    (demo-it--make-side-window side width)
    (if des-buf
        (demo-it--start-shell-reuse title directory)

      ;; Otherwise, create a new shell buffer...
      (demo-it--start-shell-new title directory size))

    (when command
      (demo-it-insert-shell command))))

(define-obsolete-function-alias 'demo-it-start-eshell 'demo-it-start-shell "2016-Oct")

(defun demo-it--start-shell-new (title directory size)
  "Attempt to create a new shell/eshell with a buffer named TITLE
in a particular DIRECTORY."
  (let ((default-directory (or directory
                               (file-name-directory (buffer-file-name)))))
    (if (eq demo-it--shell-or-eshell :shell)
        (shell title)

      (eshell "new-shell")
      (rename-buffer title))

    (text-scale-set (demo-it--get-text-scale size))

    (unless (demo-it--erase-shell-buffer)
      (demo-it-insert-shell "" :instant))))

(defun demo-it--start-shell-reuse (title directory)
  "Attempt to re-use existing shell/eshell with a buffer named
TITLE in a particular DIRECTORY."
  (switch-to-buffer title)
  (when directory
    (goto-char (point-max))
    (demo-it-insert-shell (format "cd %s" directory) :instant)
    (unless (demo-it--erase-shell-buffer)
      (demo-it-insert-shell "" :instant))))

(defun demo-it--shell-buffer-name (&optional name)
  "Return the buffer NAME for the shell or eshell window."
  (if name
      (concat "Shell: " name)
    "Shell"))

(defun demo-it-insert-shell (command &optional speed)
  "Inserts some text in the given shell or eshell. The optional
SPEED overrides the custom, `demo-it--insert-text-speed'."
  (demo-it-insert command speed)
  (if (eq demo-it--shell-or-eshell :shell)
      (comint-send-input)
    (eshell-send-input)))

(defun demo-it-insert (str &optional speed)
  "Insert STR into the current buffer as if you were typing it by hand.
The SPEED (if non-nil) overrides the default value settings of the
`demo-it--insert-text-speed' custom variable."
  (let ((timings (demo-it--get-insert-text-speed speed)))

    (if (eq timings :instant)
        (insert str)
      (let ((bottom-limit (car timings))
            (top-limit    (cdr timings)))

        ;; If we are not inserting instantaneously, then loop over each
        ;; character in the string with a random delay based on this range:
        (dolist (ch (string-to-list str))
          (insert ch)
          (let ((tm  (+ (/ bottom-limit 1000.0)
                        (/ (random top-limit) 1000.0))))
            (sit-for tm)))))))

(defun demo-it-run-in-shell (command &optional name speed)
  "Run shell command COMMAND in a previously initialized Eshell.
If NAME is not specified, it defaults to `Shell'."
  (pop-to-buffer (demo-it--shell-buffer-name name))
  (goto-char (point-max))
  (demo-it-insert-shell command speed))

(define-obsolete-function-alias 'demo-it-run-in-eshell 'demo-it-run-in-shell "2016-Oct")
(define-obsolete-function-alias 'demo-it-type-in-eshell 'demo-it-run-in-shell "2016-Oct")

(defun demo-it-show-shell (&optional name side width)
  "Show the shell buffer of a given NAME whose buffer may have
been buried and not in visible. Optionally specify the
SIDE (either 'below or 'side) of the screen where the shell
should be shown."
  (demo-it--make-side-window side width)
  (switch-to-buffer (demo-it--shell-buffer-name name)))

(define-obsolete-function-alias 'demo-it-show-eshell 'demo-it-show-shell "2016-Oct")


;; TITLE DISPLAY
;;
;;    Create a file to serve as a "title" as it will be displayed with a
;;    larger-than-life font and make Emacs not look like Emacs.

(defun demo-it-title-screen (file &optional size)
  "Display FILE to serve as the demonstration's title, as it will
be displayed with a larger-than-life font without a mode line,
etc.  SIZE specifies the text scale, which ignores the
`demo-it--text-scale' customization setting and defaults to 5x."
  (fringe-mode '(0 . 0))

  (find-file file)
  (show-all)
  (demo-it-hide-mode-line)
  (setq cursor-type nil)
  (if (fboundp 'flyspell-mode)
      (flyspell-mode -1))
  (variable-pitch-mode 1)
  (text-scale-set (or size (demo-it--get-text-scale :huge)))

  ;; Time to brag a wee bit...
  (message "† This presentation is running within Emacs."))


;; ----------------------------------------------------------------------
;; DEALING WITH FRAME
;;
;;    During a demonstration, it might be nice to toggle between
;;    full screen and "regular window" in a programmatic way:

(defun demo-it-toggle-fullscreen ()
  "Toggle the frame between full screen and normal size. Only
useful for old versions of Emacs, but this code probably won't
work on them anyway."
  (declare (obsolete toggle-frame-fullscreen "2016-Oct"))
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
  "Set the window frame to be exactly half the physical display
screen, and place it on the left side of the screen.  This can be
helpful when showing off some other application."
  (declare (obsolete "Really!? You think this is useful?" "2016-Oct"))
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

(defmacro demo-it-setq-save (&rest list)
  "Saves the original value of a LIST of variables.

Call `demo-it--setq-restore' to restore the original values of
the variables."
  `(mapcar ,(lambda (v) (puthash v (eval v) demo-it--setq-tempvars))
           '(,@list)))

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

(defun demo-it-message-keybinding (key command)
  "Display message showing the KEY keybinding and its COMMAND."
  (interactive)
  (message "Typed: '%s' Command: '%s'" key command))

;; ----------------------------------------------------------------------

(require 'demo-it-present)
(require 'demo-it-extras)

;;   As a final harrah, we need to let other files know how to include
;;   this bad child.

(provide 'demo-it)

;;; demo-it.el ends here

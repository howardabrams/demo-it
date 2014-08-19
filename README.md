When making demonstrations of new products, technologies and other
geekery, I love the versatility of using Emacs to demonstrate the
trifecta of sprint reviews:

  * Presentations explaining the technology and whatnot
  * Source code ... correctly highlighted and interactive
  * Reviewing code using a connected REPL
  * Executing the code in Eshell

However, I don't want to fat-finger, mentally burp, or even delay
gratification for my audience while I laboriously type, so I
predefine each "step" as an Elisp function, and then have `demo-it`
execute each function when I hit the `F6` key.

  * [Learn You Some Lisp for Great Good][yt1]
  * [Emacs: An Introduction for the Curious][yt2]

  Click the following for a quicker example:

  [![Example and Demonstration](http://img.youtube.com/vi/TSprQzowhAQ/0.jpg)][yt3]

Using this project is a three step process:

  1. Load the library in your own Elisp source code file
  2. Create a collection of functions that "do things".
  3. Call the `demo-it-start` function with the ordered list of functions.

For instance:

    (load-library "demo-it")   ;; Load this library of functions

    ;; Define the first of many steps that do things.
    (defun my-demo/step-1 ()
      (delete-other-windows)
      (demo/org-presentation "~/presentations/emacs-demo/emacs-demo-start.org"))

    (defun my-demo/step-2 ()
      (demo-it-load-side-window "~/Work/my-proj/src/my-proj.py")
      (demo-it-presentation-advance))

    ;; Wrap the collection of functions in another function...
    (defun my-demo ()
       "My fabulous demonstration."
       (interactive)
       (demo-start (list
                       'my-demo/step-1
                       'my-demo/step-2
                       ;; ...
                     )))

    (my-demo) ;; Optionally start the demo when file is loaded.

Each "step" is an Elisp functions that does something like load some
source code, or show off an org-mode file as a presentation.  While
this package has a collection of helping functions, the steps can use
any Elisp command to show off a feature.

For a more complete example, see [example.el][e].

I recommend installing these other Emacs packages:

  * [org-tree-slide-mode][1]
  * [org-bullets][2]
  * [expand-region][3]
  * [fancy-narrow][4]


  [yt1]: http://www.youtube.com/watch?v=3T00X_sNg4Q
  [yt2]: http://www.youtube.com/watch?v=B6jfrrwR10k
  [yt3]: http://www.youtube.com/watch?v=TSprQzowhAQ
  [e]: example/example.el
  [1]: https://github.com/takaxp/org-tree-slide
  [2]: https://github.com/sabof/org-bullets
  [3]: https://github.com/magnars/expand-region.el
  [4]: https://github.com/Bruce-Connor/fancy-narrow

;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Apr 01 17:09:00 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; sdl3-package.lisp -- export exported symbols from sdl2 which are
;;; present.

(in-package "CLAW-CXX-SDL3-USER")
(cl-user::package-add-nicknames "CLAW-CXX-SDL3-USER" "SDL3")

#+nil
(defpackage "DUMMY-READER" (:use))

#+nil
(defun get-exported-symbols (sdl2-package-file &key test-name collect-all
			     (package (find-package "DUMMY-READER")))
  (with-open-file (stream sdl2-package-file)
    (loop with *package* = package
	  and *read-eval* = nil
	  for form = (read stream nil 'endp)
	  while (not (eq form 'endp))
	  if (and (string= "DEFPACKAGE" (car form))
		  (or (null test-name)
		      (funcall test-name (cadr form))))
	  append (cdr (assoc :export (cddr form)))
	  and unless collect-all do (loop-finish))))

#+nil
(progn
(defvar $exported
  (get-exported-symbols  "~/cl/extern/Github/cl-sdl2/src/package.lisp"))
(defvar $funcs nil)
(defvar $vars nil)
(defvar $classes nil)
(defvar $rest nil)
(mapcar (lambda (x) (set x nil)) '($funcs $vars $classes $rest))
(loop for sym in $exported
      for s = (find-symbol (symbol-name sym) "CLAW-CXX-SDL3-USER")
      when s
      do (when (boundp s) (pushnew s $vars))
      (when (fboundp s) (pushnew s $funcs))
      (when (find-class s nil) (pushnew s $classes))
      (unless (or (boundp s) (fboundp s) (find-class s nil))
	(pushnew s $rest)))
(mapcar 'length (list $funcs $vars $classes $rest))
(length $exported)
(let ((*print-right-margin* 10))
  (pprint (sort (mapcar 'symbol-name (union $funcs (union $vars $classes)))
		#'string<)))
(export (union $funcs (union $vars $classes)) "SDL3"))

(eval-when (load eval compile)
(export
 (mapcar (lambda (a &aux (s (find-symbol a "CLAW-CXX-SDL3-USER")))
	   (assert s nil "~S not found in CLAW-CXX-SDL3-USER")
	   s)
	 '("ADD-TIMER"
	   "BLIT-SCALED"
	   "BLIT-SURFACE"
	   "CONVERT-SURFACE"
	   "COPY-F-RECT"
	   "COPY-INTO-F-RECT"
	   "COPY-INTO-POINT"
	   "COPY-INTO-RECT"
	   "COPY-POINT"
	   "COPY-RECT"
	   "CREATE-RENDERER"
	   "CREATE-RGB-SURFACE"
	   "CREATE-RGB-SURFACE-FROM"
	   "CREATE-RGB-SURFACE-WITH-FORMAT-FROM"
	   "CREATE-SOFTWARE-RENDERER"
	   "CREATE-TEXTURE"
	   "CREATE-TEXTURE-FROM-SURFACE"
	   "CREATE-WINDOW"
	   "CREATE-WINDOW-AND-RENDERER"
	   "DELAY"
	   "DESTROY-RENDERER"
	   "DESTROY-TEXTURE"
	   "DESTROY-WINDOW"
	   "DISABLE-SCREENSAVER"
	   "ENABLE-SCREENSAVER"
	   "EXPAND-HANDLER"
	   "F-RECT-EMPTY"
	   "F-RECT-EQUALS"
	   "F-RECTS*"
	   "FILL-RECT"
	   "FREE-EVENT"
	   "FREE-F-RECT"
	   "FREE-POINT"
	   "FREE-RECT"
	   "FREE-SURFACE"
	   "GET-ALPHA-MOD"
	   "GET-COLOR-KEY"
	   "GET-COLOR-MOD"
	   "GET-CURRENT-DISPLAY-MODE"
	   "GET-CURRENT-VIDEO-DRIVER"
	   "GET-DISPLAY-BOUNDS"
	   "GET-DISPLAY-MODE"
	   "GET-DISPLAY-NAME"
	   "GET-EVENT-TYPE"
	   "GET-GLOBAL-MOUSE-STATE"
	   "GET-KEY-FROM-SCANCODE"
	   "GET-KEY-NAME"
	   "GET-NUM-DISPLAY-MODES"
	   "GET-NUM-RENDER-DRIVERS"
	   "GET-NUM-VIDEO-DISPLAYS"
	   "GET-NUM-VIDEO-DRIVERS"
	   "GET-PERFORMANCE-COUNTER"
	   "GET-PERFORMANCE-FREQUENCY"
	   "GET-RENDER-DRAW-COLOR"
	   "GET-RENDERER"
	   "GET-RENDERER-MAX-TEXTURE-SIZE"
	   "GET-RENDERER-OUTPUT-SIZE"
	   "GET-TEXTURE-ALPHA-MOD"
	   "GET-TEXTURE-COLOR-MOD"
	   "GET-TICKS"
	   "GET-VIDEO-DRIVER"
	   "GET-WINDOW-ASPECT-RATIO"
	   "GET-WINDOW-DISPLAY-INDEX"
	   "GET-WINDOW-FLAGS"
	   "GET-WINDOW-ID"
	   "GET-WINDOW-PIXEL-FORMAT"
	   "GET-WINDOW-POSITION"
	   "GET-WINDOW-SIZE"
	   "GET-WINDOW-SURFACE"
	   "GET-WINDOW-TITLE"
	   "GL-BIND-TEXTURE"
	   "GL-CREATE-CONTEXT"
	   "GL-DELETE-CONTEXT"
	   "GL-EXTENSION-SUPPORTED-P"
	   "GL-GET-ATTR"
	   "GL-GET-ATTRS"
	   "GL-GET-PROC-ADDRESS"
	   "GL-GET-SWAP-INTERVAL"
	   "GL-MAKE-CURRENT"
	   "GL-SET-ATTR"
	   "GL-SET-ATTRS"
	   "GL-SET-SWAP-INTERVAL"
	   "GL-SWAP-WINDOW"
	   "GL-UNBIND-TEXTURE"
	   "GLOBAL-MOUSE-STATE-P"
	   "HAS-INTERSECT"
	   "HIDE-CURSOR"
	   "HIDE-WINDOW"
	   "IN-MAIN-THREAD"
	   "INIT"
	   "INIT*"
	   "INTERSECT-RECT"
	   "INTERSECT-RECT-AND-LINE"
	   "KEY-DOWN-P"
	   "KEY-UP-P"
	   "KEYBOARD-STATE-P"
	   "LOAD-BMP"
	   "LOCK-TEXTURE"
	   "MAKE-F-RECT"
	   "MAKE-POINT"
	   "MAKE-RECT"
#+nil	   "MAKE-THIS-THREAD-MAIN"
	   "MAXIMIZE-WINDOW"
	   "MINIMIZE-WINDOW"
	   "MOD-KEYWORDS"
	   "MOD-VALUE-P"
	   "MOUSE-STATE"
	   "MOUSE-STATE-P"
	   "NEW-EVENT"
	   "NEXT-EVENT"
	   "POINTS*"
	   "PUMP-EVENTS"
	   "PUSH-EVENT"
	   "PUSH-QUIT-EVENT"
	   "PUSH-USER-EVENT"
	   "QUIT"
	   "QUIT*"
	   "RAISE-WINDOW"
	   "RECT-EMPTY"
	   "RECT-EQUALS"
	   "RECTS*"
	   "REGISTER-USER-EVENT-TYPE"
	   "REMOVE-TIMER"
	   "RENDER-CLEAR"
	   "RENDER-COPY"
	   "RENDER-COPY-EX"
	   "RENDER-COPY-EX-F"
	   "RENDER-COPY-F"
	   "RENDER-DRAW-LINE"
	   "RENDER-DRAW-LINES"
	   "RENDER-DRAW-POINT"
	   "RENDER-DRAW-POINTS"
	   "RENDER-DRAW-RECT"
	   "RENDER-DRAW-RECTS"
	   "RENDER-FILL-RECT"
	   "RENDER-FILL-RECTS"
	   "RENDER-GET-VIEWPORT"
	   "RENDER-PRESENT"
	   "RENDER-SET-VIEWPORT"
	   "RESTORE-WINDOW"
	   "SCANCODE-KEY-NAME"
	   "SCANCODE-KEY-TO-VALUE"
	   "SCANCODE-NAME"
	   "SCANCODE-SYMBOL"
	   "SCANCODE="
	   "SCREENSAVER-ENABLED-P"
	   "SDL-CONTINUE"
	   "SDL-QUIT"
	   "SET-ALPHA-MOD"
	   "SET-COLOR-KEY"
	   "SET-COLOR-MOD"
	   "SET-RENDER-DRAW-BLEND-MODE"
	   "SET-RENDER-DRAW-COLOR"
	   "SET-RENDER-TARGET"
	   "SET-TEXTURE-ALPHA-MOD"
	   "SET-TEXTURE-BLEND-MODE"
	   "SET-TEXTURE-COLOR-MOD"
	   "SET-WINDOW-FULLSCREEN"
	   "SET-WINDOW-POSITION"
	   "SET-WINDOW-SIZE"
	   "SET-WINDOW-TITLE"
	   "SHOW-CURSOR"
	   "SHOW-CURSOR-P"
	   "SHOW-WINDOW"
	   "START-TEXT-INPUT"
	   "STOP-TEXT-INPUT"
	   "SURFACE-FORMAT"
	   "SURFACE-FORMAT-FORMAT"
	   "SURFACE-HEIGHT"
	   "SURFACE-PITCH"
	   "SURFACE-PIXELS"
	   "SURFACE-WIDTH"
	   "TEXTURE-HEIGHT"
	   "TEXTURE-WIDTH"
	   "UNION-RECT"
	   "UNLOCK-TEXTURE"
	   "UPDATE-TEXTURE"
	   "UPDATE-WINDOW"
	   "VERSION"
	   "VERSION-WRAPPED"
	   "WARP-MOUSE-IN-WINDOW"
	   "WAS-INIT"
	   "WINDOWPOS-CENTERED"
	   "WINDOWPOS-FROM-COORD"
	   "WINDOWPOS-UNDEFINED"
	   "WITH-EVENT-LOOP"
	   "WITH-EVERYTHING"
	   "WITH-F-RECTS"
	   "WITH-GL-CONTEXT"
	   "WITH-INIT"
	   "WITH-POINTS"
	   "WITH-RECTS"
	   "WITH-RENDERER"
	   "WITH-SDL-EVENT"
	   "WITH-WINDOW"))
	"CLAW-CXX-SDL3-USER"))

;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sat Apr 26 21:05:25 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; skitter.cl-claw-sdl3.lisp: integrate claw-cxx-sdl3 with skitter
;;;
;;; incorporates code,  Copyright (c) 2016, Baggers,All rights reserved.
;;; from
;;; ~/cl/extern/Github/skitter/sdl2/sdl2.lisp (commit 42a423158)
;;; ~/cl/extern/Github/Authors/cbaggers/cepl.skitter/sdl2.lisp (commit 42a423158)
;;;
(cl:in-package "CL-USER")
(defpackage #:skitter.sdl3
  (:use #:cl "CLAW-CXX-SDL3" "CLAW-CXX-SDL3-USER" #:skitter  #:skitter.internals #:rtg-math)
  (:shadowing-import-from "CLAW-CXX-SDL3"
   "CONDITION" "POSITION" "REM" "CLOSE" "SECOND" "TYPE" "FORMAT" "WRITE" "READ" "MOD" "LENGTH" "PHASE" "REMOVE" "FUNCTION")
  (:shadowing-import-from "SKITTER" "WINDOW" "SET-WINDOW-SIZE" "KEY-DOWN-P")
  (:shadowing-import-from "RTG-MATH" "W" "X" "Y" "Z"))

(in-package #:skitter.sdl3)


;;--------------------------------------------
;; sdl timestamp conversion

;; {TODO} optimize
(let ((sdl->lisp-time-offset 0))
  (defun set-sdl->lisp-time-offset ()
    (setf sdl->lisp-time-offset (cl:- (get-internal-real-time) (sdl3::get-ticks))))
  (defun sdl->lisp-time (sdl-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:+ sdl-time sdl->lisp-time-offset))
  (defun lisp->sdl-time (lisp-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:- lisp-time sdl->lisp-time-offset)))

;;--------------------------------------------
;; sdl event helpers


#||
(mapcar 'symbol-name (cffi:foreign-enum-keyword-list 'sdl3::sdl-event-type))
(every (lambda (x) (user::prefixp "SDL-EVENT-" x))
       (mapcar 'symbol-name (cffi:foreign-enum-keyword-list 'sdl3::sdl-event-type)))

(mapcar (lambda (a)
	  (cons (car a) (mapcar 'cdr (cdr a))))
(user::group2
 (mapcar (lambda (x)
	   (assert (user::prefixp #1="SDL-EVENT-" x))
	   (let* ((beg (cl:length #1#))
		  (end (cl:position #\- x :start beg))
		  (str2 (subseq x beg end)))
	     (cons str2 x)))
	 (mapcar 'symbol-name (cffi:foreign-enum-keyword-list 'sdl3::sdl-event-type)))
 :test #'equalp
 :key #'car
))


||#

(defmacro %case-event ((event) &body event-handlers)
  (assert (symbolp event))
  `(case (sdl3:get-event-type ,event)
     ,@(loop :for (type params . forms) :in event-handlers
          :append (let ((type (if (listp type)
                                  type
                                  (list type))))
                    (loop :for typ :in type :collect
                       (sdl3::%expand-handler event typ params forms)))
          :into results
          :finally (return (cl:remove nil results)))))

;; 2d axis can go down to -32768 but 1d axis can only go up to 32767
(defconstant +axis-norm-factor-2d+ #.(/ 1f0 32768))
(defconstant +axis-norm-factor-1d+ #.(/ 1f0 32767))

(defun on-event (event &optional tpref)
  (%case-event (event)
    (:sdl-event-quit
     (:timestamp ts)
     (set-window-manager-quitting +window-manager+ (sdl->lisp-time ts) t tpref))

;;; HERE
    (:sdl-event-window-moved
     (:timestamp ts :data1 x :data2 y)
     (set-window-pos (skitter:window 0) (sdl->lisp-time ts) (v!int x y) tpref))
    (:sdl-event-window-resized
     (:timestamp ts :data1 x :data2 y)
     (skitter:set-window-size (skitter:window 0) (sdl->lisp-time ts) (v!int x y) tpref))
    (:sdl-event-window-pixel-size-changed
     (:timestamp ts :data1 x :data2 y)
     (skitter:set-window-size (skitter:window 0) (sdl->lisp-time ts) (v!int x y) tpref))
    (:sdl-event-window-minimized
     (:timestamp ts)
     (set-window-layout (skitter:window 0) (sdl->lisp-time ts) :minimized tpref))
    (:sdl-event-window-maximized
     (:timestamp ts)
     (set-window-layout (skitter:window 0) (sdl->lisp-time ts) :maximized tpref))
    (:sdl-event-window-restored
     (:timestamp ts)
     (set-window-layout (skitter:window 0) (sdl->lisp-time ts) :restored tpref))
    (:sdl-event-window-close-requested
     (:timestamp ts)
     (set-window-layout (skitter:window 0) (sdl->lisp-time ts)t tpref))

    (:mouse-wheel (:timestamp ts :which id :x x :y y)
     (let ((mouse (mouse id)))
       (set-mouse-wheel mouse (sdl->lisp-time ts) (v! x y) tpref)))

    ((:mouse-button-down :mouse-button-up)
     (:timestamp ts :which id :button b :x x :y y :down downp)
     ;; what should we do with clicks? (:clicks c)
     (let ((mouse (mouse id)))
       (set-mouse-button mouse b (sdl->lisp-time ts) downp tpref)
       (set-mouse-pos mouse (sdl->lisp-time ts) (v! x y) tpref)))

    (:mouse-motion
     (:timestamp ts :which id :x x :y y :xrel xrel :yrel yrel)
     ;; what should we do with state? (:state s)
     (let ((mouse (mouse id)))
       (set-mouse-pos mouse (sdl->lisp-time ts) (v! x y) tpref)
       (set-mouse-move mouse (sdl->lisp-time ts) (v! xrel yrel) tpref)))

    ((:key-down :key-up)
     (:timestamp ts :scancode scancode :down downp)
     ;; what should we do with repeat (:repeat r)
     (let ((kbd (keyboard 0)))
       ;; calling (break) here corrupts segfaults sbcl
       (set-keyboard-button
        kbd
	(sdl3::scancode-key-to-value scancode) ;madhu 250426 FIXME
        (sdl->lisp-time ts)
	downp
        tpref)))

    ;; madhu 250426 - all the sdl-controller-axis constants are nil in
    ;; my build.
    ((:gamepad-axis-motion)
     (:timestamp ts :which id :axis axis :value value)
     (let ((ts (sdl->lisp-time ts))
           (gpad (gamepad id)))
       (cond
         ((= axis claw-cxx-sdl3:+sdl-controller-axis-leftx+)
          (let ((curr (gamepad-2d gpad 0))
                (val (* (float value 0f0) +axis-norm-factor-2d+)))
            (set-gamepad-2d gpad 0 ts (v2:make val (rtg-math:y curr)) tpref)))
         ((= axis claw-cxx-sdl3:+sdl-controller-axis-lefty+)
          (let ((curr (gamepad-2d gpad 0))
                (val (* (float value 0f0) +axis-norm-factor-2d+)))
            (set-gamepad-2d gpad 0 ts (v2:make (rtg-math:x curr) (- val)) tpref)))

         ((= axis claw-cxx-sdl3:+sdl-controller-axis-rightx+)
          (let ((curr (gamepad-2d gpad 0))
                (val (* (float value 0f0) +axis-norm-factor-2d+)))
            (set-gamepad-2d gpad 1 ts (v2:make val (rtg-math:y curr)) tpref)))
         ((= axis claw-cxx-sdl3:+sdl-controller-axis-righty+)
          (let ((curr (gamepad-2d gpad 0))
                (val (* (float value 0f0) +axis-norm-factor-2d+)))
            (set-gamepad-2d gpad 1 ts (v2:make (rtg-math:x curr) (- val)) tpref)))

         ((= axis claw-cxx-sdl3:+sdl-controller-axis-triggerleft+)
          (let ((val (* (float value 0f0) +axis-norm-factor-1d+)))
            (set-gamepad-1d gpad 0 ts val tpref)))
         ((= axis claw-cxx-sdl3:+sdl-controller-axis-triggerright+)
          (let ((val (* (float value 0f0) +axis-norm-factor-1d+)))
            (set-gamepad-1d gpad 1 ts val tpref)))
         ;; ((= axis claw-cxx-sdl3:+sdl-controller-axis-max+))
         ;; ((= axis claw-cxx-sdl3:+sdl-controller-axis-invalid+))
         )))

    ((:gamepad-button-down
      :gamepad-button-up)
     (:timestamp ts :which id :button b :down downp)
     (let ((ts (sdl->lisp-time ts))
           (gpad (gamepad id)))
       (set-gamepad-button gpad b ts downp tpref)))
    ;; ((:controllerdeviceadded
    ;;   :controllerdeviceremoved
    ;;   :controllerdeviceremapped)
    ;;  ()
    ;;  (print (SDL2:GET-EVENT-TYPE EVENT)))
    ))

(defun collect-sdl-events (win &optional tpref)
  (declare (ignore win))
  (let ((event (sdl3:new-event)))
    (loop :until (null (sdl3:next-event event :poll)) :do
       (on-event event tpref))
    (sdl3:free-event event)))

;;--------------------------------------------
;; intializing

#||
(scancode-key-to-value (sdl3::expand-scancode :capslock))
(cffi:foreign-enum-value 'sdl-scancode :sdl-scancode-count) ;512
(cffi:foreign-enum-value 'sdl-scancode :sdl-scancode-reserved);400
(loop for x in (cffi:foreign-enum-keyword-list 'sdl-scancode)
      for v =  (scancode-key-to-value x)
      for max = 0 then (if (and (/= v 400) (/= v 512)
				(> v max))
			   v max)
      finally (return max))
;; 290
||#

(defun make-skitter-pkg-form (name alist)
  (let ((*package* (or (find-package name) (make-package name :use nil))))
    `(progn
       (cl:defpackage ,name
	 (:use)
	 (:export ,@(mapcar #'cdr alist)))
       ,@(loop for (num . nam) in alist
	       collect `(cl:defparameter ,(cl:intern nam) ,num)))))

(defvar +keys+
  (loop for i from 0 to 280
	with prefix = #1="SDL-SCANCODE-"
	and len = (cl:length #1#)
	for key = (ignore-errors (cffi:foreign-enum-keyword 'sdl-scancode i))
	when key collect (cons i (concatenate 'string "KEY."
					      (subseq (string key) len)))))

(eval (make-skitter-pkg-form "SKITTER.SDL3.KEYS" +keys+))

(defmethod initialize-kind :after ((kind keyboard))
  (loop repeat 290 do (add kind (make-boolean-control))))

(defvar +mouse-buttons+
  (loop for i from 1 to 8
	collect (cons i (concatenate
			 'string "MOUSE."
			 (case i
			   (1 "LEFT") (3 "MIDDLE") (2 "RIGHT")
			   (otherwise (cl:format nil "OTHER~D" i)))))))

(eval (make-skitter-pkg-form "SKITTER.SDL3.MOUSE-BUTTONS" +mouse-buttons+))

(defmethod initialize-kind :after ((kind mouse))
  (loop repeat 8 do (add kind (make-boolean-control))))

(defmethod initialize-kind :after ((pad gamepad))
  ;; add two 2d axis controls
  (add pad (make-vec2-control))
  (add pad (make-vec2-control))
  ;; add two 1d axis controls
  (add pad (make-float-control))
  (add pad (make-float-control))
  ;; 17 is the number of button kinds there are
  (loop :for i :below 17 :do
     (add pad (make-boolean-control))))

;;----------------------------------------------------------------------

(defun enable-background-joystick-events ()
  (cffi:with-foreign-string (on "1")
    (claw-cxx-sdl3:sdl-set-hint
     claw-cxx-sdl3:+sdl-hint-joystick-allow-background-events+
     on)))


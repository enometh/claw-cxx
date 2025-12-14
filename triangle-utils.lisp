;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Tue Apr 01 20:27:37 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; opengl 101 support
;;;
(in-package "CLAW-CXX-SDL3-USER")

(eval-when (load eval compile)
  (export '(reset-main bt-find-threads
	    *win* *gl* *render-fn* *init-fn*
	    runmain stopmain
	    send-buffer-data
	    link-program-from-shader-strings
	    link-program-from-shaders)))

;;; ----------------------------------------------------------------------
;;;
;;; bt thread helpers: kill main thread
;;;

(defun bt-find-threads (name)
  (remove-if-not (lambda (x) (equal (bt:thread-name x) name))
		 (bt:all-threads)))

(defun reset-main ()
  (prog1 (list *the-main-thread* *main-thread* *main-thread-channel*)
    (mapcar 'bt:destroy-thread (bt-find-threads "SDL3 Main Thread"))
    (setq *the-main-thread* nil)
    (setq *main-thread* nil)
    (setq *main-thread-channel* nil)))

#+nil
(reset-main)


;;; ----------------------------------------------------------------------
;;;
;;; runmain and stopmain helpers
;;;

(defvar *win* nil "Current SDL3 window")
(defvar *gl* nil "Current GL Context")

(defvar *request-gl-fn* nil)
(defvar *init-fn* nil)
(defvar *render-fn* nil)

#||
(gl-get-attr :sdl-gl-context-profile-mask)
(gl-get-attr :sdl-gl-context-major-version)
(gl-get-attr :sdl-gl-context-minor-version)
(sdl-get-hint +SDL-HINT-OPENGL-ES-DRIVER+)
(gl-get-attr :sdl-gl-doublebuffer)
||#

(defun request-gl (&rest args &key profile major minor)
  (declare (ignore args))
  (etypecase profile
    (number (unless (find profile '(0 1 2 4))
	      (error "profile ~A should be one of 0 (nil) 1 (:core) 2 (:compat) 4 (:es)"
		     profile)))
    (null (setq profile 0))
    (keyword (setq profile (ecase profile
			     (:compat claw-cxx-sdl3:+sdl-gl-context-profile-compatibility+)
			     (:core claw-cxx-sdl3:+sdl-gl-context-profile-core+)
			     (:es claw-cxx-sdl3:+sdl-gl-context-profile-es+)))))
  (gl-set-attr :sdl-gl-context-profile-mask (or profile 0))
  (gl-set-attr :sdl-gl-context-major-version (or major 2))
  (gl-set-attr :sdl-gl-context-minor-version (or minor 1)))

(defvar *alt-event-handler* nil)

(defun main ()
  (let ((bt:*default-special-bindings*
	 `((*request-gl-fn* . ',*request-gl-fn*)
	   (*alt-event-handler* . ',*alt-event-handler*))))
    (unwind-protect
	 (with-init (:video)
	   (when *request-gl-fn* (funcall *request-gl-fn*))
	   (with-window (win :title "TRIPAD" :flags '(:opengl))
	     (setq *win* win)
	     (with-gl-context (gl win)
	       (setq *gl* gl)
	       (progn
		 (when *init-fn* (funcall *init-fn*))
		 (with-event-loop (:method :poll :cow-catcher *alt-event-handler*)
		   (:key-down (:scancode scancode)
		    (format t "HANDLING KEY DOWN EVENT~%")
		    (when (eql scancode :sdl-scancode-escape)
		      (format t "QUITTING~%")
		      (push-event :sdl-event-quit)))
		   (:idle ()
		    (cond (*render-fn*
			   (funcall *render-fn*))
			  (t (delay 1000))))
		   (:quit () t))))))
      (setq *gl* nil)
      (setq *win* nil)
      (setq *init-fn* nil)
      (setq *render-fn* nil))))

(defun runmain (&key gles-p major minor alt-event-handler)
  (reset-main)
  (mapcar 'bt:destroy-thread (bt-find-threads "background-tripad"))
  (let ((bt:*default-special-bindings*
	 `((*request-gl-fn* . ,(if (or gles-p major minor)
				   (lambda () (request-gl :profile
							  (ecase gles-p
							    ((nil :core :es :compat) gles-p)
							    ((t) :es))
							  :major (or major (and gles-p 3))
							  :minor (or minor (and gles-p 1))))))
	   (*alt-event-handler* . ',alt-event-handler))))
    (bt:make-thread #'main :name "background-tripad")))

(defun stopmain ()
  (in-main-thread () (push-event :sdl-event-quit)))

#||
(runmain)
(eql *win* (in-main-thread () *win*))
(eql *gl* (in-main-thread () *gl*))
(list (gl-get-attr :sdl-gl-context-major-version) (gl-get-attr  :sdl-gl-context-minor-version))
(stopmain)

(in-main-thread ()
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit)
  (sdl-gl-swap-window *win*))
||#



;;; ----------------------------------------------------------------------
;;;
;;; lifecycle app abstraction
;;;

(defpackage "TRI-APP"
  (:use)
  (:export
   "WIN" "GL"
   "LIFECYCLE-MIXIN"
   "SETUP-FN"
   "START"
   "UPDATE-FN"
   "CLEANUP-FN" "DRAW-FN"
   "RESIZE-FN"
   "REQUEST-GL-FN"
   "LAUNCH"
   "SHUTDOWN"
   "RESTART-PIPELINE"
   "*APP*"
   "APPLY-IN-EVENT-LOOP"
   "IN-EVENT-LOOP"))


(defclass tri-app:lifecycle-mixin ()
  ((title :initform "TRIPAD-WINDOW" :initarg :title)
   (x :initform :centered :initarg :x)
   (y :initform :centered :initarg :y)
   (w :initform 800 :initarg :w)
   (h :initform 600 :initarg :h)
   (gles-p :initform nil :initarg :gles-p)
   (sdl-window-flags :initform '(:opengl) :initarg :sdl-window-flags)
   (sdl-init-flags :initform '(:video) :initarg :sdl-init-flags)
   (opengl-version-major :initform 3 :initarg :opengl-major)
   (opengl-version-minor :initform 3 :initarg :opengl-minor)
   (win :initform nil :accessor tri-app:win)
   (gl :initform nil :accessor tri-app:gl)
   (request-gl-p :initform nil :initarg :request-gl-p :accessor request-gl-p)
   (render-fn-p :initform nil :initarg :render-fn-p :accessor render-fn-p)
   (main-thread :initform nil)
   (alt-event-handler-p :initform nil :initarg :alt-event-handler-p
			:accessor alt-event-handler-p)))


(defgeneric tri-app:setup-fn (lifecycle-mixin))

(defgeneric tri-app:draw-fn (lifecycle-mixin)
  (:method :after ((app tri-app:lifecycle-mixin))
   (with-slots (win) app
     (gl-swap-window win))))

(defgeneric tri-app:update-fn (lifecycle-mixin sdl-event))

(defgeneric tri-app:cleanup-fn (lifecycle-mixin))

(defgeneric tri-app:resize-fn (lifecycle-mixin new-w new-h)
  (:method ((app tri-app:lifecycle-mixin) new-w new-h)
   (format t "TRI-APP:RESIZE-FN: ~A" (list new-w new-h))
   (gl:viewport 0 0 new-w new-h)))

(defmethod tri-app:request-gl-fn ((app tri-app:lifecycle-mixin))
  (with-slots (gles-p opengl-version-major opengl-version-minor) app
    (request-gl :profile
		(ecase gles-p
		  ((nil :core :es :compat) gles-p)
		  ((t) :es))
		:major (or opengl-version-major (and gles-p 3))
		:minor (or opengl-version-minor (and gles-p 1)))))


(define-condition tri-app:restart-pipeline (condition) ())

(defmethod tri-app:start ((app tri-app:lifecycle-mixin))
  (assert (/= 0 (register-user-event-type :eval-event)))
  (with-slots (main-thread) app
    (assert (or (null main-thread)
		(not (bt:thread-alive-p main-thread))))
    (setq main-thread (bt:current-thread)))
  (with-slots (win
	       x y w h title sdl-init-flags sdl-window-flags
	       alt-event-handler-p
	       render-fn-p
	       gl request-gl-p)
      app
    (apply #'init sdl-init-flags)
    (unwind-protect
	 (in-main-thread ()
	   (when request-gl-p
	     (tri-app:request-gl-fn app))
	   (unwind-protect
		(progn
		  (setq win (create-window :title title :x x :y y :w w :h h :flags sdl-window-flags))
		  (unwind-protect
		       (progn
			 (setq gl (gl-create-context win))
			 (unwind-protect
			      (prog ((quit nil)
				     (idle-fn
				      (lambda ()
					(cond (render-fn-p (tri-app:draw-fn app))
					      (t (delay 1000))))))
			       restart-pipeline
				 (format t "---> SETUP~%")
				 (tri-app:setup-fn app)
				 (unwind-protect
				      (handler-case
					  (with-sdl-event (sdl-event)
					    (loop :until quit
						  :do (loop :as rc = (next-event sdl-event :poll nil)
							    :until (null rc)
							    :do (let* ((sdl-event-type (get-event-type sdl-event))
								       (sdl-event-id (and (user-event-type-p sdl-event-type)
											  (sdl-user-event-code sdl-event))))
								  (when alt-event-handler-p
								    (tri-app:update-fn app sdl-event))
								  (case sdl-event-type
								    (:lisp-message nil (get-and-handle-messages))
								    (:eval-event
								     (let ((data (get-user-data (sdl-user-event-code sdl-event))))
								       (format t "---> BINGO~%")
								       (with-simple-restart (cont "CONT")
									 (funcall data))))
								    (:sdl-event-window-resized ()
								     (with-slots (win w h) app
								       (multiple-value-bind (new-width new-height)
									   (get-window-size win)
									 (setq w new-width h new-height)
									 (tri-app:resize-fn app new-width new-height))))
								    (:sdl-event-key-down
								     (let ((scancode (sdl-keyboard-event-scancode
										      (sdl-event-key sdl-event))))
								       (format t "HANDLING KEY DOWN EVENT~%")
								       (when (eql scancode :sdl-scancode-escape)
									 (push-event :sdl-event-quit))))
								    (:idle (funcall idle-fn))
								    (:sdl-event-quit (setq quit t)))
								  (when (and sdl-event-id
									     (not (eq sdl-event-type :lisp-message)))
								    (free-user-data sdl-event-id))))
						  (when (not quit) (funcall idle-fn))))
					(tri-app:restart-pipeline (c)
					  (declare (ignore c))
					  (go restart-pipeline)))
				   (tri-app:cleanup-fn app)))
			   (gl-delete-context gl)))
		    (destroy-window win)))))
      (quit))))

(defvar tri-app:*app* nil)

(defmethod tri-app:launch ((app tri-app:lifecycle-mixin))
  (reset-main)
  (with-slots (main-thread) app
    (assert (or (null main-thread)
		(not (bt:thread-alive-p main-thread)))))
  (setq tri-app:*app* app)
  (bt:make-thread (lambda () (tri-app:start app))
		  :name (format nil "background runner for ~A" app)))

(defmethod tri-app:shutdown ((app tri-app:lifecycle-mixin))
  (in-main-thread () (push-event :sdl-event-quit)))

(defmethod tri-app:apply-in-event-loop ((app tri-app:lifecycle-mixin) func &rest args)
  (push-user-event :eval-event  (lambda () (apply func args))))

(defmacro tri-app:in-event-loop (&body body)
  `(tri-app:apply-in-event-loop tri-app:*app* (lambda () ,@body)))



;;; ----------------------------------------------------------------------
;;;
;;; shader program load helpers
;;;

(defun assert-no-shader-errors (shader-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-shader-iv shader-id :compile-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-shader-info-log shader-id))))
      (cffi:foreign-free success))))

(defun assert-no-program-errors (program-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-program-iv program-id :link-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-program-info-log program-id))))
      (cffi:foreign-free success))))

(defun link-program-from-shader-strings (vertex-source-code fragment-source-code)
  (let ((vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader)))
    (assert (not (zerop vertex-shader)))
    (assert (not (zerop fragment-shader)))
    (unwind-protect
         (progn
           (gl:shader-source vertex-shader vertex-source-code)
           (gl:compile-shader vertex-shader)
           (assert-no-shader-errors vertex-shader)

           (gl:shader-source fragment-shader fragment-source-code)
           (gl:compile-shader fragment-shader)
           (assert-no-shader-errors fragment-shader)

           (let ((shader-program-id (gl:create-program)))
             (gl:attach-shader shader-program-id vertex-shader)
             (gl:attach-shader shader-program-id fragment-shader)
             (gl:link-program shader-program-id)
             (assert-no-program-errors shader-program-id)
	     (gl:detach-shader shader-program-id vertex-shader)
	     (gl:detach-shader shader-program-id fragment-shader)
	     (values shader-program-id)))
      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader))))

(defun link-program-from-shaders (vertex-source-file fragment-source-file)
  "Create a program from the glsl source files for vertex and fragment
shaders. Must be called with an opengl
context present."
  (assert (probe-file vertex-source-file))
  (assert (probe-file fragment-source-file))
  (let ((vertex-source-code (uiop:read-file-string vertex-source-file))
        (fragment-source-code (uiop:read-file-string fragment-source-file)))
    (link-program-from-shader-strings vertex-source-code fragment-source-code)))


;;; ----------------------------------------------------------------------
;;;
;;; send vertices -- borrowed from Joh11/opengl-utils
;;;

(defun send-buffer-data (target usage arr &key buffer (type :float))
  "Copy our lisp vertices array ARR into a gl-array and load this array
into the active buffer (VBO) for OpenGL to use.

TARGET is usually ARRAY-BUFFER or ELEMENT-ARRAY-BUFFER
USAGE is usually STATIC-DRAW or DYNAMIC-DRAW or STREAM-DRAW

When BUFFER is given, binds it first (with the same TARGET), and
unbinds it at the end.

NOTE If you are using Vertex Array Objects (VAOs) to record the
setting up of attributes, then you should not pass BUFFER to this
function as it will probably unbind the buffer prematurely.
"
  (when buffer
    (gl:bind-buffer target buffer))
  (let ((n (length arr)) glarr)
    (unwind-protect (progn (setq glarr (gl:alloc-gl-array type n))
			   (dotimes (i n)
			     (setf (gl:glaref glarr i) (aref arr i)))
			   (gl:buffer-data target usage glarr))
      (when glarr
	(gl:free-gl-array glarr))))
  (when buffer
    (warn ";; if you unbind the buffer, vao doesn't work and so no correct
;; triangle. don't pass buffer or don't use this function at all.")
    (gl:bind-buffer target 0)))

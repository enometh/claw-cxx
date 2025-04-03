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

(defun main ()
  (let ((bt:*default-special-bindings*
	 `((*request-gl-fn* . ,*request-gl-fn*))))
    (unwind-protect
	 (with-init (:video)
	   (when *request-gl-fn* (funcall *request-gl-fn*))
	   (with-window (win :title "TRIPAD" :flags '(:opengl))
	     (setq *win* win)
	     (with-gl-context (gl win)
	       (setq *gl* gl)
	       (progn
		 (when *init-fn* (funcall *init-fn*))
		 (with-event-loop (:method :poll)
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

(defun runmain (&key gles-p major minor)
  (reset-main)
  (mapcar 'bt:destroy-thread (bt-find-threads "background-tripad"))
  (let ((bt:*default-special-bindings*
	 `((*request-gl-fn* . ,(if (or gles-p major minor)
				   (lambda () (request-gl :profile (if gles-p :es)
							  :major (or major (and gles-p 3))
							  :minor (or minor (and gles-p 1)))))))))
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

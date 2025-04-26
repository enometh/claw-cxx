;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Wed Apr 23 15:58:42 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; EXPERIMENTAL SDL3 backend for CEPL.
;;;
;;; incorporates code Copyright (c) 2016, Baggers
;;; ~/cl/extern/Github/Authors/cbaggers/cepl.sdl2/cepl.sdl2.lisp
;;; (commit 6da5a030), modified for use with claw-cxx-sdl3.  laste
;;; tested with ~/cl/extern/Github/Authors/cbaggers/cepl (commit
;;; 543c9fc1bc52)
;;;
;;; Unlike the cepl.sdl2 we don't use low level sdl functions. Call
;;; high level sdl3:init which initializes SDL Main thread and then
;;; arrange for the cepl context to run in this Main thread.

(defpackage #:cepl.sdl3
  (:shadowing-import-from "CL"
   "CONDITION" "POSITION" "REM" "CLOSE" "SECOND" "TYPE" "FORMAT" "WRITE" "READ" "MOD" "LENGTH" "PHASE" "REMOVE" "FUNCTION")
  (:shadowing-import-from "CEPL.HOST" "INIT")
  (:use "CL" "SDL3" "CLAW-CXX-SDL3" "CEPL.HOST"))
(in-package "CEPL.SDL3")

(defvar *initd* nil)

;;======================================================================
;; api v2

;; NOTE: to support reloading the repl after quire, sdl3:init should
;; already be called before sdl3-init is called, it can be called
;; multiple times without a problem. see (REPL) below.

(defgeneric sdl3-init (&rest init-flags)
  (:method (&rest init-flags)
    (let ((flags (or init-flags :everything)))
      (unless (sdl3:was-init :everything)
        (init-sdl3-low-level flags)
        (setf *initd* t)))))

(defun init-sdl3-low-level (&rest sdl-init-flags)
  (apply #'sdl3:init sdl-init-flags))

(defun sdl-shutdown ()
  (low-level-quit))

(defun low-level-quit ()
  (assert (find (cepl.context::primary-context) cepl.context::*contexts*))
  (dolist (ctx cepl.context::*contexts*)
    (mapcar #'(lambda (x) (ignore-errors (sdl3:destroy-window x)))
	    (cepl.context::%cepl-context-surfaces ctx))
    (setf (cepl.context::%cepl-context-surfaces ctx) nil)
    (setf (cepl.context::%cepl-context-current-surface ctx) nil)
    (let ((cepl-gl-context (cepl.context::%cepl-context-gl-context ctx)))
      (when (and cepl-gl-context  (cepl.context::handle cepl-gl-context))
	(ignore-errors(sdl3:gl-delete-context (cepl.context::handle cepl-gl-context)))
	(setf (cepl.context::%cepl-context-gl-context ctx) nil))))
  (assert (eql cepl.context::*primary-context* cepl.context::*cepl-context*))
  (when t
    (setq cepl.context::*contexts* nil)
    #+nil(setq cepl.context::*contexts* (list cepl.context::*cepl-context*))
    ;; no cepl.context:free-context, leaks memory, but at least resuse
    ;; context-ids. reset  cepl.context::*free-context-ids*
    ;; (loop :for i :below cepl.context::+max-context-count+ :collect i)
    (cepl.context::discard-context-id
     (cepl.context::%cepl-context-id cepl.context::*cepl-context*))
    (setq cepl.context::*cepl-context* (cepl.context::make-context-internals t))
    (setq cepl.context::*primary-context*  cepl.context::*cepl-context*))
  ;; XXX force shutdown anyway
  (setq cepl.lifecycle::*lifecycle-state* :uninitialized)
  (with-simple-restart (cont "cont")
    (sdl3:quit)) ;; this should kill the threads but just in case:
  (prog1 (list sdl3::*the-main-thread* sdl3::*main-thread* sdl3::*main-thread-channel*)
    (mapcar 'bt:destroy-thread
	    (remove-if-not (lambda (x) (equal (bt:thread-name x)  "SDL3 Main Thread"))
			   (bt:all-threads)))
    (setq sdl3::*the-main-thread* nil)
    (setq sdl3::*main-thread* nil)
    (setf sdl3::*main-thread-channel* nil)
    (setf sdl3::*lisp-message-event* nil))
  #+nil
  (let ((init-flags (autowrap:mask 'sdl2::sdl-init-flags :everything)))
    (sdl2::check-rc (sdl2::sdl-init init-flags))))

;;----------------------------------------------------------------------

;; {TODO} optimize
(let ((sdl->lisp-time-offset 0))
  (defun set-sdl->lisp-time-offset ()
    (setf sdl->lisp-time-offset (cl:- (get-internal-real-time) (sdl3:get-ticks))))
  (defun sdl->lisp-time (sdl-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:+ sdl-time sdl->lisp-time-offset))
  (defun lisp->sdl-time (lisp-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:- lisp-time sdl->lisp-time-offset)))

(defmacro %case-events ((event &key (method :poll) (timeout nil)  otherwise)
                        &body event-handlers)
  `(let (,(when (symbolp event) `(,event (sdl3:new-event))))
     (loop :until (null  (sdl3:next-event ,event ,method ,timeout)) :do
        (case (sdl3::get-event-type ,event)
          ,@(loop :for (type params . forms) :in event-handlers
               :append (let ((type (if (listp type)
                                       type
                                       (list type))))
                         (loop :for typ :in type :collect
                            (sdl3::expand-handler event typ params forms)))
               :into results
               :finally (return (remove nil results)))
	  (otherwise ,otherwise)))
     (sdl3:free-event ,event)))

;; $listeners used to be a lexically closed over variable. This may
;; have avoided thread safety issues but also made it undebuggable and
;; unresettable without inventing a based-Scheme object-system.
;;
;; other good-to-have cepl patches: Use pushnew instead of push in
;; cepl.context::discard-context-id.  use global variables instead of
;; lexical variables in cepl.host::register-event-listener and friends
;; (in api-0.lisp), so these can be inspected and reset by the user.
;;
;; also the single entry point of skitter into cepl is
;; (cepl:register-event-listener 'skitter.sdl3::on-event), which calls
;; cepl.host::register-event-listener, which calls
;; sdl-register-listner. cepl.host::register-event-listener should
;; patched to use the parameter NIL to cancel an existing registration
;; function, like we cancel listener registrations below.

(defvar $listeners nil)

(defun sdl-register-listener (func)
  (if func
      (unless (find func $listeners)
	(push func $listeners))
      (setq $listeners nil)))

(defun sdl-step-v1 (surface)
  (declare (ignore surface))
  (%case-events (event
		 :otherwise (loop :for listener :in $listeners :do (funcall listener event)))))

;;----------------------------------------------------------------------

(defun sdl-swap (handle)
  (claw-cxx-sdl3:sdl-gl-swap-window handle))

;;----------------------------------------------------------------------

#+nil
(cffi:foreign-enum-keyword-list  'sdl-gl-attr)

(defun make-sdl-context (surface version double-buffer
                         alpha-size depth-size stencil-size buffer-size
                         red-size green-size blue-size)
  (sdl3:gl-set-attr :sdl-gl-alpha-size alpha-size)
  (sdl3:gl-set-attr :sdl-gl-depth-size depth-size)
  (when stencil-size
    (sdl3:gl-set-attr :sdl-gl-stencil-size stencil-size))
  (sdl3:gl-set-attr :sdl-gl-red-size red-size)
  (sdl3:gl-set-attr :sdl-gl-green-size green-size)
  (sdl3:gl-set-attr :sdl-gl-blue-size blue-size)
  (sdl3:gl-set-attr :sdl-gl-buffer-size buffer-size)
  (sdl3:gl-set-attr :sdl-gl-doublebuffer (if double-buffer 1 0))
  (sdl3:gl-set-attr :sdl-gl-share-with-current-context 0)
  ;;
  (let ((context (if version
                     (create-context-by-version surface version)
                     (search-for-context surface))))
    (assert context ()
            "CEPL.SDL3: Could not find a suitable context for CEPL.
Your machine must support at least GL 3.3")
    context))

(defun make-shared-sdl-context (current-gl-context surface version double-buffer
                                alpha-size depth-size stencil-size buffer-size
                                red-size green-size blue-size)
  (declare (ignorable current-gl-context))
 (sdl3:in-main-thread ()
  (sdl3:gl-set-attr :sdl-gl-alpha-size alpha-size)
  (sdl3:gl-set-attr :sdl-gl-depth-size depth-size)
  (when stencil-size
    (sdl3:gl-set-attr :sdl-gl-stencil-size stencil-size))
  (sdl3:gl-set-attr :sdl-gl-red-size red-size)
  (sdl3:gl-set-attr :sdl-gl-green-size green-size)
  (sdl3:gl-set-attr :sdl-gl-blue-size blue-size)
  (sdl3:gl-set-attr :sdl-gl-buffer-size buffer-size)
  (sdl3:gl-set-attr :sdl-gl-doublebuffer (if double-buffer 1 0))
  (sdl3:gl-set-attr :sdl-gl-share-with-current-context 1)
  ;;
  (let ((context (if version
                     (create-context-by-version surface version)
                     (search-for-context surface))))
    (assert context ()
            "CEPL.SDL3: Could not find a suitable context for CEPL.
Your machine must support at least GL 3.3")
    (values context surface))))

(defvar *core-context* t)

(defun create-context-by-version (surface version)
  (destructuring-bind (&optional major minor)
      (cepl.context:split-float-version version)
    (sdl3:gl-set-attr :sdl-gl-context-profile-mask
		      (if *core-context*
                          claw-cxx-sdl3::+sdl-gl-context-profile-core+
                          claw-cxx-sdl3::+sdl-gl-context-profile-compatibility+))
    (sdl3:gl-set-attr :sdl-gl-context-major-version major)
    (sdl3:gl-set-attr :sdl-gl-context-minor-version minor)
    (sdl3:gl-create-context surface)))

(defun search-for-context (surface)
  (let ((p (if *core-context*
               claw-cxx-sdl3:+SDL-GL-CONTEXT-PROFILE-CORE+
               claw-cxx-sdl3:+SDL-GL-CONTEXT-PROFILE-COMPATIBILITY+))
        context)
    (loop :for (major minor core) :in `((4 6 ,p) (4 5 ,p) (4 4 ,p) (4 3 ,p)
                                        (4 2 ,p) (4 1 ,p) (4 0 ,p) (3 3 ,p))
       :until context
       :do (handler-case
               (progn
                 ;; (print (list :> major minor (= core +)))
                 (sdl3:gl-set-attr :sdl-gl-context-profile-mask core)
                 (sdl3:gl-set-attr :sdl-gl-context-major-version major)
                 (sdl3:gl-set-attr :sdl-gl-context-minor-version minor)
                 (setf context (sdl3:gl-create-context surface)))
             (error ())))
    context))


(defun sdl-make-current (context surface)
  (sdl3:gl-make-current surface context))

;;----------------------------------------------------------------------

(defun make-sdl-surface (width height title fullscreen
                         no-frame alpha-size depth-size stencil-size
                         red-size green-size blue-size buffer-size
                         double-buffer hidden resizable)
  (declare (ignore alpha-size depth-size stencil-size buffer-size double-buffer
                   red-size green-size blue-size))
 (sdl3:in-main-thread ()
  (let ((surface
         (sdl3:create-window
          :title title :w width :h height
          :flags (remove nil `(#+nil :shown
				     :opengl
                                      ,(when fullscreen :fullscreen-desktop)
                                      ,(when resizable :resizable)
                                      ,(when no-frame :borderless)
                                      ,(when hidden :hidden))))))
    (when hidden
      (sdl3:hide-window surface))
    surface)))

(defun destroy-sdl-surface (surface)
  (sdl3:destroy-window surface))

(defun sdl-surface-size (win-handle)
  (multiple-value-list (sdl3:get-window-size win-handle)))

(defun sdl-set-surface-size (win-handle width height)
  (sdl3:set-window-size win-handle width height))

(defun sdl-surface-fullscreen-p (surface)
  (not (null (intersection '(:fullscreen-desktop :fullscreen)
                           (sdl3:get-window-flags surface)))))

(defun sdl-set-surface-fullscreen (surface state)
  (sdl3:set-window-fullscreen surface state))

(defun sdl-surface-title (surface)
  (values (sdl3:get-window-title surface)))

(defun sdl-set-surface-title (surface title)
  (sdl3:set-window-title surface title))

;;----------------------------------------------------------------------

;madhu 250423 cepl fix: change cepl.host:check-host functionp tests to
; instead check for the equivalent of (lambda (x) (etypecase x
; (function t) (symbol (fboundp x))))

(defclass sdl-api (cepl.host:api-2)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform t)
   ;;
   (init-function :initform 'sdl3-init)
   ;;
   (shutdown-function :initform 'sdl-shutdown)
   ;;
   (make-surface-function :initform 'make-sdl-surface)
   ;;
   (destroy-surface-function :initform 'destroy-sdl-surface)
   ;;
   (make-context-function :initform 'make-sdl-context)
   ;;
   (step-function :initform 'sdl-step-v1)
   ;;
   (register-event-callback-function :initform 'sdl-register-listener)
   ;;
   (swap-function :initform 'sdl-swap)
   ;;
   (surface-size-function :initform 'sdl-surface-size)
   ;;
   (make-context-current-function :initform 'sdl-make-current)
   ;;
   (set-surface-size-function :initform 'sdl-set-surface-size)
   ;;
   (surface-fullscreen-p-function :initform 'sdl-surface-fullscreen-p)
   ;;
   (set-surface-fullscreen-function :initform 'sdl-set-surface-fullscreen)
   ;;
   (surface-title-function :initform 'sdl-surface-title)
   ;;
   (set-surface-title-function :initform 'sdl-set-surface-title)
   ;;
   (make-gl-context-shared-with-current-context-function
    :initform 'make-shared-sdl-context)))

(register-host 'sdl-api)

;;----------------------------------------------------------------------

(defun vsync ()
  (let ((val (cffi:with-foreign-object (ret :int)
	       (claw-cxx-sdl3-user::check-rc
		(claw-cxx-sdl3:%sdl-gl-get-swap-interval ret))
	       (cffi:mem-ref ret :int))))
    (cond
      ((= val -1) :unsupported)
      ((= val 0) nil)
      ((= val 1) t))))

(defun (setf vsync) (boolean)
  (if (eq :unsupported (vsync))
      (warn "Sorry setting vsync is not supported")
      (claw-cxx-sdl3:%sdl-gl-set-swap-interval  (if boolean 1 0)))
  boolean)


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun repl (&rest args)
  (if (cepl.context:gl-initialized-p)
      (error "repl already running?"))
  (sdl3-init)				;start SDL3 Main beforehand.
  (if cepl.host::*current-host*
      (progn (setf (cepl.context::%cepl-context-bound-thread (cepl.context::primary-context)) nil)))
  ;; make sure bound-thread is SDL Main when cepl-context is initialized
  (sdl3:in-main-thread (:no-event t) (apply #'cepl:repl args)))

#+nil
(repl)

#+nil
(cepl:quit)

#||
(claw-cxx-sdl3-user:gl-get-attr  :sdl-gl-context-major-version)
(claw-cxx-sdl3-user:gl-get-attr  :sdl-gl-context-minor-version)
(cepl.context::primary-context)
(mapcar #'cepl.context::assert-no-other-context-is-bound-to-thread (bt:all-threads))
||#

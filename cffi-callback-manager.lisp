;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Wed Sep 18 06:21:10 2019 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; ;madhu 250327 (resourced from girlib ffi-callback-manager)

(in-package "CL-USER")

(defpackage "CFFI-CALLBACK-MANAGER"
  (:use "CL")
  (:export
   "CALLBACK-MANAGER"
   "REGISTER-CALLBACK"
   "UNREGISTER-CALLBACK"
   "FIND-CALLBACK"
   "WITH-REGISTERED-CALLBACK"
   "FUNCALL-OBJECT-CALLBACK"
   "FREE-FUNCALL-OBJECT-CALLBACK"))
(in-package "CFFI-CALLBACK-MANAGER")


(defstruct (callback-manager (:constructor %make-callback-manager))
  (lock (bordeaux-threads:make-lock "callback-manager-lock"))
  (queue (make-array 0 :adjustable t :fill-pointer t))
  (free-list nil))

(defvar *callback-manager* (%make-callback-manager))

;; REGISTER-CALLBACK Allocate a tag and return the CFFI:POINTER
;; of its location. This tag identifies the FUNCTION.  The location
;; pointer can be used to lookup the function via FIND-CALLBACK.
;; FIND-CALLBACK is intended to be used within a CFFI:DEFCALLBACK with
;; the location pointer being passed in as user-data. Once the lisp
;; FUNCTION is retrieved it can be called within the CFFI:DEFCALLBACK
;; form.

(defun register-callback (function)
  "Registers a lisp object with the CALLBACK-MANAGER. Returns a foreign
pointer which is the address of an integer that identifies the object
in the CALLBACK-MANAGER."
  (with-slots (queue free-list lock) *callback-manager*
    (bordeaux-threads:with-lock-held (lock)
      (let* ((index (pop free-list)))
	(if index
	    (setf (elt queue index) function)
	    (progn (setq index (length queue))
		   (assert (= index (vector-push-extend function queue)))))
	(cffi:foreign-alloc :int :initial-element index)))))

(defun unregister-callback (loc)
  (with-slots (lock queue free-list) *callback-manager*
    (let ((index (cffi:mem-ref loc :int)))
      (bordeaux-threads:with-lock-held (lock)
	(push index free-list)
	(setf (elt queue index) nil)))
    (cffi:foreign-free loc)))

(defun find-callback (loc)
  "Returns the lisp object registered with REGISTER-CALLBACK"
  (with-slots (lock queue free-list) *callback-manager*
    (declare (ignorable free-list))
    (let ((index (cffi:mem-ref loc :int)))
      (bordeaux-threads:with-lock-held (lock)
	(elt queue index)))))

(defmacro with-registered-callback ((loc-var) function &body body)
  `(let ((,loc-var (register-callback ,function)))
     (unwind-protect (progn ,@body)
       (unregister-callback ,loc-var))))

;; provide an interface similar to 1) cl-gobject-introspection-wrapper
;; glib and 2) cl-cffi-gtk's stable pointer

(cffi:defcallback funcall-object-callback :bool ((user-data :pointer))
  (with-simple-restart (cont "Skip Error during Execution")
    (funcall (find-callback user-data))))

(cffi:defcallback free-funcall-object-callback :void ((user-data :pointer))
  (unregister-callback user-data))

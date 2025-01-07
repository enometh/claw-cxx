;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Jan 06 10:57:42 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(cl:in-package "CLAW-CXX-SDL3-USER")

(eval-when (load eval compile)
(defvar $conflicts
  (claw.util::use-spec-package "CLAW-CXX-SDL3"
			       "CLAW-CXX-SDL3-USER"
			       :dry-run-p nil)))


(cobj:define-cobject-class :CLAW-CXX-SDL3)

(defun get-conflicts-1 (spec-package &optional (package *package*))
  "Return a list of symbols to be uninterned in package"
  (setq package (find-package package))
  (setq spec-package (find-package spec-package))
  (loop for x being each symbol of package
      if (and (eql
 (symbol-package x) (find-package package))
	      (multiple-value-bind (sym stat)
		  (find-symbol (string x) spec-package)
		;;(assert (eql sym x))
		(eql stat :external)))
	collect x))

#||
(setq $c1 (get-conflicts-1 'claw-cxx-sdl3 'claw-cxx-sdl3-user))
(mapcar (lambda (x) (unintern x 'claw-cxx-sdl3-user)) $c1)
||#

(defun cobj-definables (spec-package)
  (loop for s being each symbol of spec-package
	with defn
	when (and (find-class s nil)
		  (setq defn (ignore-errors
			       (cobj::cobject-class-definition s))))
	collect defn))

(defun get-exportables (spec-package)
  (loop for defn in (cobj-definables spec-package)
	append
	(remove-if
	 (lambda (s)
	   (multiple-value-bind (sym stat)
	       (find-symbol (string s) spec-package)
	     (assert (eql sym s))
	     (assert (eql (symbol-package sym) (find-package spec-package)))
	     (eql stat :external)))
	 (cobj::cobject-class-definition-symbols defn))))

;;#+nil
(progn
(setq $ret (get-exportables :claw-cxx-sdl3))
(export $ret :claw-cxx-sdl3))

(defun set-equal (a b)
  (and (eql (set-difference a b) nil)
       (eql (set-difference b a) nil)))

;; #+nil
(progn
(assert
(set-equal (claw.util::use-spec-package :claw-cxx-sdl3 :claw-cxx-sdl3-user :dry-run-p t) $conflicts)))

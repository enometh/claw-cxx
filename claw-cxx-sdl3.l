;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Jan 02 21:52:53 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: madhu@cs.unm.edu
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;;  cl-claw-sdl3
;;
(in-package "CL-USER")

;;#+nil
(progn
(load "~/cl/extern/Github/mgl-pax/mgl-pax.system")
(load "~/cl/extern/Github/named-readtables/named-readtables.system")
(load "~/cl/extern/claw-cxx/claw-cxx.system")
(load "~/cl/extern/cl-claw-sdl3/cl-claw-sdl3.system"))

;;#+nil
(progn
(require 'cffi)			;no need for claw
(require 'claw-cxx/util))

(require 'cl-claw-sdl3/base-config)
(in-package "CLAW-CXX-SDL3-USER")
*package*

;;; generate bindings
#+nil
(progn
(unless (cffi:find-foreign-library "libresect")
  (cffi:load-foreign-library "libresect.so"))
(mk:oos :cl-claw-sdl3/defwrap :load)
(setup-base-dir)
(claw.wrapper:generate-wrapper :CLAW-CXX-SDL3))

;;; generate defsystem
#+nil
(progn
(mk:oos :cl-claw-sdl3/defsys :load)
(dump-defsystem-file))

#+nil
(load "/dev/shm/claw-cxx-sdl3/claw-cxx-sdl3.system")

;;; compile library
#+nil
(mk:oos :claw-cxx-sdl3.library :compile)

#+(and clozure nil);; work around ccl bug
(progn #+nil ;; fails on ccl, need to fix cffi-object:defcfun to avoid blowing stack.
(require 'cffi-object)
cobj::+defcfun+ ;; #<Compiled-function CFFI:DEFCFUN Macroexpander #x3020011CA2BF>
(macro-function 'cffi::defcfun) ;; #<Compiled-function CFFI:DEFCFUN Macroexpander #x3020011CA2BF>
(macro-function 'cffi-object::defcfun) ;; #<Compiled-function CFFI:DEFCFUN (Non-Global)  #x302001590E1F>
(setf (macro-function 'cffi-object::defcfun) cobj::+defcfun+)
;; to recover if overwritten
#+nil
(mk:load-system 'cffi)
#+nil
(makunbound 'cobj::+defcfun+)
#+nil
(setf cobj::+defcfun+ (macro-function 'cffi::defcfun))
#+nil
(setf (macro-function 'cffi-object::defcfun) cobj::+defcfun+)
(list cobj::+defcfun+ (macro-function 'cffi::defcfun) (macro-function 'cffi-object::defcfun)))

;; compile bindings
#+nil
(progn
(require 'cffi-object)
(list cobj::+defcfun+ (macro-function 'cffi::defcfun) (macro-function 'cffi-object::defcfun))
(setq cobj::*optimize-object-allocation-p* nil)
#+nil
(setq cobj::*optimize-object-allocation-p* t)
(setf (macro-function 'cffi::defcfun) (macro-function 'cffi-object::defcobjfun))
(assert (eql (macro-function 'cffi::defcfun)
	     (macro-function 'cffi-object::defcobjfun)))
(mk:oos :claw-cxx-sdl3.bindings :compile :force t :propagate nil)
#+nil
(setf (macro-function 'cffi::defcfun)  cobj::+defcfun+))


;;; ----------------------------------------------------------------------
;;;
;;; after everything is compiled
;;;
(load "/dev/shm/claw-cxx-sdl3/claw-cxx-sdl3.system")

(unless (cffi:find-foreign-library "libSDL3")
  (cffi:load-foreign-library "libSDL3.so"))

#||
(cffi:foreign-symbol-pointer "SDL_Init")
(cffi:foreign-symbol-pointer "SDL_GetVersion")
(cffi:foreign-funcall  "SDL_GetVersion" :int)
||#

#+nil
(unless (cffi:find-foreign-library "claw-cxx-sdl3/lib/gen/lib/adapter.x86_64-pc-linux-gnu.so")
  (mk:mk-oos :CLAW-CXX-SDL3.LIBRARY :load :compile-during-load nil))

#+nil
(require 'cffi-object)
nil
(mk:oos :CLAW-CXX-SDL3.BINDINGS :load :compile-during-load nil :load-source-instead-of-binary nil :minimal-load t)


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

#+nil
(progn
(require 'cffi-ops)
(require 'cffi-object.ops))

;;(cobj.ops:enable-cobject-ops)
;;(cobj.ops:disable-cobject-ops)


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
#+nil
(claw.util::use-spec-package "CLAW-CXX-SDL3"
			     "CLAW-CXX-SDL3-USER"
			     :dry-run-p t)

(defvar $conflicts
  (claw.util::use-spec-package "CLAW-CXX-SDL3"
			       "CLAW-CXX-SDL3-USER"
			       :dry-run-p nil))


#||
(length cobj::*cobject-class-definitions*)
(setq  cobj::*cobject-class-definitions* nil)
#+nil
(cobj::define-package-cobject-classes "CLAW-CXX-SDL3")
||#

#+nil
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

#+nil
(progn
(setq $ret (get-exportables :claw-cxx-sdl3))
(export $ret :claw-cxx-sdl3))

#||
(sdl-get-version)

(setq $d (cobj-definables :claw-cxx-sdl3))
(length $d)
(mapcar 'cobj::cobject-class-definition-class $d)
(every (lambda (c)
	 (eql (symbol-package c)
	      (find-package :claw-cxx-xkb)))
       (cobj::cobject-class-definition-symbols (car $d)))
(every (lambda (c) (eql (symbol-package c) (find-package :claw-cxx-xkb))) $ret)

(progn
  (user::lc "~/cl/package-funcs.l")
  (use-package "MY-PACKAGE-FUNCS"))

(user::lc "~/cl/read-sexp.lisp")
(package-symbols :claw-cxx-xkb-user)
(length (package-symbols :claw-cxx-sdl3 :symbol-type :external))
(length (package-symbols :claw-cxx-sdl3-user :symbol-type :internal))
||#

(defun set-equal (a b)
  (and (eql (set-difference a b) nil)
       (eql (set-difference b a) nil)))

#+nil
(set-equal (claw.util::use-spec-package :claw-cxx-sdl3 :claw-cxx-sdl3-user :dry-run-p t) $conflicts)


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

#||
(find-symbol "MAKE-SDL-ATOMIC-INT" :claw-cxx-sdl3)
(find-symbol "MAKE-SDL-ATOMIC-INT" :claw-cxx-sdl3-user)
(with-output-to-string (*standard-output*)
  (apropos "MAKE-SDL-ATOMIC-INT"))

(cffi::parse-type 'claw-cxx-sdl3::sdl-atomic-int)
(type-of (cffi::parse-type '(:struct sdl-atomic-int)))
(typep (cffi::parse-type '(:struct sdl-atomic-int)) 'cffi::foreign-type)
(cobj::find-cobject-class-definition (cffi::ensure-parsed-base-type 'sdl-atomic-int))

(eq (find-symbol "SDL-ATOMIC-INT")
    (find-symbol "SDL-ATOMIC-INT" 'claw-cxx-sdl3))

(cffi-object:define-cobject-class claw-cxx-sdl3:sdl-atomic-int)
(cffi-object::define-struct-cobject-class claw-cxx-sdl3:sdl-atomic-int)
||#



;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(load $system-file)
(require 'claw-cxx-sdl3.bindings)
(require 'claw-cxx-sdl3.library)

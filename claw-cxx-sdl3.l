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

;; load foreign library
(unless (cffi:find-foreign-library "libSDL3")
  (cffi:load-foreign-library "libSDL3.so"))

#||
(cffi:foreign-symbol-pointer "SDL_Init")
(cffi:foreign-symbol-pointer "SDL_GetVersion")
(cffi:foreign-funcall  "SDL_GetVersion" :int)
||#

;;; compile library
#+nil
(mk:oos :claw-cxx-sdl3.library :compile)

;;; compile bindings
;;;

;; swap in cffi-object defcobjfun into cffi:defcfun before compiling
;; the bindings
#+nil
(mk:oos :cl-claw-sdl3/defcfun :load :compile-during-load t)

#||
;;  loading cl-claw-sdl3/defcfun should do the following
#+nil
(setf (macro-function 'cffi::defcfun) (macro-function 'cobj::defcobjfun))
;; so
(assert (eql (macro-function 'cffi:defcfun) (macro-function 'cobj::defcobjfun)))

;; on ccl if cobj::+defcfun+ says "CFFI:DEFCFUN (Non-Global)"
#'cffi:defcfun
;; then it may need to be fixed
;; re-evaluate the form for cffi:defcfun
;;(makunbound 'cobj::+defcfun+)
(setf (symbol-value 'cobj::+defcfun+) (macro-function 'cffi:defcfun))
(mk:oos :cl-claw-sdl3/defcfun :load)
(assert (eql (macro-function 'cffi:defcfun) (macro-function 'cobj::defcobjfun)))
||#

;; compile and load cobj bindings with new defn of cffi:defcfun.
(mk:oos :claw-cxx-sdl3.bindings :load :force nil :propagate nil)

#||
;; restore
#+nil
(setf (macro-function 'cffi::defcfun)  cobj::+defcfun+)
||#


;;; ----------------------------------------------------------------------
;;;
;;; finishing step: import symbols from CLAW-CXX-SDL3 and export them
;;; from CLAW-CXX-SDL3-USER
;;;

(mk:oos :cl-claw-sdl3/library :load :compile-during-load t)


;;; ----------------------------------------------------------------------
;;;
;;; LISP LAYER OVER SDL3
;;;

#+nil
(progn
(load "~/cl/extern/Github/trivial-channels/trivial-channels.system"))

#+nil
(mk:oos :cl-claw-sdl3/user :load)


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

;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun May 25 15:06:09 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; ADMINISTRATIVE: generate the bindings and defsystem files.
;;;
(defpackage "CL-CLAW-IMLIB2-USER"
  (:use "CL"))
(in-package "CL-CLAW-IMLIB2-USER")

#+nil
(progn
(load "~/cl/extern/Github/mgl-pax/mgl-pax.system")
(load "~/cl/extern/Github/named-readtables/named-readtables.system")
(load "~/cl/extern/claw-cxx/claw-cxx.system"))

#+nil
(mk:oos :claw-cxx/util :load)

(defvar $base-dir (merge-pathnames "cl/extern/cl-claw-imlib2/"
				   (user-homedir-pathname)))

(defvar $fasl-root (merge-pathnames "/dev/shm/ecl-fasl/cl-claw-imlib2/"))

(defvar $custom-header-file-name "a.h")
(defvar $custom-header-dir (merge-pathnames "gen/" $base-dir))

(defvar $link-libs "Imlib2")
(defvar $default-cflags nil)
(defvar $default-ldflags nil)
(defvar $includes-paths (list $custom-header-dir))
(defvar $system-includes-paths nil)
(defvar $link-libs-paths nil)

(defvar $compiler-options
  (claw.util:make-compiler-options
   :link-libs $link-libs
   :default-cflags nil
   :default-ldflags $default-ldflags
   :system-includes-paths $system-includes-paths
   :include-paths $includes-paths
   :link-libs-paths $link-libs-paths))

#+nil
(defvar $inclusives "#include <Imlib2.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
")

;; write custom header
#+nil
(ensure-directories-exist $custom-header-dir)

#+nil
(with-open-file (stream (merge-pathnames $custom-header-file-name
					 $custom-header-dir)
			:direction :output
			:if-exists :supersede)
  (write-string $inclusives stream))

(defvar $wrapper-name :claw-cxx-imlib2
  "Name supplied to claw.wrapper:defwrapper" )

(defvar $system-name :claw-cxx-imlib2
  "base name for mk-defsystem generated `binding' and `library'systems")

(defvar $package-name "CLAW-CXX-IMLIB2"
  "the package from which the generated bindings and cffi-object
definitions are exported.")

(defvar $system-file
  (merge-pathnames (format nil "~(~A.system~)" $system-name)
		    $base-dir))

#+nil ;; no system library
(defvar $system-library-name
  (intern (format nil "~@:(~A.library~)" $system-name) :keyword)
  "The name <system-name>.library for the mk-defsystem system which
specifies the location of the shared dll for the c adapter code.")

(defvar $system-bindings-name
  (intern (format nil "~@:(~A.bindings~)" $system-name) :keyword))

;; flesh out definitions for the .bindings and .library systems.
;; arrange for claw-cxx to put the generated files under gen/lib and
;; gen/bindings.

#+nil
(macroexpand-1
 '(claw.util:claw-cxx-defsystems
   #.$system-name
   :source-pathname #.$base-dir
   :dll-pathname #.(merge-pathnames "lib/" $base-dir)
   :binary-pathname
   #.(if $fasl-root
       (let ((user::*binary-directory-fasl-root* $fasl-root))
	 (user::binary-directory ""))
       (user::binary-directory $base-dir))
   :generate-adapter-p common-lisp:t
   :compiler-options #.$compiler-options))

;; write out the defsystem file from the above definitions.

#+nil
(with-open-file (stream (concatenate 'string (namestring $system-file)
				     ".tmp")
			:direction :output
			:if-exists :supersede)
  (let ((*package* (or (find-package "EMPTY") (make-package "EMPTY" :use nil))))
    (let ((forms claw.util::$claw-cxx-defsystems-source-form))
      (assert (eql (car forms) 'progn))
      (map nil (lambda (form) (pprint form stream) (terpri stream))
	   (cdr forms)))))

;; load claw-cxx/wrapper to define the wrapper and generate bindings

#+nil
(mk:oos :claw-cxx :load)

;; define the wrapper, this outputs files under base-dir/gen

#+nil
(claw.wrapper:defwrapper
    (#.$wrapper-name
     ;; (:system #.system-name)
     (:base-path #.(merge-pathnames "gen/" $base-dir ))
     (:headers #.$custom-header-file-name)
     #.(cons :includes $includes-paths)
     #.(cons :system-includes $system-includes-paths)
     (:defines)
     (:targets
      ((:and :x86-64 :linux) "x86_64-pc-linux-gnu"))
     (:include-definitions "."))
  :in-package #.$package-name
  :trim-enum-prefix common-lisp:nil
  :recognize-bitfields common-lisp:t
  :recognize-strings common-lisp:t
  ;; :with-adapter (:dynamic :path "lib/adapter.c")
  )

;; load libresect to generate the bindings

#+nil
(unless (cffi:find-foreign-library "libresect")
  (cffi:load-foreign-library "libresect.so"))

;; generate the bindings

#+nil
(claw.wrapper:generate-wrapper $wrapper-name)

#+nil
(load $system-file)

#+nil
(mk:oos $system-bindings-name :load)

#+nil
(unless (cffi:find-foreign-library "Imlib2")
  (cffi:load-foreign-library "libImlib2.so"))

#+nil
(cffi:foreign-symbol-pointer "imlib_context_set_image")

#+nil
(setq $conlicts (claw.util:use-spec-package "CLAW-CXX-IMLIB2" "CL-CLAW-IMLIB2-USER"))

#+nil
(package-shadowing-symbols "CL-CLAW-IMLIB2-USER")

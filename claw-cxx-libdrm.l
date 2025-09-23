;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Aug 29 14:39:47 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;

(in-package "CL-USER")

(progn
(load "~/cl/extern/claw-cxx/claw-cxx.system")
(require 'claw-cxx/util))

(ensure-directories-exist "/dev/shm/claw-cxx-libdrm/" :verbose t)
(wildset-lpn-translations "DRMSRC" "/dev/shm/claw-cxx-libdrm/" :wipe t)

#++nil
(translate-logical-pathname "DRMSRC:;")

#+nil
(claw.util:claw-cxx-defsystems
 "claw-cxx-libdrm"
 :source-pathname "DRMSRC:gen;"
 :binary-pathname "DRMSRC:fasl;"
 :compiler-options (claw.util:make-compiler-options)
 :dll-pathname "DRMSRC:lib;")

(make:defsystem "claw-cxx-libdrm.bindings"
  :source-pathname (translate-logical-pathname "DRMSRC:")
  :binary-pathname (let ((*binary-directory-fasl-root*
			  (translate-logical-pathname "DRMSRC:fasl;")))
		     (binary-directory ""))
  :source-extension "lisp"
  :depends-on ("uiop" "cffi")
  :components ((:module "gen" :components
		((:module "bindings" :components
		  ((:file "x86_64-pc-linux-gnu"
		    #+:lispworks-personal-edition :load-only
		    #+:lispworks-personal-edition t
		    :if-feature (:and :x86-64 :linux))
		   #+nil
		   (:file "i686-pc-linux-gnu" :if-feature (:and :x86 :linux))))))))

#+nil
(mk:oos :claw-cxx-libdrm.bindings :compile)


#||
(make:defsystem "claw-cxx-libdrm.library"
  :source-pathname "DRMSRC:gen;"
  :binary-pathname "DRMSRC:lib;"
  :source-extension "lisp"
  :depends-on ("uiop" "cffi")
  :components ((:module "gen" :components
		((:module "lib" :components
		  ((:file "adapter.x86_64-pc-linux-gnu"
		    :source-extension "c"
		    :if-feature (:and :x86-64 :linux)
		    :language :claw-cxx-adapter :compiler-options
		    (claw.util:make-compiler-options
		     :link-libs "libdrm"
		     ))))))))

#+nil
(mk:oos :claw-cxx-libdrm.library :compile)
||#


;;; ----------------------------------------------------------------------
;;;
;;; GENERATE BINDINGS
;;;

#+nil
(require 'claw-cxx)

#+nil
(unless (cffi:find-foreign-library "libresect.so")
  (cffi:load-foreign-library "libresect.so"))

#||
/7/gtk/redshift/src/gamma-drm.c
pkg-config --cflags libdrm
pkg-config --libs libdrm
||#

#+nil
(claw.wrapper:claw-cxx-defwrapper
 "claw-cxx-libdrm"
 :package "CLAW-CXX-LIBDRM"
 :base-path "DRMSRC:gen;"
 :headers ( "xf86drm.h" "xf86drmMode.h" )
 :system-includes ("/usr/include" "/usr/include/libdrm"))


#+nil
(claw.wrapper:defwrapper ("claw-cxx-libdrm"
			  #+nil
			  (:system "claw-cxx-libdrm")
                          (:base-path "DRMSRC:gen;")
                          (:headers ;; "drm.h"
			   "xf86drm.h" "xf86drmMode.h"
			   )
			  (:includes "/dev/shm/claw-cxx-libdrm")
                          (:system-includes "/usr/include"
			   "/opt/llvm-20.1.8/include"
                           "/usr/include/libdrm")
                          (:targets
                           ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
			   #+nil
                           ((:and :x86 :linux) "i686-pc-linux-gnu"))
                          (:include-definitions "."))
  :in-package "CLAW-CXX-LIBDRM"
  :trim-enum-prefix nil
  :recognize-bitfields t
  :recognize-strings t
  ;; :with-adapter (:dynamic :path "lib/adapter.c")
  :inline-functions nil
  )

#+nil
(claw:generate-wrapper "claw-cxx-libdrm")



;;; ----------------------------------------------------------------------
;;;
;;;

;;; CLAW BUG ENUMS DEFINED INSIDE STRUCTS


#||
;; Unknown CFFI type CLAW-CXX-LIBDRM::|C:@S@DRM-BUF-DESC@E@DRM.H@9489|
;; Unknown CFFI type CLAW-CXX-LIBDRM::|C:@S@DRM-CONTROL@E@DRM.H@4770|

sed -i \
-e 's/claw-cxx-libdrm::|C:@S@DRM-BUF-DESC@E@DRM.H@9489|/:unsigned-long/g' \
-e 's/claw-cxx-libdrm::|C:@S@DRM-CONTROL@E@DRM.H@4770|/:unsigned-long/' \
 "/dev/shm/claw-cxx-libdrm/gen/bindings/x86_64-pc-linux-gnu.lisp"

(cffi:defctype CLAW-CXX-LIBDRM::C\:@S@DRM-BUF-DESC@E@DRM.H@9489 :unsigned-long)
(cffi:defctype  CLAW-CXX-LIBDRM::C\:@S@DRM-CONTROL@E@DRM.H@4770 :unsigned-long)
||#

#||
/7/gtk/libresect/build/resect-test -I /usr/include/drm /usr/include/drm/drm.h
(cd /usr/include/libdrm && cl-bindgen f -a -I/usr/include/drm -- /usr/include/drm/drm.h)
(cd /usr/include && cl-bindgen f stdio.h)
cl-bindgen f -h
cl-bindgen -h
anonymous enums defined in structs not processed by claw-cxx

(cffi:defcstruct (claw-cxx-libdrm::drm-control :size 8)
  (claw-cxx-libdrm::func :unsigned-long ;; edited
   :offset 0)
  (claw-cxx-libdrm::irq :int :offset 4))

(cffi:defcstruct (claw-cxx-libdrm::drm-control :size 8)
  (claw-cxx-libdrm::func :unsigned-long ;; edited
   :offset 0)
  (claw-cxx-libdrm::irq :int :offset 4))
||#

(unless (cffi:find-foreign-library "libdrm")
  (cffi:load-foreign-library "libdrm.so"))


;;; ----------------------------------------------------------------------
;;;
;;; USE CFFI-OBJECT
;;;

(mk:oos 'cffi-object :load)

(setf (macro-function 'cffi::defcfun) (macro-function 'cobj::defcobjfun))
(assert (eql (macro-function 'cffi::defcfun)
	     (macro-function 'cffi-object::defcobjfun)))


#+nil ;; restore
(setf (macro-function 'cffi::defcfun) cffi-object::+defcfun+)

;; load bindings file with defcobjfun
(mk:oos :claw-cxx-libdrm.bindings :load :compile-during-load nil :load-source-instead-of-binary t)


;;; ----------------------------------------------------------------------
;;;
;;;  LIBDRM-USER
;;;

(common-lisp:in-package :claw-cxx-libdrm-bindings~pristine)
*package*

;; THE MEAT
(cobj::define-package-cobject-classes (:claw-cxx-libdrm :claw-cxx-libdrm))

#+nil
(mapcar 'symbol-name
	(claw.util:use-spec-package "CLAW-CXX-LIBDRM"
				    "CL"
				     :dry-run-p t))

(in-package :CL-USER)

#+nil
(when (find-package "LIBDRM-USER") (delete-package "LIBDRM-USER"))

(defpackage "LIBDRM-USER"
  (:use "CL" "CLAW-CXX-LIBDRM")
  (:shadow "COUNT" "LENGTH" "LIST" "MAP" "MOD"
   "SIGNAL" "SEQUENCE" "TYPE" "VALUES"))


(in-package "LIBDRM-USER")


(defun cobj-get-definables (spec-package)
  (loop	with defn
	for s being each symbol of spec-package
	when (and (find-class s nil)
		  (setq defn (ignore-errors
			       (cobj::cobject-class-definition s))))
	collect defn))

#+nil
(cobj-get-definables :claw-cxx-libdrm)

(defun cobj-get-exportables (spec-package &key (ignore-already-exported t))
  (loop for defn in (cobj-get-definables spec-package)
	append
	(remove-if
	 (lambda (s)
	   (multiple-value-bind (sym stat)
	       (find-symbol (string s) spec-package)
	     (assert (eql sym s))
	     (assert (eql (symbol-package sym) (find-package spec-package)))
	     (and ignore-already-exported
		  (eql stat :external))))
	 (cobj::cobject-class-definition-symbols defn))))

#+nil
(cobj::cobject-class-definition-symbols
 (cobj::cobject-class-definition '%drm-mode-object-properties))


(export (cobj-get-exportables :claw-cxx-libdrm
			      :ignore-already-exported nil)
	:claw-cxx-libdrm)



;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defstruct conn fd res)

(defvar $c (make-conn))

(defun call-perror-fn (dispatch-fn fmt fmt-args)
  (apply dispatch-fn "~@?: ~A" fmt (append fmt-args (cl:list (cffi:foreign-funcall "strerror" :int (cffi:mem-ref (cffi:foreign-funcall  "__errno_location" :pointer) :int) :string)))))

(defun sys-error (fmt &rest fmt-args)
  (call-perror-fn #'error fmt fmt-args))

(defun sys-cerror (fmt &rest fmt-args)
  (with-simple-restart (cont "continue, ignoring sys-error")
    (apply #'sys-error fmt fmt-args)))

(defun conn-open (c &key (card-string "/dev/dri/card0"))
  (with-slots (fd res) c
    (assert (null fd) nil "close ~A first" c)
    (let ((ret (cffi:foreign-funcall "open" :string card-string
				     :int (logior #o00000002
						  #o02000000)
				     :int)))
      (if (< ret 0)
	  (sys-error "opening ~S" card-string)
	  (setq fd ret)))
    (setq res (drm-mode-get-resources fd))
    c))

#||
;; CCL ERROR: compiler macro calls funcall-dynamic-extent-form which
is not defined.
(compile nil (lambda (c)
	       (with-slots (res) c
		 (drm-mode-free-resources  res)
		 )e))
cobj::funcall-dynamic-extent-form
cobj::funcall-form-type
(cobj::funcall-dynamic-extent-form 'drm-mode-free-resources nil)
(cobj::define-struct-cobject-class %drm-mode-res)
(cobj::define-struct-cobject-class %drm-mode-property)
||#

(defun conn-close (c)
  (with-slots (fd res) c
    (when (and fd #+nil (> fd 0))
      (if (< (cffi:foreign-funcall "close" :int fd :int) 0)
	  (sys-error "closing fd ~D" fd))
      (setq fd nil))
    (when res
      ;; FIXME CCL BUG ON (drm-mode-free-resource)
      (%drm-mode-free-resources (cobj:cobject-pointer res))
      (setq res nil))
    c))

(defun get-connectors (c)
  (with-slots (res) c
    (loop for i below (drm-mode-res-count-connectors res)
	  collect (cobj:cref (drm-mode-res-connectors res) i))))

(defun get-crtcs (c)
  (with-slots (res) c
    (loop with crtcs = (%drm-mode-res-crtcs res)
	  for i below (%drm-mode-res-count-crtcs res)
	  collect (cobj:cref crtcs i))))

(defun cobj-nullp (obj)
  (cffi:null-pointer-p (cobj:cobject-pointer obj)))

(defmacro let-finally ((binding-var binding-form finally-form) &body body)
  `(let ((,binding-var nil))
     (unwind-protect
	  (progn
	    (setq ,binding-var ,binding-form)
	    ,@body)
       (when ,binding-var
	 ,finally-form))))

(defun get-prop-prop-values (props)
  "Return (PROP-ID .  VALUE)"
  (loop for i below (drm-mode-object-properties-count-props props)
	collect (cons (cobj:cref (drm-mode-object-properties-props props) i)
		      (cobj:cref (drm-mode-object-properties-prop-values props) i))))

(defun get-connector-properties (c)
  (with-slots (fd) c
    (loop for c-id in (get-connectors c) collect
	  (let-finally (c1 (drm-mode-get-connector fd c-id)
			   (drm-mode-free-connector c1))
	    (if (cffi:null-pointer-p (cobj:cobject-pointer c1))
		(sys-cerror "could not get connector ~d" c-id))
	    (assert (= c-id (drm-mode-connector-connector-id c1)))
	    (cl:list c-id
		     (drm-mode-get-connector-type-name
		      (drm-mode-connector-connector-type c1))
		     (drm-mode-connector-connector-type-id c1)
		     (let-finally (props (drm-mode-object-get-properties fd c-id +DRM-MODE-OBJECT-CONNECTOR+)
					 (drm-mode-free-object-properties props))
		       (get-prop-prop-values props)))))))

#+nil
(get-connector-properties $c)

(defun get-crtc-properties (c)
  (with-slots (fd) c
    (loop for c-id in (get-crtcs c) collect
	  (let-finally (crtc (drm-mode-get-crtc fd c-id)
			     (drm-mode-free-crtc crtc))
	    (if (cobj-nullp crtc)
		(sys-cerror "could not get crtc ~D" c-id))
	    (cl:list c-id
		     (%drm-mode-crtc-crtc-id crtc)
		     (drm-mode-crtc-crtc-id crtc)
		     (let-finally (props (drm-mode-object-get-properties fd c-id +DRM-MODE-OBJECT-CRTC+)
					 (drm-mode-free-object-properties props))
		       (get-prop-prop-values props)))))))

#+nil
(get-crtc-properties $c)



#||
(conn-open $c)
(conn-close $c)
(get-connectors $c)
(get-crtcs $c)
(cffi:null-pointer-p (cobj:cobject-pointer (drm-mode-get-connector (conn-fd $c) 10)))
(get-connector-properties $c)
(mapcar 'type-of (cl:list (conn-res $c) (drm-mode-get-resources (conn-fd $c))))
(cobj:cpointer-eq (conn-res $c) (drm-mode-get-resources (conn-fd $c)))
(cl:list (conn-res $c) (drm-mode-get-resources (conn-fd $c)))
||#

(defun show-cobj (obj &aux (old-obj obj) converted-p)
  (when (typep obj 'cobj:cpointer)
    (setq obj
	  (cobj:pointer-cobject (cobj:cobject-pointer obj)
				(cobj::cpointer-element-type obj)))
    (setq converted-p t))
  (let* ((class (class-of obj))
	 (class-name (class-name class))
	 (cobj-defn (cobj::cobject-class-definition class-name))
	 (acc-alist
	  (cobj::cobject-class-definition-slot-accessors cobj-defn))
	 (ret (loop for (sym . acc) in acc-alist
			   collect (cons sym (funcall acc obj)))))
    (cl:values (cons (cobj:cobject-pointer obj)
		     ret)
	       converted-p)))

#||
+DRM-MODE-OBJECT-CONNECTOR+
+DRM-MODE-OBJECT-CRTC+
(drm-mode-free-object-properties $props)
(setq $props (drm-mode-object-get-properties (conn-fd $c) 241 +DRM-MODE-OBJECT-CONNECTOR+))
(show-cobj $props)
(show-cobj (cobj:cref $props 0))
(setq $p6 (cobj:cref $props 6))
(show-cobj $p6)
(setq $p6-prop (drm-mode-get-property (conn-fd $c) 6))
(%dump-prop $p6-prop)
||#

(defun dump-blob (c blob-id &optional (stream t))
  (let-finally (blob (drm-mode-get-property-blob (conn-fd c) blob-id)
		     (drm-mode-free-property-blob blob))
    (when (cobj-nullp blob)
      (format stream "~%")
      (return-from dump-blob nil))
    (let ((blob-data (cobj:cobject-pointer (%drm-mode-property-blob-data blob))))
      (loop for i below (%drm-mode-property-blob-length blob)
	    if (zerop (cl:mod i 16))
	    do (format stream "~%~C~C~C" #\Tab #\Tab #\Tab)
	    end
	    do (format stream "~(~2,'0X~)" (cffi:mem-aref blob-data :unsigned-char i)))
      (terpri stream))))

#+nil
(dump-blob $c 242)

(defvar $drm-mode-flags '(+DRM-MODE-PROP-PENDING+
			  +DRM-MODE-PROP-IMMUTABLE+
			  +DRM-MODE-PROP-SIGNED-RANGE+
			  +DRM-MODE-PROP-RANGE+
			  +DRM-MODE-PROP-ENUM+
			  +DRM-MODE-PROP-BITMASK+
			  +DRM-MODE-PROP-BLOB+
			  +DRM-MODE-PROP-OBJECT+))

;; xf86drmMode.h static inlines
(defun drm-mode-get-property-type (prop)
  (let ((ret
	 (logand (%drm-mode-property-flags prop)
		 (logior +DRM-MODE-PROP-LEGACY-TYPE+
			 +DRM-MODE-PROP-EXTENDED-TYPE+))))
    (cl:values ret
	       (loop for i in $drm-mode-flags
		     for ok = (= ret (symbol-value i))
		     if ok
		     collect i))))

(defun drm-property-type-is (prop type)
  (= (drm-mode-get-property-type prop) type))

(defun %dump-prop (prop &optional (stream t))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (format stream " ~A:~&"
	  (cobj:ccoerce (%drm-mode-property-name prop) 'string))
  (format stream "~C~Cflags:" #\Tab #\Tab)
  (let ((f (%drm-mode-property-flags prop)))
    (when (plusp (logand f +DRM-MODE-PROP-PENDING+))
      (format stream " pending"))
    (when (plusp (logand f +DRM-MODE-PROP-IMMUTABLE+))
      (format stream " immutable"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-SIGNED-RANGE+)
      (format stream " signed range"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-RANGE+)
      (format stream " range"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-ENUM+)
      (format stream " enum"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-BITMASK+)
      (format stream " bitmask"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-BLOB+)
      (format stream " blob"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-OBJECT+)
      (format stream " object")))
  (terpri stream)
  (when (drm-property-type-is prop +DRM-MODE-PROP-SIGNED-RANGE+)
    (format stream "~C~Cvalues:" #\Tab #\Tab)
    (loop for i below (%drm-mode-property-count-values prop)
	  do (format stream " ~D" ;; (coerce U642I64 (prop->values[i]))
		     (cobj:cref (%drm-mode-property-values prop) i)))
    (terpri stream))
  (when (drm-property-type-is prop +DRM-MODE-PROP-RANGE+)
    (format stream "~C~Cvalues:" #\Tab #\Tab)
    (loop for i below (%drm-mode-property-count-values prop)
	  do (format stream " ~D"
		     (cobj:cref (%drm-mode-property-values prop) i)))
    (terpri stream))
  (cond ((drm-property-type-is prop +DRM-MODE-PROP-ENUM+)
	 (format stream "~C~Cenums:" #\Tab #\Tab)
	 (loop for i below (%drm-mode-property-count-enums prop)
	       for e = (cobj:cref (%drm-mode-property-enums prop) i)
	       do (format stream " ~A=~D"
			  (cobj:ccoerce (drm-mode-property-enum-name e) 'string)
			  (drm-mode-property-enum-value e)))
	 (terpri stream))
	((drm-property-type-is prop +DRM-MODE-PROP-BITMASK+)
	 (format stream "~C~Cvalues:" #\Tab #\Tab)
	 (loop for i below (%drm-mode-property-count-enums prop)
	       for b = (cobj:cref (%drm-mode-property-enums prop) i)
	       do (format stream " ~A=#x~X"
			  (cobj:ccoerce (drm-mode-property-enum-name b) 'string)
			  (ash 1 (drm-mode-property-enum-value b))))
	 (terpri stream))
	(t (assert (zerop (%drm-mode-property-count-enums prop))))))


#+nil
(%dump-prop $p6-prop t)

(defun dump-prop (c prop-id value &optional (stream t))
  (let-finally (prop (drm-mode-get-property (conn-fd c) prop-id)
		     (drm-mode-free-property prop))
    (format stream "~C~D" #\Tab prop-id)
    (when (cobj-nullp prop)
      (format stream "~%")
      (return-from dump-prop nil))
    (%dump-prop prop stream)
    (cond ((drm-property-type-is prop +DRM-MODE-PROP-BLOB+)
	   (format stream "~C~Cblobs:~%" #\Tab #\Tab)
	   (loop for i below (%drm-mode-property-count-blobs prop)
		 do (dump-blob c (cobj:cref (%drm-mode-property-blob-ids prop) i) stream)))
	  (t (assert (zerop (%drm-mode-property-count-blobs prop)))))
    (format stream "~C~Cvalue:" #\Tab #\Tab)
    (cond ((drm-property-type-is prop +DRM-MODE-PROP-BLOB+)
	   (dump-blob c value stream))
	  ((drm-property-type-is prop +DRM-MODE-PROP-SIGNED-RANGE+)
	   (format stream " ~D" value))
	  (t (format stream " ~D" value))))
  (terpri stream))

(defun list-object-properties (c id type &optional (stream t))
  (check-type type (member #.+DRM-MODE-OBJECT-CONNECTOR+ #.+DRM-MODE-OBJECT-CRTC+))
  (let-finally (props (drm-mode-object-get-properties
		       (conn-fd c) id type)
		      (drm-mode-free-object-properties props))
    (loop with p-props = (drm-mode-object-properties-props props)
	  with p-values = (drm-mode-object-properties-prop-values props)
	  for i below (drm-mode-object-properties-count-props props)
	  for p-id = (cobj:cref p-props i)
	  for p-value = (cobj:cref p-values i)
	  do (dump-prop c p-id p-value stream)
	  collect (cons p-id p-value))))

#+nil
(list-object-properties $c 241 +DRM-MODE-OBJECT-CONNECTOR+)

(defun list-connector-properties (c &optional (stream t))
  (with-slots (fd res) c
    (loop with conns = (drm-mode-res-connectors res)
	  for i below (drm-mode-res-count-connectors res)
	  for conn-id = (cobj:cref conns i)
	  do (let-finally (conn (drm-mode-get-connector fd conn-id)
				(drm-mode-free-connector conn))
	       (if (cobj-nullp conn)
		   (sys-cerror "could not get connector ~D" conn-id))
	       (format stream "Connector ~D (~A-~D)~%"
		       (drm-mode-connector-connector-id conn)
		       (drm-mode-get-connector-type-name
			(drm-mode-connector-connector-type conn))
		       (drm-mode-connector-connector-type-id conn))
	       (list-object-properties
		c (drm-mode-connector-connector-id conn)
		+DRM-MODE-OBJECT-CONNECTOR+)))))

(defun list-crtc-properties (c &optional (stream t))
  (with-slots (fd res) c
    (loop with crtcs = (drm-mode-res-crtcs res)
	  for i below (drm-mode-res-count-crtcs (conn-res $c))
	  for crtc-i = (cobj:cref crtcs i)
	  do (let-finally (crtc (drm-mode-get-crtc fd crtc-i)
				(drm-mode-free-crtc crtc))
	       (if (cobj-nullp crtc)
		   (sys-cerror "could not get crtc ~D" crtc-i))
	       (let ((id (%drm-mode-crtc-crtc-id crtc)))
		 (assert (= id crtc-i))
		 (format stream "CRTC ~D~%" id)
		 (list-object-properties c id +DRM-MODE-OBJECT-CRTC+ stream))))))

(defun set-property (c obj-id obj-type prop-id value)
  (drm-mode-object-set-property (conn-fd c) obj-id obj-type prop-id value))

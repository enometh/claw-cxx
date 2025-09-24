(cl:in-package "CL-LIBDRM-IMPL")

;; THE MEAT
(cobj::define-package-cobject-classes (:claw-cxx-libdrm :claw-cxx-libdrm))

#+nil ;; maybe restore
(cl:setf (cl:macro-function 'cffi::defcfun) cl:cffi-object::+defcfun+)

(cl:defun cobj-get-definables (spec-package)
  (cl:loop	with defn
		for s being each symbol of spec-package
		when (cl:and (cl:find-class s cl:nil)
			     (cl:setq defn (cl:ignore-errors
					     (cobj::cobject-class-definition s))))
		collect defn))

#+nil
(cobj-get-definables :claw-cxx-libdrm)

(cl:defun cobj-get-exportables (spec-package cl:&key (ignore-already-exported cl:t))
  (cl:loop for defn in (cobj-get-definables spec-package)
	   append
	   (cl:remove-if
	    (cl:lambda (s)
	      (cl:multiple-value-bind (sym stat)
		  (cl:find-symbol (cl:string s) spec-package)
		(cl:assert (cl:eql sym s))
		(cl:assert (cl:eql (cl:symbol-package sym)
				   (cl:find-package spec-package)))
		(cl:and ignore-already-exported
			(cl:eql stat :external))))
	    (cobj::cobject-class-definition-symbols defn))))

#+nil
(cobj::cobject-class-definition-symbols
 (cobj::cobject-class-definition 'claw-cxx-libdrm:%drm-mode-object-properties))

#+nil
(cobj-get-exportables :claw-cxx-libdrm
		      :ignore-already-exported cl:nil)

;; EXPORT
(cl:export (cobj-get-exportables :claw-cxx-libdrm
				 :ignore-already-exported cl:t)
	:claw-cxx-libdrm)

;; XXX
(cl:unless (cffi:find-foreign-library "libdrm")
  (cffi:load-foreign-library "libdrm.so"))

;; GET CONFLICTS WITH CL
#+nil
(cl:let ((spec-package :claw-cxx-libdrm)
	 (package :cl))
  (cl:labels ((get-exports (spec-package)
		(cl:loop for sym being each external-symbol of spec-package
			 when (cl:eql (cl:symbol-package sym) spec-package)
			 collect sym))
	      (get-conflicts (spec-package cl:&optional (package cl:*package*))
		(cl:loop for ext-sym in (get-exports spec-package)
			 for nam = (cl:symbol-name ext-sym)
			 for (sym status) = (cl:multiple-value-list
					     (cl:find-symbol nam package))
			       if (cl:and status (cl:not (cl:eql sym ext-sym)))
			       collect sym)))
    (cl:setq spec-package (cl:find-package spec-package))
    (cl:setq package (cl:find-package package))
    (cl:let ((conflicts (get-conflicts spec-package package)))
      (cl:sort
       (cl:mapcar (cl:lambda (x)
		    (cl:let ((sym (cl:find-symbol (cl:symbol-name x) spec-package)))
			    (cl:assert (cl:eql (cl:symbol-package sym) spec-package))
			    (cl:string sym)))
		  conflicts)
       #'cl:string<))))

;; => ("COUNT" "LENGTH" "MOD" "TYPE" "LIST" "SEQUENCE" "SIGNAL" "VALUES" "MAP")


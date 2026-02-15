;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
(defpackage "CLAW-MK-DEFSYSTEM-ADAPTER"
  (:use "CL")
  (:export "+COMPILER+"))
(in-package "CLAW-MK-DEFSYSTEM-ADAPTER")


;;; ----------------------------------------------------------------------
;;;
;;; MK DEFSYSTEM INTEGRATION
;;;
;; use :language :claw-cxx-adapter in the component's :language to
;; compile the and load the c adapter file. pass cflags and ldflags
;; via the :compiler-option slot for the component in the defsystem
;; form. e.g.
;;
;; :components
;; ((:file "bindings/x86_64-pc-linux-gnu"
;;   :if-feature (:and :x86-64 :linux))
;;  (:module "lib"
;;     :components
;;     ((:file "adapter.x86_64-pc-linux-gnu.c"
;;       :if-feature (:and :x86-64 :linux)
;;       :language :claw-cxx-adapter
;;       :compiler-options (:cflags "-O0 -g3"
;;			  :ldflags "-Wl,-O1 -Wl,--as-needed"))))
;;
;; NOTE: mk-defsystem puts all binary files under the binary-pathname
;; supplied to the main defsystem form, and it is customary to specify
;; a binary-directory root which is specific to a particular lisp
;; implementation. HOWEVER the dlls do not need to be implementaion
;; specific. so it may be expedient to define a separate system for
;; just loading the dll.

(defvar +compiler+ #+nil "clang"
	"gcc")

;;madhu 250906 - atdoc magic-pathname, don't forget to set the
;;variable to the value returned by fix-sbcl-homedir.
#+sbcl
(defun fix-sbcl-homedir (file)
  (unless (pathnamep file) (setq file (pathname file)))
  (let ((pd (pathname-directory file)))
    (when (and (eql (car pd) :absolute)
	       (eql (second pd) :home))
      (let ((uhpd (pathname-directory (user-homedir-pathname))))
	(assert (eql (car uhpd) :absolute))
	(rplacd pd
	      (append (copy-seq (cdr uhpd))
		      (cddr pd))))))
  file)

(defun mk-adapter-cc (input-file &rest args &key output-file force
		      &allow-other-keys)
  (format t "~%~S~%" `(mk-adapter-cc ,input-file ,@args))
  (assert (cl-user::suffixp ".so" output-file))
  #+sbcl
  (progn (setq input-file (fix-sbcl-homedir input-file))
	 (setq output-file (fix-sbcl-homedir output-file)))
  (let* ((object-file (make-pathname :type "o" :defaults output-file))
	 (cflags (getf args :cflags))
	 (ldflags (getf args :ldflags)))
    (flet ((compile-if-newer (source target fmt-command &rest args)
	     (when (or force (not (probe-file target))
		       (< (file-write-date target)
			  (file-write-date source)))
	       (let ((command (apply #'format nil fmt-command args)))
		 (format t "~&~A~%" command)
		 (uiop:run-program command
				   :error-output *error-output*
				   :output *standard-output*)))))
      (compile-if-newer input-file object-file
			"~a -c -fPIC -DPIC ~S -o ~S~@[ ~a~]" +compiler+
			(namestring input-file)
			(namestring object-file)
			cflags)
      (compile-if-newer object-file output-file
			"~a -shared -fPIC -DPIC -ldl ~S -o ~S~@[ ~a~]" +compiler+
			(namestring object-file)
			(namestring output-file)
			ldflags))))

(defun mk-adapter-ld (filespec &rest args)
  (format t "~%~S~%" `(mk-adapter-ld ,filespec ,@args))
  (cffi:load-foreign-library filespec))

#+mk-defsystem
(mk:define-language :claw-cxx-adapter
    :compiler 'mk-adapter-cc
    :loader 'mk-adapter-ld
    :source-extension "c"
    :binary-extension "so"  ; or dll, dylib?
    :output-files
    (lambda (c)
      (let* ((input (mk::component-full-pathname c :source))
	     (output (mk::component-full-pathname c :binary))
	     (object (make-pathname :type "o" :defaults output)))
	(list object output))))

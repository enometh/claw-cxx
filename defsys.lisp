(in-package "CLAW-CXX-SDL3-USER")

(eval-when (load eval compile)
(defun setup-compiler-options ()
  (setq $compiler-options
	(claw.util:make-compiler-options
	 :link-libs $link-libs
	 :default-cflags nil
	 :default-ldflags $default-ldflags
	 :system-includes-paths $system-includes-paths
	 :include-paths $includes-paths
	 :link-libs-paths $link-libs-paths))))

(eval-when (load eval compile)
(defvar $compiler-options (setup-compiler-options)))

(claw.util:claw-cxx-defsystems #.$system-name
			       :source-pathname $base-dir
			       :dll-pathname (merge-pathnames "lib/" $base-dir)
			       :binary-pathname
			       (if $fasl-root
			       (let ((user::*binary-directory-fasl-root* $fasl-root))
				 (user::binary-directory ""))
			       (user::binary-directory $base-dir))
			       :generate-adapter-p t
			       :compiler-options #.$compiler-options)


(defun dump-defsystem-file (&key (if-exists :supersede))
  (or (and (probe-file $system-file)
	   (not (member if-exists '(:supersede :overwrite))))
      (user::dump-sexp-to-file claw.util::$claw-cxx-defsystems-source-form
			       $system-file)))

#+nil
(dump-defsystem-file)

#||
(load (merge-pathnames $base-dir $system-file))

;; compile-dll
#+nil
(mk:mk-oos $system-library-name :compile :compile-during-load t)

;; load dll
#+nil
(unless (cffi:find-foreign-library
	 (format nil "~(~a~)/lib/gen/lib/adapter.x86_64-pc-linux-gnu.so"
		 $system-name))
(mk:mk-oos $system-library-name :load :compile-during-load nil))

 ;; load sources without compiling only
#+nil
(let ((MK::*operations-propagate-to-subsystems* nil))
  (mk:oos $system-bindings-name :load  :compile-during-load nil :load-source-instead-of-binary t :minimal-load t))
||#
(in-package "CLAW-CXX-SDL3-USER")

#+nil
(unless (cffi:find-foreign-library "libresect")
  (cffi:load-foreign-library "libresect.so"))

#+nil
(unless (cffi:find-foreign-library "libresect")
  (cffi:load-foreign-library "/home/madhu/scratch/extern/libresect/build/libresect.so"))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

;; make sure a.h is present where defwrapper expects to find it.
#+nil
(setup-base-dir)


;;#+nil
(claw.wrapper:defwrapper (#.$wrapper-name
;;                          (:system #.system-name)
                          (:base-path #.(merge-pathnames "gen/" $base-dir))
                          (:headers "a.h")
			  #.(cons :includes $includes-paths)
                          #.(cons :system-includes $system-includes-paths)
			  (:defines)
                          (:targets
                           ((:and :x86-64 :linux) "x86_64-pc-linux-gnu"))
                          (:include-definitions "."))
                         :in-package
			 #.$package-name
                         :trim-enum-prefix
                         common-lisp:nil
                         :recognize-bitfields
                         common-lisp:t
                         :recognize-strings
                         common-lisp:t
                         :with-adapter
                         (:dynamic :path "lib/adapter.c"))

#+nil
(setq claw.generator.common:*delete-duplicate-definitions* t)

#+nil
(claw.wrapper:generate-wrapper $wrapper-name)

#||
(let ((claw.wrapper::*always-generate* nil)))
(claw.wrapper:load-wrapper $wrapper-name))
(setq $opts (car (setq $wrapper-def (gethash $wrapper-name claw.wrapper::*wrapper-registry*))))
(setq $config (cdr $wrapper-def))
(setq $opts1 (claw.wrapper::eval-opts $wrapper-name $opts))
(claw.wrapper::wrapper-options-persistent $opts1)
(let ((claw.wrapper::*always-generate* nil)
      (claw.wrapper::*path-mapper*
       (lambda (path)
	 (claw.wrapper::find-path
	  path
	  :system (claw.wrapper::wrapper-options-system $opts1)
	  :path (claw.wrapper::wrapper-options-base-path $opts1)))))
  (setq $bindings-table
	(claw.wrapper::make-bindings-table $wrapper-name
					   $opts1
					   $config)))
(claw.wrapper::wrapper-options-targets $opts1)
(setq $bindings (claw.wrapper::expand-bindings $opts1 $bindings-table))
(let ((*package* (find-package $package-name)))
  (eval $bindings))
||#

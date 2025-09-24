(cl:in-package "CL-LIBDRM-IMPL")

(cl:setf (cl:macro-function 'cffi::defcfun)
	 (cl:macro-function 'cobj::defcobjfun))

(cl:assert (cl:eql (cl:macro-function 'cffi::defcfun)
		   (cl:macro-function 'cffi-object::defcobjfun)))

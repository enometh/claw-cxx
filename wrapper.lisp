(cl:in-package "CL-CLAW-MUPDF")

(claw:defwrapper (:claw-cxx-mupdf
		  (:system :cl-claw-mupdf)
		  (:base-path
		   (mk::system-relative-pathname :cl-claw-mupdf
						 "gen"))
		  (:headers "mupdf/pdf.h")
		  (:includes  #+nil "/usr/include/"
		   "/14/build/mupdf/include")
		  (:targets
		   ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
		   ((:and :x86 :linux) "i686-pc-linux-gnu"))
		  (:include-definitions ".")
;;		  (:include-definitions "fitz.*")
;;		  (:exclude-definitions "^$")
		  (:spec-path (mk::system-relative-pathname :cl-claw-mupdf
						 "bindings")))
    :in-package :CLAW-CXX-MUPDF
    :trim-enum-prefix nil
    :recognize-bitfields t
    :recognize-strings t
    :with-adapter (:dynamic :path "lib/adapter.c")
    )

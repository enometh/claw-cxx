(cl:in-package "CL-CLAW-MUPDF")

#+nil
(claw.wrapper:claw-cxx-defwrapper :claw-cxx-mupdf)

(claw.wrapper:defwrapper (:claw-cxx-mupdf
;;		  (:system :cl-claw-mupdf)
		  (:base-path
		   (mk::system-relative-pathname :claw-cxx-mupdf
						 "gen"))
		  (:headers "mymupdf.h")
		  (:includes ;; "/usr/include/" "/14/build/mupdf/include"
		   #.(namestring (mk::system-relative-pathname :claw-cxx-mupdf "")))
		  (:targets
		   ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
		   #+nil
		   ((:and :x86 :linux) "i686-pc-linux-gnu"))
;;		  (:include-definitions ".")
		  (:include-definitions "^fitz" "^[fF][zZ]" "^[pP][dD][fF]")
		  (:exclude-definitions ;; "^$"
		   "__log2l" "DBL_DENORM")
		  )
    :in-package :CLAW-CXX-MUPDF
    :trim-enum-prefix nil
    :recognize-bitfields t
    :recognize-strings t
    :with-adapter (:dynamic :path "lib/adapter.c")
    )

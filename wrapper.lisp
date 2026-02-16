(cl:in-package "CL-USER")

#+nil ;;macroexpand for hint
(claw.wrapper:claw-cxx-defwrapper :claw-cxx-mupdf)

(claw.wrapper:defwrapper (:claw-cxx-mupdf
		  (:base-path #.(merge-pathnames "gen/" *cl-claw-mupdf-source-dir*))
		  (:headers "mymupdf.h")
		  (:includes ;; "/usr/include/" "/14/build/mupdf/include"
		   #.(namestring (truename *cl-claw-mupdf-source-dir*)))
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

#+nil
(claw:generate-wrapper :claw-cxx-mupdf)

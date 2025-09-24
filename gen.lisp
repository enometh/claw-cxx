(in-package "CL-LIBDRM-IMPL")

(cl:unless (cffi:find-foreign-library "libresect.so")
  (cffi:load-foreign-library "libresect.so"))

#||
pkg-config --cflags libdrm
pkg-config --libs libdrm
||#

#+cl:nil
(claw.wrapper:claw-cxx-defwrapper
 "claw-cxx-libdrm"
 :package "CLAW-CXX-LIBDRM"
 :base-path "DRMSRC:gen;"
 :headers ( "xf86drm.h" "xf86drmMode.h" )
 :system-includes ("/usr/include" "/usr/include/libdrm"))


(claw.wrapper:defwrapper ("claw-cxx-libdrm"
			  #+cl:nil
			  (:system "claw-cxx-libdrm")
                          (:base-path "DRMSRC:gen1;")
                          (:headers ;; "drm.h"
			   "xf86drm.h" "xf86drmMode.h"
			   )
			  (:includes "/dev/shm/claw-cxx-libdrm")
                          (:system-includes "/usr/include"
                           "/usr/include/libdrm")
                          (:targets
                           ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
			   #+cl:nil
                           ((:and :x86 :linux) "i686-pc-linux-gnu"))
                          (:include-definitions "."))
  :in-package "CLAW-CXX-LIBDRM"
  :trim-enum-prefix cl:nil
  :recognize-bitfields cl:t
  :recognize-strings cl:t
  ;; :with-adapter (:dynamic :path "lib/adapter.c")
  :inline-functions cl:nil
  )

#+nil
(claw:generate-wrapper "claw-cxx-libdrm")

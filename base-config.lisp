(in-package "CLAW-CXX-SDL3-USER")


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
;; $base-dir is where for generated bindings, c-sources and the
;; mk-defsystem files for those live.

(defvar $base-dir "/dev/shm/claw-cxx-sdl3/")

#+nil
(defvar $base-dir "/14/build/EXT-LISP/claw-cxx-sdl3/")

;; the generated mk-defsystem definitions also place the shared
;; objects and c compilation artefacts under base-dir/lib.

;;  if $fasl-root is nil the generated mk-defsystem definitions will
;; put fasl files under
;; <user::*binary-directory-fasl-root*>/cl-claw-sdl3/<lisp-impl-dir>.
;; otherwise the fasl files will go under $fasl-root/<lisp-impl-dir>
;; see lc-lite.lisp from mk-defsystem-3.x for binary-directory.

#+nil
(defvar $fasl-root nil)

(defvar $fasl-root (merge-pathnames "fasl/"  $base-dir))

;; the following variables are for the compiler system. they are used
;; to by generate compiler options to supply to mk-defsystem to
;; compile the generated adapter files into shared objects the include
;; locations are also supplied to claw-wrapper.defwrapper to generate
;; the bindings

#+nil
(defvar $system-includes-paths nil)

(defvar $system-includes-paths
  (list "/home/madhu/root/usr/include"))

(defvar $includes-paths
  (list
   $base-dir))

(defvar $link-libs (list "SDL3"))

#+nil
(defvar $link-libs-paths nil)

(defvar $link-libs-paths
  (list "/home/madhu/root/usr/lib64/"))

(defvar $default-ldflags
  (list "-Wl,--enable-new-dtags"
	(format nil "-Wl,-rpath,~A" (car $link-libs-paths))))

#||
PKG_CONFIG_PATH=~/root/usr/lib64/pkgconfig/ pkg-config --cflags sdl3
PKG_CONFIG_PATH=~/root/usr/lib64/pkgconfig/ pkg-config --libs sdl3
-L/home/madhu/root/usr/lib64/pkgconfig/../../lib64 -Wl,-rpath,/home/madhu/root/usr/lib64/pkgconfig/../../lib64 -Wl,--enable-new-dtags -lSDL3
||#


;; the $inclusives string is written to a header file which is
;; supplied to claw-wrapper.defwrapper.

(defvar $inclusives "

#include <SDL3/SDL.h>
#include <SDL3/SDL_video.h>
")

(defvar $wrapper-name :claw-cxx-sdl3
  "Name supplied to claw.wrapper:defwrapper" )

(defvar $system-name :claw-cxx-sdl3
  "base name for mk-defsystem generated `binding' and `library'systems")

;;

(defvar $package-name "CLAW-CXX-SDL3"
  "the package from which the generated bindings and cffi-object
definitions are exported.")


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defvar $system-file
  (merge-pathnames  (format nil "~(~A.system~)" $system-name)
		    $base-dir)
  "Path to the mk-defsystem definition file that contains 2 systems for
the `bindings' and the `library'")

(defvar $system-library-name
  (intern (format nil "~@:(~A.library~)" $system-name) :keyword)
  "The name <system-name>.library for the mk-defsystem system which
specifies the location of the shared dll for the c adapter code.")

(defvar $system-bindings-name
  (intern (format nil "~@:(~A.bindings~)" $system-name) :keyword)
  "The name <system-name>.bindings for the mk-defsystem system which
specifies the location of the generated lisp bindings.")

(defun setup-base-dir ()
  "Create $base-dir and dump the contents of $INCLUSIVES into a file
called `a.h ' in that directory."
  (ensure-directories-exist $base-dir :verbose t)
  (unless (probe-file (merge-pathnames "a.h" $base-dir))
    (user::string->file $inclusives  (merge-pathnames "a.h" $base-dir))))


(in-package "CLAW-CXX-SDL3-USER")

#+(and clozure nil)
;; cffi-object::defcfun apparently should not be "Compiled
;; Function CFFI:DEFCFUN (Non Global)". it should be
;; "Compiled-function CFFI:DEFCFUN Macroexpander"
(setf (macro-function 'cffi::defcfun) cobj::+defcfun+)

;; replace cffi:defcfun with cobj:defcobjfun
(eval-when (load eval compile)
  (unless (eql (macro-function 'cffi::defcfun)
	       (macro-function 'cobj::defcobjfun))
(setf (macro-function 'cffi::defcfun) (macro-function 'cobj::defcobjfun))))


#+nil
(progn
(require 'cffi-ops)
(require 'cffi-object)
(require 'cffi-object.ops))

;;(cobj.ops:enable-cobject-ops)
;;(cobj.ops:disable-cobject-ops)
;;(macro-function 'cffi:defcfun)

#+clozure
(setq cobj::*optimize-object-allocation-p* nil)
#+nil ;madhu 250105 ;; ccl requires this to be nil
(setq cobj::*optimize-object-allocation-p* t)

#+nil
(list cobj::+defcfun+  (macro-function 'cffi::defcfun) (macro-function 'cffi-object::defcfun) )

(defvar $fail (macro-function 'cffi-object::defcfun))


#+nil
 ;; recover cffi:defcfun and cobj:+defcfun+ if overwrittern by loading
 ;; the systema gain.need to fix cffi-object:defcfun to avoid blowing
 ;; stack.
(progn
(mk:load-system :cffi)
(makunbound 'cobj::+defcfun+)
(setq cobj::+defcfun+ (macro-function 'cffi::defcfun))
(list (macro-function 'cffi::defcfun)  cobj::+defcfun+
      (macro-function 'cffi-object::defcfun)))


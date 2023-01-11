(uiop:define-package :claw.wrapper
  (:use #:cl #:alexandria #:claw.util)
  (:export #:defwrapper
           #:include
           #:load-wrapper
           #:generate-wrapper

           #:wrapper-name
           #:wrapper-options
           #:wrapper-configuration
           #:wrapper-target
           #:wrapper-entities
           #:wrapper-always-generate

           #:wrapper-options-standard

           #:wrapper-options-headers
           #:wrapper-options-includes
           #:wrapper-options-defines
           #:wrapper-options-intrinsics

           #:merge-wrapper-pathname

           #:generate-bindings
           #:bindings-definition
           #:bindings-required-systems
           #:bindings-required-packages
           #:unexport-bindings
           #:reexport-bindings

           #:describe-foreign-library
           #:foreign-library-entities
           #:foreign-library-language))


(uiop:define-package :%claw.wrapper.pristine
  (:use))

;madhu 230111 defining this as :%claw.wrapper.cl makes
; the "CL" package inaccessible in allegro 10.1
(uiop:define-package "%CLAW.WRAPPERCL"
  (:use #:cl))

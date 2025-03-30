(in-package "CLAW-CXX-SDL3-USER")

(defun create-properties ()
  (sdl-create-properties))

#||
/**
 * # CategoryProperties
 *
 * A property is a variable that can be created and retrieved by name at
 * runtime.
 *
 * All properties are part of a property group (SDL_PropertiesID). A property
 * group can be created with the SDL_CreateProperties function and destroyed
 * with the SDL_DestroyProperties function.
 *
 * Properties can be added to and retrieved from a property group through the
 * following functions:
 *
 * - SDL_SetPointerProperty and SDL_GetPointerProperty operate on `void*`
 *   pointer types.
 * - SDL_SetStringProperty and SDL_GetStringProperty operate on string types.
 * - SDL_SetNumberProperty and SDL_GetNumberProperty operate on signed 64-bit
 *   integer types.
 * - SDL_SetFloatProperty and SDL_GetFloatProperty operate on floating point
 *   types.
 * - SDL_SetBooleanProperty and SDL_GetBooleanProperty operate on boolean
 *   types.
 *
 * Properties can be removed from a group by using SDL_ClearProperty.
 */
||#

#||
;; [Sat Feb 22 17:38:55 2025 +0530]
(setq $gpid (sdl-get-global-properties))
(cffi::actual-type (cffi::parse-type 'sdl-properties-id))

(cffi:defcallback property-enumerator :void
    ((userdata :pointer)
     (property-id :unsigned-int)
     (name :string))
  (declare (ignore userdata))
  (format t "enumerating property: id = ~D name = ~A~%" property-id name))

(%sdl-enumerate-properties $gpid (cffi:callback property-enumerator) (cffi:null-pointer))
(%sdl-enumerate-properties 2 (cffi:callback property-enumerator) (cffi:null-pointer))
||#

(cffi:defcallback map-property-names-callback :void
    ((user-data :pointer)
     (property-id :unsigned-int)
     (name :string))
  (declare (ignore property-id))
  (let ((thunk (cffi-callback-manager:find-callback user-data)))
    (with-simple-restart (skip-execution "Skip Executing map-property-names-callback")
      (funcall thunk name))))

(defun map-property-names (property-id func)
  "call func with each property name associated with property-id"
  (cffi-callback-manager:with-registered-callback (loc) func
    (%sdl-enumerate-properties property-id (cffi:callback map-property-names-callback) loc)))

(defun get-property-names (property-id)
  (let (ret)
    (map-property-names property-id (lambda (name) (push name ret)))
    ret))

;; kludge to pass a cobj "null-ptr" default argument for
;; sdl-get-pointer-property and sdl-set-pointer-property.

(cffi:defcstruct (null-ptr :size 0))

(cobj:define-cobject-class (:struct null-ptr))

(defvar +cobj-null-pointer+
  (cobj:manage-cobject (cobj:pointer-cobject (cffi:null-pointer) 'null-ptr)))


#||
(sdl-get-property-names (sdl-get-global-properties))
(setq  $test-props (sdl-create-properties))
(%sdl-set-pointer-property $test-props "null" (cffi:make-pointer 666))
(sdl-get-pointer-property $test-props "null" +cobj-null-pointer+)
(type-of (sdl-get-pointer-property $test-props "null" +cobj-null-pointer+))
(sdl-set-pointer-property $test-props "null" (cobj:pointer-cobject (cffi:make-pointer 666) 'null-ptr))
(sdl-set-string-property $test-props "str" "barf")
(sdl-get-string-property $test-props "str" "barf1")
(sdl-get-string-property $test-props "numb" "wtf")
(sdl-set-number-property $test-props "numb" 666)
(sdl-get-number-property $test-props "numb" 666)
(sdl-set-float-property $test-props "pi" 3.14)
(sdl-get-float-property $test-props "pif" 3.0)
(sdl-get-string-property $test-props "numb" "def")
(sdl-get-boolean-property $test-props "numb" nil)
(sdl-set-boolean-property $test-props "numb" nil)
(sdl-copy-properties src dst)
(sdl-lock-properties prop-id)
||#

;; NOTE: SDL properties are like multityped perl vars. no lisp equivalent,
;; no concept of setting up common lisp accessors

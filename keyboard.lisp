(in-package "CLAW-CXX-SDL3-USER")

;; Keymod is no longer an enum, but constants.
;; The keysym field of key events has been removed to remove one level of indirection, and sym has been renamed key.


;; SDL_PRESSED and SDL_RELEASED have been removed. For the most part you can replace uses of these with true and false respectively. Events which had a field state to represent these values have had those fields changed to bool down, e.g. event.key.state is now event.key.down.

(defun key-down-p (state)
  (eql state t))

(defun key-up-p (state)
  (eql state nil ))

#||
(equal (sdl-get-scancode-name 88) "Keypad Enter")
(cffi:foreign-funcall "SDL_GetScancodeFromKey" :int 88 :pointer (cffi:null-pointer) :int)
(cffi:foreign-enum-value 'sdl-scancode (%sdl-get-scancode-from-key 88 (cffi:null-pointer)))
||#

(defun scancode-symbol (scancode)
  "Converts a scancode number to a scancode keyword."
  (cffi:foreign-enum-keyword 'sdl-scancode scancode))


#||
(eql (scancode-symbol 88) :SDL-SCANCODE-KP-ENTER)
(eql (scancode-key-to-value :sdl-scancode-kp-enter) 88)
(null (scancode-key-to-value :kp-enter))
||#

(defun scancode-key-to-value (scancode-key)
  "Converts a scancode keyword to its numerical value."
  (cffi:foreign-enum-value 'sdl-scancode scancode-key :errorp nil))

(defgeneric scancode= (value scancode-key))

(defmethod scancode= ((scancode integer) scancode-key)
  (= scancode (scancode-key-to-value scancode-key)))


(defun mod-keywords (value)
  (loop for sym in '(ALT CAPS GUI LALT LCTRL LGUI LSHIFT MODE NONE  NUM RALT RCTRL GUI SCROLL RSHIFT)
	for nam =  (concatenate 'string "+SDL-KMOD-" (string sym) "+")
	for var = (find-symbol nam "CLAW-CXX-SDL3")
	for val = (progn (assert var) (symbol-value var))
	when (not (zerop (logand val value)))
	collect (intern (string sym) :keyword)))

(defun compute-mod-value (keywords &key (error-p t))
  "Return the combined value of the mod masks denoted by KEYWORDS. If
ERROR-P is NIL, unknown mask are ignored in the computation, and
returned as the second return value."
  (loop for sym in keywords
	with ret = 0 and unknowns
	for nam = (concatenate 'string "+SDL-KMOD-" (string sym) "+")
	for var = (find-symbol nam "CLAW-CXX-SDL3")
	do (cond (var (let ((val (symbol-value var)))
			(setq ret (logior ret val))))
		 (error-p (error "Unknown MOD ~S" sym))
		 (t (pushnew sym unknowns)))
	finally (return (values ret unknowns))))


#||
(equal (mod-keywords 10) '(:RSHIFT))
(equal (mod-keywords 100) '(:LCTRL))
(= (mod-value '(:LCTRL :RSHIFT)) 66)
(mod-value-p 66 :ALT1)
||#

(defun mod-value-p (value &rest keywords)
  (let ((mask (compute-mod-value keywords :error-p nil)))
    (/= 0 (logand mask value))))

;; cffi-object gets GetKeyboardState args and return valuewrong.

#||
(setq $a (cobj:manage-cobject (cobj:pointer-cpointer (cffi:foreign-alloc :int) '(unsigned-byte 32))))
(setf (cobj:cref $a) 0)
(setq $b (sdl-get-keyboard-state $a))
(cobj:cref $a)
||#

(defun keyboard-state-p (scancode)
  "Whether the key corresponding to the given scancode is currently pressed."
  (cffi:with-foreign-object (numkeys :int)
    (let* ((ptrbool (%sdl-get-keyboard-state numkeys))
	   (len (cffi:mem-ref numkeys :int)))
      (assert (< scancode (cffi:mem-ref numkeys :int))
	  nil
	  "invalid scancode ~A (should be < ~A" scancode len)
      (cffi:mem-aref ptrbool :bool scancode))))

(defun get-key-from-scancode (scancode &key modstate key-event)
  "Get the key code corresponding to the given scancode according to
the current keyboard layout.

the modifier state to use when translating the scancode to a keycode

If you want to get the keycode as it would be delivered in key events,
including options specified in SDL_HINT_KEYCODE_OPTIONS, then you
should pass key_event as true. Otherwise this function simply
translates the scancode based on the given modifier state.
"
  (sdl-get-key-from-scancode scancode
			     (etypecase modstate
			       (integer modstate)
			       (null 0)
			       ((or keyword (cons keyword))
				(compute-mod-value (if (atom modstate)
						       (list modstate)
						       modstate)
						   :error-p t)))
			     key-event))

(defun get-key-name (key)
  (values (sdl-get-key-name key)))

(defun scancode-name (scancode)
  (get-key-name (get-key-from-scancode scancode)))

(defun scancode-key-name (scancode)
  (let ((key (get-key-from-scancode scancode)))
    (values (sdl-get-key-name key))))

;; The text input state hase been changed to be
;; window-specific. SDL_StartTextInput(), SDL_StopTextInput(),
;; SDL_TextInputActive(), and SDL_ClearComposition() all now take a
;; window parameter.

(defun start-text-input (window)
  (sdl-start-text-input window))

(defun stop-text-input (window)
  (sdl-stop-text-input window))

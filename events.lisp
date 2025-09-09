(in-package "CLAW-CXX-SDL3-USER")

;; from sdl2/src/events.lisp, adapted for cffi-object
;; :firstevent -> :sdl-event-first, etc.
;; check-rc

(defvar *user-event-types* (make-hash-table))
(defvar *user-event-codes* (make-hash-table))
(defvar *user-events* (make-hash-table))
(defvar *user-event-id* (make-sdl-atomic-int :value 0))
(defvar *event-loop* nil)

#+nil
(cobj::define-struct-cobject-class sdl-event)

(eval-when (load eval compile)
(defun new-event (&optional (event-type :sdl-event-first))
  (make-sdl-event :type (if (integerp event-type)
			    event-type
			    (cffi:foreign-enum-value 'sdl-event-type
						     event-type)))))

#+nil
(new-event)

(defun free-event (event)
  (declare (ignorable event))
  ;; presumably handled by cffi-object magic?
  )

(defun register-user-event-type (user-event-type)
  "Register a new sdl event-type if it doesn't already exist"
  (when (not (keywordp user-event-type))
    (error "Event types must be given as keywords"))
  (multiple-value-bind (event-type-code event-type-found)
      (gethash user-event-type *user-event-codes*)
    (if event-type-found
        event-type-code
        (let ((new-event-code (sdl-register-events 1)))
          (if (not (= new-event-code 0))
              (progn
                (setf (gethash user-event-type *user-event-codes*) new-event-code)
                (setf (gethash new-event-code *user-event-types*) user-event-type)
                new-event-code)
              (error (format nil "Failed to register new user-event type: ~a" user-event-type)))))))

;;XXX
(defmacro with-sdl-event ((event &optional (event-type :sdl-event-first)) &body body)
  "Allocate and automatically free an sdl event struct."
  `(let ((,event (new-event ,event-type)))
     (unwind-protect (progn ,@body)
       (free-event ,event))))

(defun add-user-data (user-data)
  (let* ((event-id (sdl-add-atomic-int *user-event-id* 1))
         (id-in-use (nth-value 1 (gethash event-id *user-events*))))
    (when id-in-use
      (error "Event ID already in use"))
    (setf (gethash event-id *user-events*) user-data)
    event-id))

(defun free-user-data (event-id)
  (remhash event-id *user-events*))

(defun get-user-data (event-id)
  "Returns the user-data attached to an event-id and if the event-id was found"
  (gethash event-id *user-events*))

(defun get-event-code (event-type)
  (multiple-value-bind (user-event-code is-user-event) (gethash event-type *user-event-codes*)
    (cond
      (is-user-event
       user-event-code)
      ((eq :lisp-message event-type)
       *lisp-message-event*)
      (t
       (cffi:foreign-enum-value 'sdl-event-type  event-type)))))

(defun get-event-type (event)
  (multiple-value-bind (user-event-type is-user-event)
      (gethash (sdl-event-type event) *user-event-types*)
    (cond
      (is-user-event
       user-event-type)
      ((eq (sdl-event-type event) *lisp-message-event*)
       :lisp-message)
      (t
       (or (cffi:foreign-enum-keyword 'sdl-event-type
				      (sdl-event-type event))

           (sdl-event-type event))))))

(defun user-event-type-p (event-type)
  (nth-value 1 (gethash event-type *user-event-codes*)))

(defun pump-events ()
  (sdl-pump-events))

(defun push-event (event)
  "Allocates a new sdl event struct of the specified type and pushes it into the queue."
  (etypecase event
    (symbol
     (with-sdl-event (ev event)
       #+nil
       (setf (sdl-event-type ev)
	     (get-event-code event))
       (check-rc (sdl-push-event ev))))
    ;; remember this will never fire for events like sdl-keyboard-event which are not subtypes of sdl-event.
    (sdl-event
     (check-rc (sdl-push-event event)))
    (sdl-user-event
     (check-rc (sdl-push-event event)))))

(defun push-user-event (user-event &optional user-data)
  "Allocates a new user-defined sdl event struct of the specified type and pushes it into the queue.
Stores the optional user-data in sdl2::*user-events*"
  (if (user-event-type-p user-event)
      (with-sdl-event (event (if (symbolp user-event) (get-event-code user-event) user-event))
        (let ((event-id (add-user-data user-data)))
	  (setf (sdl-user-event-code (sdl-event-user event))
		event-id)
          (push-event event)))
      (error "Not a known user-event type")))

(defun push-quit-event ()
  (push-event :sdl-event-quit))

(defun next-event (event &optional (method :poll) timeout)
  "Method can be either :poll, :wait. If :wait is used, `TIMEOUT` may be specified."
  (case method
    (:poll (sdl-poll-event event))
    (:wait
     (if timeout
         (sdl-wait-event-timeout event timeout)
         (sdl-wait-event event)))
    (:wait-with-timeout
     (sdl-wait-event-timeout event (or timeout 0)))
    (otherwise (error "Event method must be :poll or :wait"))))

(defun expand-idle-handler (event-handlers)
  (remove nil (mapcar #'(lambda (handler)
                          (cond ((eq (first handler) :idle)
                                 `(progn ,@(rest (rest handler))))))
                      event-handlers)))

(defun expand-quit-handler (sdl-event forms quit)
  (declare (ignore sdl-event))
  `(:sdl-event-quit (setf ,quit (funcall #'(lambda () ,@forms)))))


(defparameter *event-type-to-accessor*
  '((:sdl-event-gamepad-axis-motion . :gaxis)
    (:sdl-event-gamepad-button-down . :gbutton)
    (:sdl-event-gamepad-button-up . :gbutton)
    (:sdl-event-gamepad-added . :gdevice)
    (:sdl-event-gamepad-remapped . :gdevice)
    (:sdl-event-gamepad-removed . :gdevice)
    (:sdl-event-drop-file . :drop)
    (:sdl-event-finger-motion . :tfinger)
    (:sdl-event-finger-down . :tfinger)
    (:sdl-event-finger-up . :tfinger)
    (:sdl-event-joystick-axis-motion . :jaxis)
    (:sdl-event-joystick-ball-motion . :jball)
    (:sdl-event-joystick-button-down . :jbutton)
    (:sdl-event-joystick-button-up . :jbutton)
    (:sdl-event-joystick-added . :jdevice)
    (:sdl-event-joystick-removed . :jdevice)
    (:sdl-event-joystick-hat-motion . :jhat)
    (:sdl-event-key-down . :key)
    (:sdl-event-key-up . :key)
    (:sdl-event-mouse-button-down . :button)
    (:sdl-event-mouse-button-up . :button)
    (:sdl-event-mouse-motion . :motion)
    (:sdl-event-mouse-wheel . :wheel)
;;    (:multigesture . :mgesture)
;;    (:syswmevent . :syswm)
    (:sdl-event-text-editing . :edit)
    (:sdl-event-text-input . :text)
    (:sdl-event-user . :user)
    (:lisp-message . :user)
    (:sdl-event-window-close-requested . :window)
    (:sdl-event-window-display-changed . :window)
    (:sdl-event-window-enter . :window)
    (:sdl-event-window-exposed . :window)
    (:sdl-event-window-focus-gained . :window)
    (:sdl-event-window-focus-lost . :window)
    (:sdl-event-window-hidden . :window)
    (:sdl-event-window-hit-test . :window)
    (:sdl-event-window-iccprof-changed . :window)
    (:sdl-event-window-mouse-leave . :window)
    (:sdl-event-window-maximized . :window)
    (:sdl-event-window-minimized . :window)
    (:sdl-event-window-resized . :window)
    (:sdl-event-window-restored . :window)
    (:sdl-event-window-shown . :window)
    (:sdl-event-window-moved . :window)
    (:sdl-event-window-high-pixel-density . :window)
    (:sdl-event-window-pixel-size-changed . :window)
    (:sdl-event-allow-input-grabbed . :window)
    (:sdl-event-quit . :quit)))

#||
(cffi:foreign-enum-value 'sdl-event-type :sdl-event-mouse-wheel)
(cdr (assoc :sdl-event-mouse-wheel *event-type-to-accessor*))
(apropos (event-type-to-event-accessor  :sdl-event-mouse-wheel))
(event-type-to-event-accessor :sdl-event-window-shown)
||#


;; this returns the accessor to retrieve the specic event-type from
;; sdl-event
(defun event-type-to-event-accessor (event-type &key user-ok)
  "If USER-OK non-NIL and EVENT-TYPE is not found, assume EVENT-TYPE is a
:sdl-user-event."
  (check-type event-type keyword)
  (let ((ref (or (cdr (assoc event-type *event-type-to-accessor*))
		 (and user-ok :user))))
    (assert ref nil "Unknown event-type ~A in *event-type-to-accessor*" event-type)
    (let* ((name (format nil "SDL-EVENT-~A" (string ref)))
	   (sym (find-symbol name "CLAW-CXX-SDL3")))
      (assert (fboundp sym))
      #+nil
      (when (fboundp sym)
	(fdefinition sym))
      (values sym ref))))

#+nil
(eql (event-type-to-event-accessor :sdl-event-key-down) 'SDL-EVENT-KEY)

#+nil
(eql (event-type-to-event-accessor :sdl-event-barf :user-ok t) 'SDL-EVENT-USER)

#||
(defvar $c (cffi::ensure-parsed-base-type 'claw-cxx-sdl3::sdl-event))
(sort
 (mapcar (lambda (n)
	   (cons (intern (symbol-name n) :keyword)
		 (cffi::foreign-slot-type 'claw-cxx-sdl3::sdl-event n)))
	 (cffi:foreign-slot-names $c))
 (lambda (a b) (string< (car a) (car b))))
(eql #1='sdl-mouse-button-event (find-symbol (symbol-name #1#) :claw-cxx-sdl3))
||#

(defvar *event-type-to-class*
  '((:adevice . sdl-audio-device-event) (:button . sdl-mouse-button-event)
    (:cdevice . sdl-camera-device-event) (:clipboard . sdl-clipboard-event)
    (:common . sdl-common-event) (:display . sdl-display-event)
    (:drop . sdl-drop-event) (:edit . sdl-text-editing-event)
    (:edit-candidates . sdl-text-editing-candidates-event)
    (:gaxis . sdl-gamepad-axis-event) (:gbutton . sdl-gamepad-button-event)
    (:gdevice . sdl-gamepad-device-event) (:gsensor . sdl-gamepad-sensor-event)
    (:gtouchpad . sdl-gamepad-touchpad-event) (:jaxis . sdl-joy-axis-event)
    (:jball . sdl-joy-ball-event) (:jbattery . sdl-joy-battery-event)
    (:jbutton . sdl-joy-button-event) (:jdevice . sdl-joy-device-event)
    (:jhat . sdl-joy-hat-event) (:kdevice . sdl-keyboard-device-event)
    (:key . sdl-keyboard-event) (:mdevice . sdl-mouse-device-event)
    (:motion . sdl-mouse-motion-event) (:padding . uint8)
    (:paxis . sdl-pen-axis-event) (:pbutton . sdl-pen-button-event)
    (:pmotion . sdl-pen-motion-event) (:pproximity . sdl-pen-proximity-event)
    (:ptouch . sdl-pen-touch-event) (:quit . sdl-quit-event)
    (:render . sdl-render-event) (:sensor . sdl-sensor-event)
    (:text . sdl-text-input-event) (:tfinger . sdl-touch-finger-event)
    (:type . uint32) (:user . sdl-user-event) (:wheel . sdl-mouse-wheel-event)
    (:window . sdl-window-event)))

(defun event-type-to-class-name-and-slot-names (event-type)
  (let ((indicator (cdr (assoc event-type *event-type-to-accessor*))))
    (when indicator
      (let ((class (cdr (assoc indicator *event-type-to-class*))))
	(when class
	  (values class
		  (cffi:foreign-slot-names
		   (cffi::ensure-parsed-base-type class))
		  #+nil
		  (mapcar (lambda (slot) (slot-value slot 'cffi::name))
			  (cffi::slots-in-order (cffi::parse-type class)))))))))

#+nil
(event-type-to-class-name-and-slot-names :sdl-event-mouse-wheel)

(defun event-type-to-slot-accessor (event-type &optional slots)
  (check-type event-type keyword)
  (let ((elt (assoc event-type *event-type-to-accessor*)))
    (unless elt (error "no such event: ~A." event-type))
    (unless slots
      (setq slots (nth-value 1 (event-type-to-class-name-and-slot-names event-type))))
    (when elt
      (loop for slot in (alexandria:ensure-list slots)
	    collect
	    (let* ((indicator (cdr elt))
		   (cobj-event-type-name
		    (cdr (assoc indicator *event-type-to-class*)))
		   (slot-accessor-name
		    (find-symbol (concatenate 'string
					      (symbol-name cobj-event-type-name)
					      "-"
					      (symbol-name slot))
				 "CLAW-CXX-SDL3")))
	      (unless (and slot-accessor-name
			   (fboundp slot-accessor-name))
		(let* ((cffi-type-name cobj-event-type-name)
		       (cffi-type
			(cffi::ensure-parsed-base-type cffi-type-name))
		       (slots (and cffi-type
				   (cffi:foreign-slot-names cffi-type))))
		(error "no slot named ~A in ~A: ~:[no such type~;available slots: ~:*~A~]."
		       slot cobj-event-type-name slots)))
	      slot-accessor-name)))))


#+nil
(event-type-to-slot-accessor :sdl-event-key-down '(scancode :key :mod))
;; => (SDL-KEYBOARD-EVENT-SCANCODE SDL-KEYBOARD-EVENT-KEY SDL-KEYBOARD-EVENT-MOD)


(defun unpack-event-params (event-var event-type params)
  (mapcar (lambda (param)
            (let ((keyword (first param))
		  (binding (second param)))
	      (multiple-value-bind (acc ref)
		   (event-type-to-event-accessor event-type :user-ok (eql keyword :user-data))
		;; ;madhu 250525 what if :user-data is not supplied?
		(if (eql keyword :user-data)
		    `(,binding (get-user-data (sdl-user-event-code ,event-var)))
		    (let ((slot-acc (event-type-to-slot-accessor event-type keyword)))
		      (if (and (or (eql ref :text) (eql ref :edit)) (eql keyword :text))
			  `(,binding (cffi:foreign-string-to-lisp
				      (,(car slot-acc) (,acc ,event-var))))
			  `(,binding (,(car slot-acc) (,acc ,event-var)))))))))
	  params))

#+nil
(unpack-event-params 'ev :sdl-event-key-down '((:scancode key)))
;; => ((KEY (SDL-KEYBOARD-EVENT-SCANCODE (SDL-EVENT-KEY EV))))

(defvar +all-sdl-event-types+  (cffi:foreign-enum-keyword-list 'sdl-event-type))

(defun expand-event-type (event-type)
  (if (user-event-type-p event-type)
      (return-from expand-event-type event-type))
  ;; could use handle-short-enum-keyword.
  (if (find event-type +all-sdl-event-types+)
      event-type
      (let ((sym (find-symbol (concatenate 'string "SDL-EVENT-" (string event-type)) :keyword)))
	(if (and sym (find sym +all-sdl-event-types+))
	    sym
	    (error "Unknown sdl-event-type ~A. see +all-sdl-event-types+ for long form of required keyword." event-type)))))

#+nil
(expand-event-type :key-up)

(defun expand-handler (sdl-event event-type params forms)
  (let ((parameter-pairs nil)
	(event-type (if (member event-type '(:quit :idle))
			event-type
			(expand-event-type event-type))))
    (do ((keyword params (if (cdr keyword)
                             (cddr keyword)
                             nil)))
        ((null keyword))
      (push (list (first keyword) (second keyword)) parameter-pairs))
    `(,event-type
      (let (,@(unpack-event-params sdl-event
                                   event-type
                                   (nreverse parameter-pairs)))
        ,@forms))))

#+nil
(expand-handler 'ev :sdl-event-key-down '(:scancode sym) '(1 2 3))
;; => (:SDL-EVENT-KEY-DOWN (LET ((SYM (SDL-KEYBOARD-EVENT-SCANCODE (SDL-EVENT-KEY EV)))) 1 2 3))

;;; alternative
(defun %expand-handler (event-var event-type params forms)
  (setq event-type (expand-event-type event-type))
  `(,event-type
     (let (,@(loop for (keyword binding) in
		   (loop for keyword = params then (if (cdr keyword) (cddr keyword))
			 until (null keyword)
			 collect (list (first keyword) (second keyword)))
		   for ref = (or (cdr (assoc event-type *event-type-to-accessor*))
				 :user)
		   for acc = (event-type-to-event-accessor event-type)
		   for slot-acc = (event-type-to-slot-accessor event-type keyword)
		   collect `(,binding (,(car slot-acc) (,acc ,event-var)))))
       ,@forms)))

#+nil
(%expand-handler 'sdl-event :quit '(:timestamp ts) '((+ 2 3)))

;; TODO you should be able to specify a target framerate
(defmacro with-event-loop ((&key background (method :poll) (timeout nil) recursive
				 cow-catcher)
                           &body event-handlers)
  (let ((quit (gensym "QUIT-"))
        (sdl-event (gensym "SDL-EVENT-"))
        (sdl-event-type (gensym "SDL-EVENT-TYPE"))
        (sdl-event-id (gensym "SDL-EVENT-ID"))
        (idle-func (gensym "IDLE-FUNC-"))
        (rc (gensym "RC-")))
    `(when (or ,recursive (not *event-loop*))
       (setf *event-loop* t)
       (in-main-thread (:background ,background)
         (let ((,quit nil)
               (,idle-func nil))
           (unwind-protect
                (with-sdl-event (,sdl-event)
                  (setf ,idle-func #'(lambda () ,@(expand-idle-handler event-handlers)))
                  (progn ,@(cddr (find :initialize event-handlers :key #'first)))
                  (loop :until ,quit
                        :do (loop :as ,rc = (next-event ,sdl-event ,method ,timeout)
                                  ,@(if (eq :poll method)
                                        `(:until (null ,rc))
                                        `(:until ,quit))
                                  :do (let* ((,sdl-event-type (get-event-type ,sdl-event))
                                             (,sdl-event-id (and (user-event-type-p ,sdl-event-type)
                                                                 (sdl-user-event-code ,sdl-event))))
					,@(and cow-catcher
					       `((and ,cow-catcher
						      (funcall ,cow-catcher ,sdl-event))))
                                        (case ,sdl-event-type
                                          (:lisp-message () (get-and-handle-messages))
                                          ,@(loop :for (type params . forms) :in event-handlers
                                                  :collect
                                                  (if (eq type :quit)
                                                      (expand-quit-handler sdl-event forms quit)
                                                      (expand-handler sdl-event type params forms))
                                                    :into results
                                                  :finally (return (remove nil results))))
                                        (when (and ,sdl-event-id
                                                   (not (eq ,sdl-event-type :lisp-message)))
                                          (free-user-data ,sdl-event-id))))
                            (unless ,quit
                              (funcall ,idle-func))))
             (setf *event-loop* nil)))))))

#+nil
(with-init (:video)
  (with-window (win :flags '(:opengl))
    (with-event-loop (:method :poll)
      (:sdl-event-key-down (:scancode scancode)
       (format t "HANDLING KEY DOWN EVENT~%")
       (when (eql scancode :sdl-scancode-escape)
	 (format t "QUITTING~%")
         (push-event :sdl-event-quit)))
      (:idle () (delay 300))
      (:quit () t))))

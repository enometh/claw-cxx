;madhu 250605 converted from corresponding example of cl-sdl2
(in-package #:cl-claw-sdl3-examples)


(defun basic-test ()
  "The kitchen sink."
  (sdl3:with-init (:everything)
    (multiple-value-bind (major minor patchlevel)
	(version)
      (format t "Using SDL Library Version: ~D.~D.~D~%"
	      major minor patchlevel))
    (finish-output)

    (sdl3:with-window (win :title "Basic" :flags '(:opengl))
      (sdl3:with-gl-context (gl-context win)
        (let ((controllers ())
              (haptic ()))

          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl3:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho -2 2 -2 2 -2 2)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 1.0 1.0)
          (gl:clear :color-buffer)

          (format t "Opening game controllers.~%")
          (finish-output)
          ;; open any game controllers
	  #+nil
          (loop :for i :upto (- (sdl3:joystick-count) 1)
                :do (when (sdl2:game-controller-p i)
                      (format t "Found gamecontroller: ~a~%"
                              (sdl2:game-controller-name-for-index i))
                      (let* ((gc (sdl2:game-controller-open i))
                             (joy (sdl2:game-controller-get-joystick gc)))
                        (setf controllers (acons i gc controllers))
                        (when (sdl2:joystick-is-haptic-p joy)
                          (let ((h (sdl2:haptic-open-from-joystick joy)))
                            (setf haptic (acons i h haptic))
                            (sdl2:rumble-init h))))))

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl3:with-event-loop (:method :poll)
            (:key-down (:scancode scancode :key key :mod mod-value)
	     (cond
	       ((eql scancode :sdl-scancode-w) (format t "~a~%" "WALK"))
	       ((eql scancode :sdl-scancode-s) (sdl3:show-cursor))
	       ((eql scancode :sdl-scancode-h) (sdl3:hide-cursor)))
	     (format t "Key sym: ~a, code: ~a, mod: ~a~%"
		     key
		     scancode
		     mod-value))
	    ;; can use short form without "sdl-event-".
            (:sdl-event-key-up (:scancode scancode)
                    (when (eql scancode :sdl-scancode-escape)
                      (sdl3:push-event :sdl-event-quit)))

            (:sdl-event-mouse-motion
	     (:x x :y y :xrel xrel :yrel yrel :state state)
	     (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
		     x xrel y yrel state))

	    #+nil
            (:controlleraxismotion
             (:which controller-id :axis axis-id :value value)
             (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                     controller-id axis-id value))

	    #+nil
            (:controllerbuttondown (:which controller-id)
                                   (let ((h (cdr (assoc controller-id haptic))))
                                     (when h
                                       (sdl2:rumble-play h 1.0 100))))

            (:idle ()
                   (gl:clear :color-buffer)
                   (gl:begin :triangles)
                   (gl:color 1.0 0.0 0.0)
                   (gl:vertex 0.0 1.0)
                   (gl:vertex -1.0 -1.0)
                   (gl:vertex 1.0 -1.0)
                   (gl:end)
                   (gl:flush)
                   (sdl3:gl-swap-window win))

            (:quit () t))

          (format t "Closing opened game controllers.~%")
          (finish-output)
          ;; close any game controllers that were opened as well as any haptics
	  #+nil
          (loop :for (i . controller) :in controllers
                :do (sdl2:game-controller-close controller)
                    (sdl2:haptic-close (cdr (assoc i haptic)))))))))

#+nil
(basic-test)
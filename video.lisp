(in-package "CLAW-CXX-SDL3-USER")

(defun get-num-video-drivers ()
  (sdl-get-num-video-drivers))

(defun get-video-driver (driver-index)
  (sdl-get-video-driver driver-index))

(defun get-current-video-driver ()
  (sdl-get-current-video-driver))

#||
(setq $int-storage (cffi:foreign-alloc :int))
(type-of (setq $cptr (cobj:pointer-cpointer $int-storage
					    '(signed-byte 32))))
(setq $int (cobj:manage-cobject $cptr))
(if (cffi:null-pointer-p
     (cobj:cobject-pointer (setq $displays (sdl-get-displays $int))))
    (sdl-get-error))
(cobj::cobject-type $int)
(cobj:cref $int)
(get-num-video-displays)
(setq $ret-array (sdl-get-fullscreen-display-modes 1 $int))
(cobj:cref $int)
(type-of $ret-array)
(cobj:cref $ret-array 1)
(defun claw-cxx-sdl3::sdl-get-fullscreen-display-modes-1
    (claw-cxx-sdl3:display-id claw-cxx-sdl3::count)
  ((lambda (result7278)
     (cffi-object:pointer-cpointer result7278
                                   '(:pointer
                                     claw-cxx-sdl3:sdl-display-mode)))
   (claw-cxx-sdl3:%sdl-get-fullscreen-display-modes claw-cxx-sdl3:display-id
                                                    (cffi-object:cobject-pointer
                                                     claw-cxx-sdl3::count))))
(setq $ret-array (claw-cxx-sdl3::sdl-get-fullscreen-display-modes-1 1 $int))
(cobj:cref $int)
(cobj:cref $ret-array)
||#

(defun get-num-video-displays ()
  (warn "DL_GetNumVideoDisplays() - replaced with SDL_GetDisplays()")
  (cffi:with-foreign-object (ret :int)
    (let ((display-ids (claw-cxx-sdl3::%sdl-get-displays ret)))
      (cond ((cffi:null-pointer-p display-ids)
	     (error "~A" (sdl-get-error)))
	    (t (let ((ret (cffi:mem-ref ret :int)))
		 (%sdl-free display-ids)
		 ret))))))

(defun get-display-name (display-index)
  (sdl-get-display-name display-index))

(defun get-num-display-modes (display-index)
  (warn "SDL_GetNumDisplayModes() - replaced with SDL_GetFullscreenDisplayModes()")
  (cffi:with-foreign-object (xcount :int)
    (let ((display-modes
	   (%sdl-get-fullscreen-display-modes display-index xcount)))
      (cond ((cffi:null-pointer-p display-modes)
	     (error "~A" (sdl-get-error)))
	    (t (let ((ret (cffi:mem-ref xcount :int)))
		 (%sdl-free display-modes)
		 ret))))))

(defun get-current-display-mode (display-index)
  (let ((display-mode (sdl-get-current-display-mode display-index)))
    (values (sdl-display-mode-format display-mode)
	    (sdl-display-mode-w display-mode)
	    (sdl-display-mode-h display-mode)
	    (sdl-display-mode-refresh-rate display-mode))))

#||
(setq $ret (init* :video))
(get-current-video-driver)
(get-num-video-displays)
(get-display-name 0)
(get-num-display-modes 1)
(get-current-display-mode 1)
||#

(defun get-display-mode (display-index mode-index)
  (warn "SDL_GetDisplayMode() has been removed")
  (cffi:with-foreign-object (xcount :int)
    (let ((display-modes
	   (%sdl-get-fullscreen-display-modes display-index xcount)))
      (cond ((null display-modes)
	     (error "~A" (sdl-get-error)))
	    (t (assert (< mode-index (cffi:mem-ref xcount :int)))
	       (let* ((p (cffi:mem-aref display-modes :pointer mode-index))
		      (o (cobj:manage-cobject (cobj:pointer-cpointer p 'sdl-display-mode))))
		 (let ((display-mode o))
		   (values (sdl-display-mode-format display-mode)
			   (sdl-display-mode-w display-mode)
			   (sdl-display-mode-h display-mode)
			   (sdl-display-mode-refresh-rate display-mode)))))))))

#+nil
(get-display-mode 1 78)

(defun get-display-bounds (display-index)
  "Use this function to get the desktop area represented by a display, with the primary display
located at 0,0."
  (let ((rect (make-sdl-rect)))
    (check-rc (sdl-get-display-bounds display-index rect))
    rect))

#||
(setq $rect (make-sdl-rect))
(setq $ret (sdl-get-display-bounds 1 $rect))
(check-rc $rect)
$rect
(get-display-bounds 1)
||#

#+TODO
(autowrap:define-bitmask-from-enum
    (sdl-window-flags sdl2-ffi:sdl-window-flags)
  '(:centered . #x0))

#+TODO
(autowrap:define-bitmask-from-enum
    (sdl-gl-attr sdl2-ffi:sdl-glattr))

(defun windowpos-undefined (&optional (display 0))
  (logior +sdl-windowpos-undefined-mask+ display))

(defun windowpos-centered (&optional (display 0))
  (logior +sdl-windowpos-centered-mask+ display))

(defun windowpos-from-coord (n)
  (case n
    (:undefined (windowpos-undefined))
    (:centered (windowpos-centered))
    (t n)))

(defun handle-sdl-window-flags (window-flags)
  (etypecase window-flags
    (integer window-flags)
    (symbol
     (let* ((name (format nil "+~:@(SDL-WINDOW-~A~)+" window-flags))
	    (sym (find-symbol name :claw-cxx-sdl3)))
       (assert sym nil "~A: ~A not found" window-flags name)
       (symbol-value sym)))
    (cons (cond ((endp (cdr window-flags))
		 (assert (symbolp (car window-flags)))
		 (handle-sdl-window-flags (car window-flags)))
		(t (assert (symbolp (car window-flags)))
		   (+ (handle-sdl-window-flags (car window-flags))
		      (handle-sdl-window-flags (cdr window-flags))))))))

#+nil
(handle-sdl-window-flags :opengl)

(defun create-window (&key (title "SDL3 Window") (x :centered) (y :centered) (w 800) (h 600) flags)
  (let ((window-flags (handle-sdl-window-flags flags))
        (x (windowpos-from-coord x))
        (y (windowpos-from-coord y)))
    (check-nullptr (sdl-create-window title w h window-flags))))

(defun destroy-window (win)
  (sdl-destroy-window win))

(defmacro with-window ((win
                        &key (title "SDL3 Window") (x :centered) (y :centered) (w 800) (h 600) flags)
                       &body body)
  `(let ((,win (create-window :title ,title :x ,x :y ,y :w ,w :h ,h :flags ,flags)))
     (unwind-protect (progn ,@body)
       (destroy-window ,win))))

(defmacro with-everything ((&key window gl) &body body)
  (assert (and window gl))
  (let ((window (if (symbolp window) (list window) window)))
    (destructuring-bind (win &key (title "SDL3 Window") (x :centered) (y :centered) (w 800) (h 600)
                               (flags ''(::opengl)) (fullscreen nil))
        window
      `(with-init (:everything)
         (with-window (,win :title ,title :x ,x :y ,y :w ,w :h ,h :flags ,flags)
           (with-gl-context (,gl ,win)
             (set-window-fullscreen ,win ,fullscreen)
             ,@body))))))

(defun hide-window (win)
  (sdl-hide-window win))

(defun show-window (win)
  (sdl-show-window win))

(defun maximize-window (win)
  (sdl-maximize-window win))

(defun minimize-window (win)
  (sdl-minimize-window win))

(defun raise-window (win)
  (sdl-raise-window win))

(defun restore-window (win)
  (sdl-restore-window win))

(defun update-window (win)
  (check-rc (sdl-update-window-surface win)))

(defun set-window-title (win title)
  (sdl-set-window-title win title))

(defun set-window-fullscreen (win fullscreen-value)
  "`FULLSCREEN-VALUE` of `t` or `:fullscreen` is \"regular\" fullscreen, `SDL_WINDOW_FULLSCREEN`.
Specifying `:windowed` or `:desktop` is \"windowed\" fullscreen, using
`SDL_WINDOW_FULLSCREEN_DESKTOP`."
  (let ((flag (case fullscreen-value
                ((nil))
                ((:desktop :windowed) :fullscreen-desktop)
                ((t :fullscreen) :fullscreen))))
    (check-rc (sdl-set-window-fullscreen
               win (ecase flag
		     ((:fullscreen :fullscreen-desktop)
		      +sdl-window-fullscreen+)
		     ((nil) 0))))))

(defun set-window-size (win w h)
  (sdl-set-window-size win w h))

(defun set-window-position (win x y)
  (sdl-set-window-position win
                           (windowpos-from-coord x)
                           (windowpos-from-coord y)))

(defun get-window-title (win)
  (sdl-get-window-title win))

(defun get-window-position (win)
  (cffi:with-foreign-objects ((xpos :int)
			      (ypos :int))
    (%sdl-get-window-position (cobj:cobject-pointer win) xpos ypos)
    (values (cffi:mem-ref xpos :int) (cffi:mem-ref ypos :int))))

(defun get-window-size (win)
  (cffi:with-foreign-objects ((width :int)
			      (height :int))
    (%sdl-get-window-size (cobj:cobject-pointer win) width height)
    (values (cffi:mem-ref width :int) (cffi:mem-ref height :int))))

(defun get-window-aspect-ratio (win)
  (multiple-value-call #'/ (get-window-size win)))

(defun get-window-surface (win)
  ;; Do NOT free the returned surface.
  (check-nullptr (sdl-get-window-surface win)))


(defun window-flags-to-keyword (flags)
  (loop for sym in '(FULLSCREEN OPENGL OCCLUDED HIDDEN BORDERLESS RESIZABLE MINIMIZED MAXIMIZED MOUSE-GRABBED INPUT-FOCUS MOUSE-FOCUS EXTERNAL MODAL HIGH-PIXEL-DENSITY MOUSE-CAPTURE MOUSE-RELATIVE-MODE ALWAYS-ON-TOP UTILITY TOOLTIP POPUP-MENU KEYBOARD-GRABBED VULKAN METAL TRANSPARENT NOT-FOCUSABLE)
	for nam =  (concatenate 'string "+SDL-WINDOW-" (string sym) "+")
	for var = (find-symbol nam "CLAW-CXX-SDL3")
	for val = (symbol-value var)
	when (not (zerop (logand val flags)))
	collect (intern (string sym) :keyword)))

#+nil
(window-flags-to-keyword (handle-sdl-window-flags '(:fullscreen :opengl)))

(defun get-window-flags (win)
  (window-flags-to-keyword (sdl-get-window-flags win)))

(defun get-window-pixel-format (win)
  "Use this function to get the pixel format associated with the window."
  (cffi:foreign-enum-keyword 'sdl-pixel-format
			     (sdl-get-window-pixel-format win)))

(declaim (inline get-window-id))
(defun get-window-id (win)
  (sdl-get-window-id win))

(defun get-window-display-index (window)
  (warn "renamed SDL_GetWindowDisplayIndex() => SDL_GetDisplayForWindow()")
  (sdl-get-display-for-window window))


(defun enable-screensaver ()
  (sdl-enable-screen-saver))

(defun disable-screensaver ()
  (sdl-disable-screen-saver))

(defun screensaver-enabled-p ()
  (sdl-screen-saver-enabled))

(defun gl-create-context (win)
  (check-nullptr (sdl-gl-create-context win)))

(defun gl-delete-context (gl-context)
  (warn "SDL_GL_DeleteContext() has been renamed to SDL_GL_DestroyContext to match SDL naming conventions (and glX/EGL!).")
  (sdl-gl-destroy-context  gl-context)
  (values))

(defmacro with-gl-context ((gl-context-sym win) &body body)
  `(let ((,gl-context-sym (gl-create-context ,win)))
     (unwind-protect
          (progn ,@body)
       (gl-delete-context ,gl-context-sym))))

(defun gl-extension-supported-p (extension)
  (sdl-gl-extension-supported extension))

(defun gl-make-current (win gl-context)
  (check-rc (sdl-gl-make-current win gl-context)))

(defun gl-get-swap-interval ()
  (cffi:with-foreign-object (interval :int)
    (check-rc (%sdl-gl-get-swap-interval interval))
    (cffi:mem-ref interval :int)))

(defun gl-set-swap-interval (interval)
  "0 for immediate updates, 1 for updates synchronized with the vertical retrace"
  (check-rc (sdl-gl-set-swap-interval interval)))

(defun gl-swap-window (win)
  (sdl-gl-swap-window win))

(defun gl-get-attr (attr)
  (cffi:with-foreign-object (value :int)
    (check-rc (%sdl-gl-get-attribute
	       (cffi:foreign-enum-value 'sdl-gl-attr attr)
	       value))
    (cffi:mem-ref value :int)))

(defun gl-get-attrs (&rest attrs)
  (mapcan #'list attrs (mapcar #'gl-get-attr attrs)))

(defun gl-set-attr (attr value)
  (check-rc (sdl-gl-set-attribute
	     (cffi:foreign-enum-value  'sdl-gl-attr attr)
	     value)))

(defun gl-set-attrs (&rest attr-plist)
  (loop :for (attr value) :on attr-plist :by #'cddr
        :do (gl-set-attr attr value)))

(defun gl-get-proc-address (proc-name)
  (sdl-gl-get-proc-address proc-name))

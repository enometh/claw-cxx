(in-package "CLAW-CXX-SDL3-USER")

(defmacro warp-mouse-in-window (win x y)
  "Use this function to move the mouse to the given position within the window."
  `(sdl-warp-mouse-in-window ,win ,x ,y))

;; SDL_ShowCursor() has been split into three functions: SDL_ShowCursor(), SDL_HideCursor(), and SDL_CursorVisible()
(defun hide-cursor ()
  "Set the cursor to invisible in all SDL2 windows."
  (sdl-hide-cursor))

(defun show-cursor ()
  "Set the cursor to visible in all SDL2 windows."
  (sdl-show-cursor))

(defun show-cursor-p ()
  "Returns whether the cursor is currently visible."
  (sdl-cursor-visible))

;; The following functions have been removed: SDL_SetRelativeMouseMode() - replaced with SDL_SetWindowRelativeMouseMode(), SDL_GetRelativeMouseMode() - replaced with SDL_GetWindowRelativeMouseMode()
#||
(defun set-relative-mouse-mode (enabled)
  (let ((enabled (ecase enabled ((1 t) 1) ((0 nil) 0))))
    (check-rc (sdl-set-relative-mouse-mode enabled))))

(defun relative-mouse-mode-p ()
  (sdl-true-p (sdl-get-relative-mouse-mode)))

(defun toggle-relative-mouse-mode ()
  (set-relative-mouse-mode (relative-mouse-mode-p)))
||#

;; SDL_GetMouseState(), SDL_GetGlobalMouseState(), SDL_GetRelativeMouseState(), SDL_WarpMouseInWindow(), and SDL_WarpMouseGlobal() all use floating point mouse positions, to provide sub-pixel precision on platforms that support it.
(defun mouse-state ()
  "Returns (VALUES X Y BITMASK) where X, Y give the mouse cursor position relative to the focused
window and BITMASK has bit i from the right set if and only if mouse button i is pressed."
  (cffi:with-foreign-objects ((x :float) (y :float))
    (let ((buttons (%sdl-get-mouse-state x y)))
      (values (cffi:mem-ref x :float) (cffi:mem-ref y :float) buttons))))

(defun mouse-state-p (button)
  "Whether the mouse button numbered BUTTON is pressed inside the focused window. 1 indicates the
left mouse button, 2 the middle mouse button and 3 the right mouse button."
  (let ((x (cffi:null-pointer)) (y (cffi:null-pointer)))
    (let ((buttons (%sdl-get-mouse-state x y))
	  (mask (ash 1 (1- button))))
    (plusp (logand buttons mask)))))

(defun get-global-mouse-state ()
  "Returns (X Y BITMASK) where X and Y are positions of the mouse cursor relative to the desktop
and BITMASK has bit i from the right set if and only if mouse button i is pressed."
  (cffi:with-foreign-objects ((x :float) (y :float))
    (let ((buttons (%sdl-get-global-mouse-state x y)))
      (values (cffi:mem-ref  x y buttons)))))

(defun global-mouse-state-p (button)
  "Whether the mouse button numbered BUTTON is pressed. 1 indicates the left mouse button,
2 the middle mouse button and 3 the right mouse button. This function works relative to desktop
and can be used even when there is no SDL window open."
  ;; as long as we don't dereference the null pointer, this shows we
  ;; can use the cobj method sdl-get-global-mouse-state instead of
  ;; the cffi method %sdl-get-global-mouse-state
  (let ((x (cffi:null-pointer)) (y (cffi:null-pointer)))
    (let ((x1 (cobj:pointer-cpointer x 'single-float))
	  (y1 (cobj:pointer-cpointer y 'single-float)))
  (let ((buttons (sdl-get-global-mouse-state x1 y1))
	(mask (ash 1 (1- button))))
    (plusp (logand buttons mask))))))

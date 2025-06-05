(in-package "CLAW-CXX-SDL3-USER")

;; TODO: wrap get property functions with optional default and error checking

;; SDL_GetRendererInfo() has been removed. The name of a renderer can be retrieved using SDL_GetRendererName(), and the other information is available as properties on the renderer.

;; The following symbols have been removed:

;; SDL_RENDERER_ACCELERATED - all renderers except SDL_SOFTWARE_RENDERER are accelerated
;; SDL_RENDERER_PRESENTVSYNC - replaced with SDL_PROP_RENDERER_CREATE_PRESENT_VSYNC_NUMBER during renderer creation and SDL_PROP_RENDERER_VSYNC_NUMBER after renderer creation
;; SDL_RENDERER_SOFTWARE - you can check whether the name of the renderer is SDL_SOFTWARE_RENDERER
;; SDL_RENDERER_TARGETTEXTURE - all renderers support target texture functionality
;; SDL_ScaleModeBest = use SDL_SCALEMODE_LINEAR instead
;; SDL

;; enum SDL_TextureModulate is gone, without mention
;; enum SDL_RendererFlip => SDL_FlipMode - moved to SDL_surface.h


(defun get-num-render-drivers ()
  "Return the number of 2D rendering drivers available for the current display."
  (sdl-get-num-render-drivers))

(defun get-render-driver-name (index)
  (sdl-get-render-driver index))

#||
(loop for i below (get-num-render-drivers) collect (get-render-driver-name i))
;; => ("gpu" "opengl" "opengles2" "vulkan" "software")
||#

;; SDL_CreateWindowAndRenderer() now takes the window title as the first parameter.

(defun create-window-and-renderer (width height flags &key (title "CLAW-CXX-SDL3"))
  (let ((window (cffi:foreign-alloc :pointer))
	(renderer (cffi:foreign-alloc :pointer)))
    (check-rc
     (%sdl-create-window-and-renderer title width height (handle-sdl-window-flags flags) window renderer))
    (values (cobj:manage-cobject (cobj:pointer-cpointer (cffi:mem-ref window :pointer) 'sdl-window))
	    (cobj:manage-cobject (cobj:pointer-cpointer (cffi:mem-ref renderer :pointer) 'sdl-renderer)))))

;; SDL_CreateRenderer()'s second argument is no longer an integer index, but a const char * representing a renderer's name; if you were just using a for-loop to find which index is the "opengl" or whatnot driver, you can just pass that string directly here, now. Passing NULL is the same as passing -1 here in SDL2, to signify you want SDL to decide for you.

;; SDL_CreateRenderer()'s flags parameter has been removed. See specific flags below for how to achieve the same functionality in SDL 3.0.

(defun create-renderer (window name)
  "Create a 2D rendering context for a window."
  (check-nullptr (sdl-create-renderer window name)))

(defun create-software-renderer (surface)
  "Create and return a 2D software rendering context for the surface."
  (check-nullptr (sdl-create-software-renderer surface)))

(defun destroy-renderer (r)
  (sdl-destroy-renderer r))

(defmacro with-renderer ((renderer-sym window &key name) &body body)
  `(let ((,renderer-sym (create-renderer ,window ,name)))
     (unwind-protect
          (progn ,@body)
       (destroy-renderer ,renderer-sym))))

(defun get-renderer (window)
  "Return NIL if there is no renderer associated with the window, or otherwise the SDL_Renderer
structure."
  (let ((renderer (sdl-get-renderer window)))
    (if (cffi:null-pointer-p (cobj:cobject-pointer renderer))
        nil
        renderer)))

#+nil
(cobj:cobject-pointer (cobj:pointer-cobject (cffi:null-pointer) 'sdl-f-rect))

#+nil
(defun ensure-nullable (x x-type)
  (cond ((typep x x-type) x)
	((null x)
	 (cobj:manage-cobject
	  (cobj:pointer-cobject (cffi:null-pointer) x-type)))
	(t (error "tahp error"))))

#||
(cffi-object::define-struct-cobject-class sdl-f-rect)
(eql 'sdl-rect 'claw-cxx-sdl3::sdl-rect)
(eql 'sdl-f-rect 'claw-cxx-sdl3::sdl-f-rect)
(setq $x (make-sdl-f-rect :x (float 0) :y 0.0 :w 0.0 :h 0.0))
||#

(defun ensure-frect (rect)
  (etypecase rect
    (sdl-f-rect rect)
    (sdl-rect (make-sdl-f-rect
	       :x (float (sdl-f-rect-x rect))
	       :y (float (sdl-f-rect-y rect))
	       :w (float (sdl-f-rect-w rect))
	       :h (float (sdl-f-rect-h rect))))
    (null (cobj:manage-cobject
	   (cobj:pointer-cobject (cffi:null-pointer) 'sdl-f-rect)))))

;; SDL_GetRendererOutputSize() => SDL_GetCurrentRenderOutputSize(), returns bool
;; SDL_RenderCopy() => SDL_RenderTexture(), returns bool
;; SDL_RenderCopyEx() => SDL_RenderTextureRotated(), returns bool
;; SDL_RenderCopyExF() => SDL_RenderTextureRotated(), returns bool
;; SDL_RenderCopyF() => SDL_RenderTexture(), returns bool

;; punt?
(defun render-copy (renderer texture &rest args &key source-rect dest-rect)
  "Use this function to copy a portion of the texture to the current rendering target."
  (declare (ignorable source-rect dest-rect))
  (apply #'render-copy-f renderer texture args))

(defun render-copy-f (renderer texture &key source-rect dest-rect)
  "Copy a portion of the texture to the current rendering target at subpixel precision."
  (declare (ignorable source-rect dest-rect))
  (check-rc (sdl-render-texture renderer texture
				(ensure-frect source-rect)
				(ensure-frect dest-rect))))

;; punt?
(defun render-copy-ex (renderer texture  &rest args &key source-rect dest-rect angle center flip)
  "Use this function to copy a portion of the texture to the current rendering target, optionally
rotating it by angle around the given center and also flipping it top-bottom and/or left-right."
  (declare (ignorable source-rect dest-rect angle center flip))
  (apply #'render-copy-ex renderer texture args))

;; SDL_RendererFlip => SDL_FlipMode - moved to SDL_surface.h

(defun render-copy-ex-f (renderer texture &key source-rect dest-rect angle center flip)
  "Copy a portion of the source texture to the current rendering target, with rotation and flipping,
at subpixel precision."
  (declare (ignorable source-rect dest-rect angle center flip))
  (check-rc (sdl-render-texture-rotated
             renderer
             texture
             (ensure-frect source-rect)
             (ensure-frect dest-rect)
             (coerce (or angle 0) 'double-float)
             center
	     (cffi:foreign-enum-value
	      'sdl-renderer-flip flip))))

(defun set-render-draw-color (renderer r g b a)
  "Use this function to set the color used for drawing operations (Rect, Line and Clear)."
  (check-rc (sdl-set-render-draw-color renderer r g b a)))

(defun get-render-draw-color (renderer)
  "Use this function to get the current color used by renderer for drawing operations"
  (cffi:with-foreign-objects ((r :uint8) (g :uint8) (b :uint8) (a :uint8))
    (check-rc (%sdl-get-render-draw-color (cobj:cobject-pointer renderer) r g b a))
    (values (cffi:mem-ref r :uint8)
	    (cffi:mem-ref g :uint8)
	    (cffi:mem-ref b :uint8)
	    (cffi:mem-ref a :uint8))))

(defun set-texture-blend-mode (texture blend-mode)
  "Use this function to set the blend mode for a texture, used by SDL_RenderCopy()."
  (check-rc (sdl-set-texture-blend-mode texture blend-mode)))

(defun set-render-draw-blend-mode (renderer blend-mode)
  "Use this function to set the blend mode used for drawing operations (Fill and Line)."
  (check-rc (sdl-set-render-draw-blend-mode renderer blend-mode)))

(defun set-render-target (renderer texture)
  "Use this function to set a texture as the current rendering target."
  (check-rc (sdl-set-render-target renderer texture)))

;;* SDL_RenderDrawLine() => SDL_RenderLine(), returns bool
;;* SDL_RenderDrawLineF() => SDL_RenderLine(), returns bool
(defun render-draw-line (renderer x1 y1 x2 y2)
  "Use this function to draw a line on the current rendering target."
  (check-rc (sdl-render-line renderer (float x1) (float y1) (float x2) (float y2))))

;;* SDL_RenderDrawLines() => SDL_RenderLines(), returns bool
;;* SDL_RenderDrawLinesF() => SDL_RenderLines(), returns bool
(defun render-draw-lines (renderer points num-points)
  "Pass a pointer to SDL_Point to render connected lines on the current rendering target."
  (assert (cffi:pointerp points))
  (check-rc (%sdl-render-lines (cobj:cobject-pointer renderer)
			       points num-points)))

;;* SDL_RenderDrawPoint() => SDL_RenderPoint(), returns bool
;;* SDL_RenderDrawPointF() => SDL_RenderPoint(), returns bool
(defun render-draw-point (renderer x y)
  "Use this function to draw a point on the current rendering target."
  (check-rc (sdl-render-point renderer x y)))

;;* SDL_RenderDrawPoints() => SDL_RenderPoints(), returns bool
;;* SDL_RenderDrawPointsF() => SDL_RenderPoints(), returns bool
(defun render-draw-points (renderer points num-points)
  "Use this function to draw multiple points on the current rendering target."
  (assert (cffi:pointerp points))
  (check-rc (%sdl-render-points (cobj:cobject-pointer renderer)
			       points num-points)))

;;* SDL_RenderDrawRect() => SDL_RenderRect(), returns bool
;;* SDL_RenderDrawRectF() => SDL_RenderRect(), returns bool
(defun render-draw-rect (renderer sdl-rect)
  "Use this function to draw a rectangle on the current rendering target."
  (check-rc (sdl-render-rect renderer sdl-rect)))

;;* SDL_RenderDrawRectF() => SDL_RenderRect(), returns bool
;;* SDL_RenderDrawRects() => SDL_RenderRects(), returns bool
(defun render-draw-rects (renderer rects num-rects)
  "Use this function to draw some number of rectangles on the current rendering target."
  (assert (cffi:pointerp rects))
  (check-rc (%sdl-render-rects (cobj:cobject-pointer renderer) rects num-rects)))

;;* SDL_RenderFillRectF() => SDL_RenderFillRect(), returns bool
(defun render-fill-rect (renderer sdl-rect)
  "Use this function to fill a rectangle on the current rendering target with
the drawing color,  at subpixel precision."
  (check-rc (sdl-render-fill-rect renderer sdl-rect)))

;; skip render-fill-rect-f

;;* SDL_RenderFillRectsF() => SDL_RenderFillRects(), returns bool
(defun render-fill-rects (renderer rects num-rects)
  "Use this function to fill some number of rectangles on the current
rendering target with the drawing color at subpixel
precision."
  (assert (cffi:pointerp rects))
  (check-rc (%sdl-render-fill-rects (cobj:cobject-pointer  renderer) rects num-rects)))

;; skip render-fill-rects-f

;;* SDL_RenderSetViewport() => SDL_SetRenderViewport(), returns bool
(defun render-set-viewport (renderer sdl-rect)
  "Use this function to set the drawing area for rendering on the current target."
  (check-rc (sdl-set-render-viewport renderer sdl-rect)))

#+nil
(cffi-object::define-struct-cobject-class sdl-rect)

;;* SDL_RenderGetViewport() => SDL_GetRenderViewport(), returns bool
(defun render-get-viewport (renderer)
  "Use this function to get the drawing area for the current target."
  (let ((rect (make-sdl-rect)))
    (check-rc (sdl-get-render-viewport renderer renderer))
    rect))

(defun render-clear (renderer)
  "Use this function to clear the current rendering target with the drawing color."
  (check-rc (sdl-render-clear renderer)))

(defun render-present (renderer)
  "Use this function to update the screen with rendering performed."
  (sdl-render-present renderer))

(defun render-get-name (renderer)
  (sdl-get-renderer-name renderer))

#||
(get-property-names (sdl-get-renderer-properties $r))
#+nil
("SDL.renderer.window" "SDL.renderer.SDR_white_point"
 "SDL.renderer.max_texture_size" "SDL.renderer.output_colorspace"
 "SDL.renderer.HDR_enabled" "SDL.renderer.HDR_headroom" "SDL.renderer.name"
 "SDL.renderer.texture_formats" "SDL.renderer.vsync")

;; => ("SDL.renderer.window" "SDL.renderer.SDR_white_point"
;;  "SDL.renderer.output_colorspace" "SDL.renderer.HDR_enabled"
;;  "SDL.renderer.HDR_headroom" "SDL.renderer.name" "SDL.renderer.texture_formats"
;;  "SDL.renderer.vsync")

(setq $render-property-id (sdl-get-renderer-properties $r))

(sdl-get-string-property $render-property-id "SDL.renderer.name" "")
(setq $p (sdl-get-pointer-property $render-property-id "SDL.renderer.texture_formats" +cobj-null-pointer+))
(type-of $p)
(cobj:cobject-pointer $p)
+SDL-PIXELFORMAT-UNKNOWN+
(cffi:foreign-enum-value 'sdl-pixel-format :sdl-pixelformat-unknown)

(cffi:mem-aref (cobj:cobject-pointer $p) 'sdl-pixel-format 0)
(cffi:mem-aref (cobj:cobject-pointer $p) 'sdl-pixel-format 1)
(cffi:mem-aref (cobj:cobject-pointer $p) 'sdl-pixel-format 3)
(cffi:mem-aref (cobj:cobject-pointer $p) 'sdl-pixel-format 4)
||#

(defun render-get-texture-formats (renderer)
  "Returns a list of keywords of sdl-pixel-format cenum"
  (let* ((group-id (sdl-get-renderer-properties renderer))
	 (p (%sdl-get-pointer-property group-id
				       +sdl-prop-renderer-texture-formats-pointer+
				       ;; "SDL.renderer.texture_formats"
				       (cffi:null-pointer))))
    (unless (cffi:null-pointer-p p)
      (loop for i from 0
	    for fmt = (cffi:mem-aref p 'sdl-pixel-format i)
	    until (eql fmt :SDL-PIXELFORMAT-UNKNOWN)
	    collect fmt))))

#||
(setq $fmts (render-get-texture-formats $r))
(cffi:foreign-enum-keyword-list 'sdl-pixel-format)
(cffi:foreign-enum-value 'sdl-pixel-format  :SDL-PIXELFORMAT-UNKNOWN)
(cffi:foreign-enum-value 'sdl-pixel-format 0)
(cffi:foreign-enum-value 'sdl-pixel-format
			 '(:SDL-PIXELFORMAT-UNKNOWN :SDL-PIXELFORMAT-INDEX1MSB))
||#

#+GONE
(defun get-renderer-info (renderer)
  "SDL_GetRendererInfo() has been removed. The name of a renderer can be retrieved using SDL_GetRendererName(), and the other information is available as properties on the renderer."
  (let* ((name (sdl-get-renderer-name renderer)))
    (check-rc (sdl-get-renderer-info renderer rinfo))
    rinfo))

(defun get-renderer-max-texture-size (renderer)
  (let ((id (sdl-get-renderer-properties renderer)))
    ;; may not be present for a given renderer
    (sdl-get-number-property id  "SDL.renderer.max_texture_size"
			     -1)))

#+nil
(get-renderer-max-texture-size $r)

;; SDL_GetRendererOutputSize() => SDL_GetCurrentRenderOutputSize(), returns bool
(defun get-renderer-output-size (renderer)
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%sdl-get-current-render-output-size (cobj:cobject-pointer renderer) x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

#+nil
(get-renderer-output-size $r)

#+GONE
(defun query-texture (texture)
;; SDL_QueryTexture() has been removed. The properties of the texture can be queried using SDL_PROP_TEXTURE_FORMAT_NUMBER, SDL_PROP_TEXTURE_ACCESS_NUMBER, SDL_PROP_TEXTURE_WIDTH_NUMBER, and SDL_PROP_TEXTURE_HEIGHT_NUMBER. A function SDL_GetTextureSize() has been added to get the size of the texture as floating point values.
  (c-with ((texture-format sdl2-ffi:uint32)
           (access :int)
           (width  :int)
           (height :int))
    (check-rc (sdl-query-texture texture (texture-format &) (access &) (width &) (height &)))
    (values texture-format access width height)))

;;; Convenience functions to query only textures width and height
(defun texture-width (texture)
  (let ((id (sdl-get-texture-properties texture)))
    (sdl-get-number-property id +SDL-PROP-TEXTURE-WIDTH-NUMBER+ -1)))

(defun texture-height (texture)
  (let ((id (sdl-get-texture-properties texture)))
    (sdl-get-number-property id +SDL-PROP-TEXTURE-HEIGHT-NUMBER+ -1)))

(defun update-texture (texture rect pixels pitch)
  "Use this function to update the given texture rectangle with new pixel data."
  (check-rc (sdl-update-texture texture rect pixels pitch)))


(defun handle-short-enum-keyword (keyword cffi-enum-type prefix)
  (etypecase keyword
    (keyword
     (let* ((keywords (cffi:foreign-enum-keyword-list cffi-enum-type)))
       (cond ((find keyword keywords) keyword)
	     (t (let* ((try (concatenate 'string prefix
					 "-"
					(symbol-name keyword)))
		      (key2 (find-symbol try :KEYWORD)))
		  (if (find key2 keywords)
		      key2
		      (error "Unknown enum keyword ~A for ~A (~A)~%want one of ~A" keyword
			     cffi-enum-type key2 keywords)))))))))

#||
(cffi:foreign-enum-keyword-list 'sdl-gpu-texture-format)
(handle-short-enum-keyword :r8-unorm 'sdl-gpu-texture-format "SDL-GPU-TEXTUREFORMAT")
(handle-short-enum-keyword :r8-unorm1 'sdl-gpu-texture-format "SDL-GPU-TEXTUREFORMAT")
||#


(defun create-texture (renderer pixel-format access width height)
  "Use this function to create a texture for a rendering context."
  (setq pixel-format (handle-short-enum-keyword pixel-format
						'sdl-pixel-format
						"SDL-PIXELFORMAT"))
  (setq access (handle-short-enum-keyword access
					  'sdl-texture-access
					  "SDL-TEXTUREACCESS"))
  (check-nullptr (sdl-create-texture renderer pixel-format access
                                     width height)))

(defun create-texture-from-surface (renderer surface)
  "Use this function to create a texture from sdl2 surface for a rendering context."
  (check-nullptr (sdl-create-texture-from-surface renderer surface)))

(defun set-texture-color-mod (texture r g b)
  "Use this function to set an additional color value multiplied into render copy operations."
  (check-rc (sdl-set-texture-color-mod texture r g b)))

;; not unsigned-short but uint8
(defun get-texture-color-mod (texture)
  "Use this function to get the additional color value multiplied into render copy operations."
  (cffi:with-foreign-objects ((r :uint8)
			      (g :uint8)
			      (b :uint8))
    (check-rc (%sdl-get-texture-color-mod (cobj:cobject-pointer texture)
					  r g b))
    (values (cffi:mem-ref r :uint8)
	    (cffi:mem-ref g :uint8)
	    (cffi:mem-ref b :uint8))))

(defun set-texture-alpha-mod (texture alpha)
  "Use this function to set an additional alpha value multiplied into render copy operations."
  (check-rc (sdl-set-texture-alpha-mod texture alpha)))

(defun get-texture-alpha-mod (texture)
  "Use this function to get the additional alpha value multiplied into render copy operations."
  (cffi:with-foreign-object (alpha :uint8)
    (check-rc (%sdl-get-texture-alpha-mod (cobj:cobject-pointer texture) alpha))
    (cffi:mem-ref alpha :uint8)))

(defun destroy-texture (texture)
  "Use this function to destroy the specified texture."
  (sdl-destroy-texture texture)
  #+NIL(invalidate texture))

(defun lock-texture (texture &optional rect)
  "Use this function to lock a portion of the texture for write-only pixel access."
  (cffi:with-foreign-objects ((pixels :pointer)
			      (pitch :int))
    (check-rc (%sdl-lock-texture (cobj:cobject-pointer texture)
				 (if rect (cobj:cobject-pointer rect)
				     (cffi:null-pointer))
				 pixels pitch))
    (values (cffi:mem-ref pixels :pointer)
	    (cffi:mem-ref pitch :int))))

(defun unlock-texture (texture)
  "Use this function to unlock a texture, uploading the changes to video memory, if needed. Warning:
See Bug No. 1586 before using this function!"
  (sdl-unlock-texture texture))

;; The following functions have been removed:
;;* SDL_GL_BindTexture() - use SDL_GetTextureProperties() to get the OpenGL texture ID and bind the texture directly
(defun gl-bind-texture (texture)
  (let* ((prop-id (sdl-get-texture-properties texture))
	 (gl-target (sdl-get-number-property prop-id +SDL-PROP-TEXTURE-OPENGL-TEXTURE-TARGET-NUMBER+ -1))
	 (gl-tex-id (sdl-get-number-property prop-id +SDL-PROP-TEXTURE-OPENGL-TEXTURE-NUMBER+ -1)))
    (assert (and prop-id (/= -1 gl-tex-id)  (/= -1 gl-target)))
    (gl:bind-texture gl-target gl-tex-id)))

;;* SDL_GL_UnbindTexture() - use SDL_GetTextureProperties() to get the OpenGL texture ID and unbind the texture directly
(defun gl-unbind-texture (texture)
  (let* ((prop-id (sdl-get-texture-properties texture))
	 (gl-target (sdl-get-number-property prop-id +SDL-PROP-TEXTURE-OPENGL-TEXTURE-TARGET-NUMBER+ -1))
	 (gl-tex-id (sdl-get-number-property prop-id +SDL-PROP-TEXTURE-OPENGL-TEXTURE-NUMBER+ -1)))
    (assert (and prop-id (/= -1 gl-tex-id)  (/= -1 gl-target)))
    (gl:bind-texture gl-target 0)))


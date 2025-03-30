(in-package "CLAW-CXX-SDL3-USER")

;; SDL_Surface has been simplified and internal details are no longer in the public structure.

;; The format member of SDL_Surface is now an enumerated pixel format value. You can get the full details of the pixel format by calling SDL_GetPixelFormatDetails(surface->format). You can get the palette associated with the surface by calling SDL_GetSurfacePalette(). You can get the clip rectangle by calling SDL_GetSurfaceClipRect().

;; The userdata member of SDL_Surface has been replaced with a more general properties interface, which can be queried with SDL_GetSurfaceProperties()

(defun surface-width (surface)
  (sdl-surface-w surface))

(defun surface-height (surface)
  (sdl-surface-h surface))

(defun surface-pixels (surface)
  "Access raw pixel data from a surface object"
  (sdl-surface-pixels surface))

(defun surface-format (surface)
  (sdl-surface-format surface))

(defun surface-pitch (surface)
  (sdl-surface-pitch surface))

;; The `format` member of SDL_Surface is now an enumerated pixel format value. You can get the full details of the pixel format by calling `SDL_GetPixelFormatDetails(surface->format)`. You can get the palette associated with the surface by calling SDL_GetSurfacePalette(). You can get the clip rectangle by calling SDL_GetSurfaceClipRect().

(defun surface-format-format (surface)
  (sdl-surface-format surface))

;; SDL_CreateRGBSurface() and SDL_CreateRGBSurfaceWithFormat() have been combined into a new function SDL_CreateSurface().
(defun create-rgb-surface (width height depth
                           &key (r-mask 0) (g-mask 0) (b-mask 0) (a-mask 0) (flags 0))
  (when flags (warn "create-rgb-surface: ignored flags ~a" flags))
  (check-nullptr
   (sdl-create-surface width height
		       (sdl-get-pixel-format-for-masks depth r-mask g-mask b-mask a-mask))))

;; SDL_CreateRGBSurfaceFrom() and SDL_CreateRGBSurfaceWithFormatFrom() have been combined into a new function SDL_CreateSurfaceFrom(), and the parameter order has changed for consistency with SDL_CreateSurface().
(defun create-rgb-surface-from (pixels width height depth pitch
                                &key (r-mask 0) (g-mask 0) (b-mask 0) (a-mask 0))
  (check-nullptr
   (sdl-create-surface-from width height
			    (sdl-get-pixel-format-for-masks depth r-mask g-mask b-mask a-mask)
			    pixels
			    pitch)))

(defun create-rgb-surface-with-format-from (pixels width height depth pitch
                                            &key (format
						  :SDL-PIXELFORMAT-RGBA8888))
  (when depth (warn "create-rgb-surface-with-format-from: ignoring depth ~A." depth))
  (sdl-create-surface-from  width height
			    format ;;depth
			    pixels
			    pitch))

;;* SDL_FreeSurface() => SDL_DestroySurface()
(defun free-surface (surface)
  (sdl-destroy-surface surface)
  #+NIL
  (invalidate surface))

;;* SDL_LoadBMP_RW() => SDL_LoadBMP_IO()
;;* SDL_RWFromFile() => SDL_IOFromFile()
(defun load-bmp (filename)
  (sdl-load-bmp-io (sdl-io-from-file (namestring (merge-pathnames filename)) "rb") 1))

;; Removed the unused 'flags' parameter from SDL_ConvertSurface.
(defun convert-surface (surface format &key (flags 0))
  (when flags (warn "convert-surface: ignoring flags ~A." flags))
  (check-nullptr
   (sdl-convert-surface surface format)))

;;* SDL_ConvertSurfaceFormat() => SDL_ConvertSurface()
#+GONE
(defun convert-surface-format (surface pixel-format-enum &key (flags 0))
  (check-nullptr
   (sdl-convert-surface surface
                               (enum-value '(:enum (sdl-pixel-format)) pixel-format-enum)
                               flags)))

;;* SDL_UpperBlit() => SDL_BlitSurface(), returns bool
(defun blit-surface (surface-src src-rect surface-dst dst-rect)
  (check-rc (sdl-blit-surface surface-src src-rect surface-dst dst-rect)))

;;* SDL_UpperBlitScaled() => SDL_BlitSurfaceScaled(), returns bool
(defun blit-scaled (surface-src src-rect surface-dst dst-rect scale-mode)
  (check-rc (sdl-blit-surface-scaled surface-src src-rect surface-dst dst-rect scale-mode)))

;;* SDL_FillRect() => SDL_FillSurfaceRect(), returns bool
;;* SDL_FillRects() => SDL_FillSurfaceRects(), returns bool
(defun fill-rect (surface-dst rect color)
  (check-rc (sdl-fill-surface-rect surface-dst rect color)))

;;* SDL_SetColorKey() => SDL_SetSurfaceColorKey(), returns bool
(defun set-color-key (surface flag key)
  "Use this function to set the color key (transparent pixel) in a surface."
  (check-rc (sdl-set-surface-color-key flag surface key)))

;; * SDL_GetColorKey() => SDL_GetSurfaceColorKey(), returns bool
(defun get-color-key (surface)
  "Use this function to get the color key (transparent pixel) for a surface."
  (cffi:with-foreign-object (key :int)
    (check-rc (%sdl-get-surface-color-key (cobj:cobject-pointer surface) key))
    (cffi:mem-ref key :int)))

(defun set-alpha-mod (surface alpha)
  "Use this function to set an additional alpha value used in blit operations."
  (check-rc (sdl-set-surface-alpha-mod surface alpha)))

(defun get-alpha-mod (surface)
  "Use this function to get the additional alpha value used in blit operations."
  (cffi:with-foreign-object (alpha :int)
    (check-rc (%sdl-get-surface-alpha-mod (cobj:cobject-pointer surface) alpha))
    (cffi:mem-ref alpha :int)))

(defun set-color-mod (surface r g b)
  "Use this function to set an additional color value multiplied into blit operations."
  (check-rc (sdl-set-surface-color-mod surface r g b)))

(defun get-color-mod (surface)
  "Use this function to get the additional color value multiplied into blit operations."
  (cffi:with-foreign-objects ((r :uint8)
			      (g :uint8)
			      (b :uint8))
    (check-rc (%sdl-get-surface-color-mod (cobj:cobject-pointer surface)
					  r g b))
    (values (cffi:mem-ref r :uint8)
	    (cffi:mem-ref g :uint8)
	    (cffi:mem-ref b :uint8))))

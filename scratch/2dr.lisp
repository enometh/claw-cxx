;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Sep 04 10:13:30 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2021 brickmaker <zhaoxiaodong@zju.edu.cn>
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; shaders and implementation based on
;;; https://github.com/brickmaker/webgl2-2d: renderer/renderer.js
;;; (commit d0dbab1203)
;;;
;;; other priors
;;; https://github.com/karellodewijk/canvas-webgl (JS)
;;; https://github.com/play-co/webgl-2d (JS)
;;; https://github.com/tfriedel6/canvas (go)
;;; https://github.com/memononen/nanovg (C)
;;; skity? (C++)
;;; https://github.com/vydd/sketch.git (common lisp)
;;; https://github.com/femtovg/femtovg (Rust) and maybe more than half
;;; a dozen rust knockoffs mostly for webgpu
(cl:in-package "2DR")


(defvar $vertex-Shader-Source "#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 a_position;

// Used to pass in the resolution of the canvas
uniform vec2 u_resolution;

uniform mat4 u_transform;

// all shaders have a main function
void main() {

  vec4 pos = u_transform * vec4(a_position, 0., 1.);

  // TODO: consider use projection matrix
  // convert the position from pixels to 0.0 to 1.0
  vec2 zeroToOne = pos.xy / u_resolution;

  // convert from 0->1 to 0->2
  vec2 zeroToTwo = zeroToOne * 2.0;

  // convert from 0->2 to -1->+1 (clipspace)
  vec2 clipSpace = zeroToTwo - 1.0;

  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
//   gl_Position = vec4(clipSpace, 0, 1);
}
")

(defvar $fragment-Shader-Source "#version 300 es

precision highp float;

uniform vec4 u_color;

// we need to declare an output for the fragment shader
out vec4 outColor;

void main() {
  outColor = u_color;
}
")

(defvar $texture-Vertex-Shader-Source "#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec2 a_position;
in vec2 in_texCoord;

out vec2 texCoord;

// Used to pass in the resolution of the canvas
uniform vec2 u_resolution;

uniform mat4 u_transform;

// all shaders have a main function
void main() {
  texCoord = in_texCoord;

  vec4 pos = u_transform * vec4(a_position, 0., 1.);

  // TODO: consider use projection matrix
  // convert the position from pixels to 0.0 to 1.0
  vec2 zeroToOne = pos.xy / u_resolution;

  // convert from 0->1 to 0->2
  vec2 zeroToTwo = zeroToOne * 2.0;

  // convert from 0->2 to -1->+1 (clipspace)
  vec2 clipSpace = zeroToTwo - 1.0;

  gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
//   gl_Position = vec4(clipSpace, 0, 1);
}
")
(defvar $texture-Fragment-Shader-Source "#version 300 es

precision highp float;

in vec2 texCoord;

uniform vec4 u_color;

uniform sampler2D u_sampler;

// we need to declare an output for the fragment shader
out vec4 outColor;

void main() {
//   outColor = u_color;
  outColor = texture(u_sampler, texCoord);
}
")


(defun create-texture-from-uint8-array (array type width height)
  (let ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-image-2d :texture-2d 0 type width height 0 type
		     :unsigned-byte array :raw t)
    (gl:generate-mipmap :texture-2d)
    texture))

(defun create-texture-info (image width height)
  (let ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (let ((level 0)
	   (internal-format :rgba)
	   (src-format :rgba)
	   (src-type :unsigned-byte))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d level internal-format
		       width height 0
		       src-format src-type image :raw t)
      (gl:generate-mipmap :texture-2d)
      texture)))


(defstruct program-info program vao vbo ebo)

(defun clean-program-info (program-info)
  (with-slots (program vao vbo ebo) program-info
    (when program
      (gl:delete-program program)
      (setq program nil))
    (when (or vbo ebo)
      (gl:delete-buffers (list vbo ebo))
      (setq vbo nil)
      (setq ebo nil))
    (when vao
      (gl:delete-vertex-arrays (list vao))
      (setq vao nil))))

(defstruct (simple-program-info (:include program-info))
	   a-position u-color u-transform u-resolution)

(defun create-simple-program-info (vert-src frag-src)
  (let ((program-info (make-simple-program-info)))
    (with-slots (program
		 a-position u-resolution u-transform u-color
		 vao vbo ebo)
	program-info
      (setq program (link-program-from-shader-strings vert-src frag-src))
      (setq a-position (gl:get-attrib-location program "a_position"))
      (setq u-resolution (gl:get-uniform-location program "u_resolution"))
      (setq u-transform (gl:get-uniform-location program "u_transform"))
      (setq u-color (gl:get-uniform-location program "u_color"))
      (setq vao (gl:create-vertex-array))
      (setq vbo (gl:gen-buffer))
      (gl:bind-vertex-array vao)
      (gl:enable-vertex-attrib-array a-position)
      (gl:bind-buffer :array-buffer vbo)
      (let ((size 2) (type :float) (normalized nil) (stride 0) (offset 0))
	(gl:vertex-attrib-pointer a-position size type normalized stride  offset))
      (setq ebo (gl:gen-buffer))
      (gl:bind-buffer :element-array-buffer ebo))
    program-info))

(defstruct renderer
  current-program-info
  simple-program-info
  texture-program-info
  width height
  (transform #(1 0 0 0
	       0 1 0 0
	       0 0 1 0
	       0 0 0 1)))

(defun setup-simple-program-info (program-info renderer)
  (cond ((eq (renderer-current-program-info renderer) program-info) t)
	(t (setf (renderer-current-program-info renderer) program-info)
	   (with-slots (program vao u-resolution u-transform) program-info
	     (gl:use-program program)
	     (gl:bind-vertex-array vao)
	     (gl:uniformf u-resolution (renderer-width renderer)
			  (renderer-height renderer))
	     (gl:uniform-matrix-4fv u-transform
				    (renderer-transform renderer))))))

(defstruct (texture-program-info (:include program-info))
	   tbo
	   a-position in-texcoord u-resolution u-transform u-sampler)

(defun clean-texture-program-info (program-info)
  (with-slots (tbo) program-info
    (when tbo
      (gl:delete-buffers (list tbo))
      (setq tbo nil))
    (clean-program-info program-info)))

(defun create-texture-program-info (vert-src frag-src)
  (let ((program-info (make-texture-program-info)))
    (with-slots (program
		 a-position u-resolution u-transform u-sampler
		 in-texcoord
		 vao vbo tbo ebo)
	program-info
      (setq program (link-program-from-shader-strings vert-src frag-src))
      (setq a-position (gl:get-attrib-location program "a_position"))
      (setq in-texcoord (gl:get-attrib-location program "in_texCoord"))
      (setq u-resolution (gl:get-uniform-location program "u_resolution"))
      (setq u-transform (gl:get-uniform-location program "u_transform"))
      (setq u-sampler (gl:get-uniform-location program "u_sampler"))
      (setq vao (gl:create-vertex-array))
      (setq vbo (gl:gen-buffer))
      (gl:bind-vertex-array vao)
      (gl:enable-vertex-attrib-array a-position)
      (gl:bind-buffer :array-buffer vbo)
      (let ((size 2) (type :float) (normalized nil) (stride 0) (offset 0))
	(gl:vertex-attrib-pointer a-position size type normalized stride  offset))
      ;; should be called tbo not ebo
      (setq tbo (gl:gen-buffer))
      (gl:enable-vertex-attrib-array in-texcoord)
      (gl:bind-buffer :array-buffer tbo)
      (let ((size 2) (type :float) (normalized nil) (stride 0) (offset 0))
	(gl:vertex-attrib-pointer in-texcoord size type normalized stride  offset))
      (setq ebo (gl:gen-buffer))
      (gl:bind-buffer :element-array-buffer ebo))
    program-info))

(defun setup-texture-program-info (program-info renderer)
  (cond ((eq (renderer-current-program-info renderer) program-info) t)
	(t (setf (renderer-current-program-info renderer) program-info)
	   (with-slots (program
			u-sampler
			vao resolution u-transform)
	       program-info
	     (gl:use-program program)
	     (gl:bind-vertex-array vao)
	     (gl:uniformf resolution
			  (renderer-width renderer)
			  (renderer-height renderer))
	     (gl:uniform-matrix-4fv u-transform
				    (renderer-transform renderer))
	     ;; bind texture to texture unit 0
	     (gl:active-texture :texture0)
	     (gl:uniformi u-sampler 0)))))

#+nil
(defun setup-program-info (program-info renderer)
  (etypecase program-info
    (simple-program-info (setup-simple-program-info program-info renderer))
    (texture-program-info (setup-texture-program-info program-info renderer))))

(defun prepare-stencil ()
  (gl:color-mask nil nil nil nil)
  (gl:depth-mask nil)
  (gl:enable :stencil-test)
  (gl:stencil-func :always 1 #xff)
  (gl:stencil-op :keep :keep :replace))

(defun use-stencil ()
  (gl:color-mask  t t t t)
  (gl:depth-mask t)
  (gl:stencil-func :equal 1 #xff))

(defun renderer-set-transform (renderer transform)
  (with-slots ((renderer-transform transform) current-program-info) renderer
    (setq renderer-transform (copy-seq transform))
    (gl:uniform-matrix-4fv
     (with-slots (u-transform) current-program-info u-transform)
     transform)))

(defun %send-data (target array usage type)
  ;; (%send-data :array-buffer :static-draw vertices :float)
  (let ((n (length array)) glarr)
    (unwind-protect (progn (setq glarr (gl:alloc-gl-array type n))
			   (dotimes (i n)
			     (setf (gl:glaref glarr i) (aref array i)))
			   (gl:buffer-data target usage glarr))
      (gl:free-gl-array glarr))))

(defun renderer-draw (renderer vertices indices fill-style)
  (with-slots (simple-program-info current-program-info) renderer
    (setup-simple-program-info simple-program-info renderer))
  (with-slots (current-program-info) renderer
    (with-slots (vbo ebo u-color) current-program-info
      (gl:bind-buffer :array-buffer vbo)
      (%send-data :array-buffer vertices :static-draw :float)
      (gl:bind-buffer :element-array-buffer ebo)
      (%send-data :element-array-buffer indices :static-draw :int16)
      (gl:uniformfv u-color (map 'vector 'float fill-style))
      (let ((primitive-type :triangles)
	    (count (length indices))
	    (index-type :unsigned-short)
	    (offset 0))
	(%gl:draw-elements primitive-type count index-type offset)))))

(defun renderer-draw-texture (renderer vertices indices texcoords texture)
  (with-slots (texture-program-info current-program-info) renderer
    (setup-texture-program-info texture-program-info renderer))
  (with-slots (current-program-info) renderer
    (with-slots (vbo ebo tbo u-color) current-program-info
      (gl:bind-buffer :array-buffer vbo)
      (%send-data :array-buffer vertices :static-draw :float)
      (gl:bind-buffer :array-buffer tbo)
      (%send-data :array-buffer texcoords :static-draw :float)
      (gl:bind-buffer :element-array-buffer ebo)
      (%send-data :element-array-buffer indices :static-draw :int16)
      (gl:bind-texture :texture-2d texture)
      (let ((primitive-type :triangles)
	    (offset 0)
	    (count (length indices))
	    (index-type :unsigned-short))
	(%gl:draw-elements primitive-type count index-type offset)))))

(defun renderer-draw-image (renderer image width height sx sy swidth sheight dx dy dwidth dheight)
  (let* ((x1 (/ dx height))
	 (x2 (/ (+ sx swidth) width))
	 (y1 (/ sy height))
	 (y2 (/ (+ sy sheight) height))
	 (texcoords (list x1 y1 x2 y1 x2 y2 x1 y2))
	 (vertices (list dx dy (+ dx dwidth) dy (+ dx dwidth) (+ dy dheight) dx  (+ dy dheight)))
	 (indices (list 0 1 2 0 2 3))
	 (texture (create-texture-info image width height)))
    (renderer-draw-texture renderer vertices indices texcoords texture)))

(defun vert-flip (a w h)
  (loop for r below (ash h -1)
	do (rotatef (subseq a (* w r) (* w (1+ r)))
		    (subseq a (* w (- h r 1)) (* w (- h r))))))

(defun renderer-get-image-data (renderer x y w h)
  (let* ((flipy (- (renderer-height renderer) y h))
	 (data
	  (gl:read-pixels x flipy w h :rgba :unsigned-byte)))
    (vert-flip data (* w 4) h)))

(defun renderer-put-image-data (renderer data width height dx dy dirtyx dirtyy dirtywidth dirtyheight)
  (let ((dwidth width)
	(dheight height)
	texture)
    (cond ((and (not (zerop dirtywidth)) (not (zerop dirtyheight)))
	   (let ((array (make-array (* dirtywidth dirtyheight 4)
				    :element-type '(unsigned-byte 8))))
	     (loop for r below (min dirtyheight (- height dirtyy))
		   for slice = (subseq data
				       (* (+ (* (+ dirtyy r) width) dirtyx) 4)
				       (* (+ (* (+ dirtyy r) width)
					     (min width (+ dirtyx dirtywidth)) 4)))
		   for offset = (* r dirtywidth 4)
		   do (setf (subseq array offset) slice))
	     (setq texture (create-texture-from-uint8-array
			    array :rgba dirtyheight dirtywidth)
		   dwidth dirtywidth
		   dheight dirtyheight)))
	  (t (setq texture (create-texture-from-uint8-array
			    data :rgba width height))))
    (let ((texcoords (list 0 0 1 0 1 1 0 1))
	  (vertices (list dx dy (+ dx dwidth) dy (+ dx dwidth) (+ dy dheight) dx (+ dy dheight)))
	  (indices (list 0 1 2 0 2 3)))
      (if (and (not (zerop dirtywidth)) (not (zerop dirtyheight)))
	  (loop for i below (length vertices) by 2 do
		(incf (elt vertices i) dirtyx)
		(incf (elt vertices (1+ i)) dirtyy)))
      (renderer-draw-texture renderer vertices indices texcoords texture))))

(defclass 2dr-app  (tri-app:lifecycle-mixin)
  ((renderer :initform nil))
  (:default-initargs
   :title "render2d-app"))

(defmethod tri-app:cleanup-fn ((app 2dr-app))
  (with-slots (renderer) app
    (when renderer
      (with-slots (simple-program-info texture-program-info current-program-info)
	  renderer
	(when simple-program-info
	  (clean-program-info simple-program-info)
	  (setq simple-program-info nil))
	(when texture-program-info
	  (clean-texture-program-info texture-program-info)
	  (setq texture-program-info nil))
	(setq current-program-info nil)))))

(defmethod tri-app:setup-fn ((app 2dr-app))
  (with-slots (renderer) app
    (setq renderer (make-renderer))
    (with-slots (simple-program-info texture-program-info current-program-info)
	renderer
      (setq simple-program-info (create-simple-program-info $vertex-shader-source $fragment-shader-source))
      (setq texture-program-info (create-texture-program-info $texture-vertex-shader-source $texture-Fragment-Shader-Source))
      (multiple-value-bind (w h) (get-window-size (tri-app:win app))
	(gl:viewport 0 0 w h)
	(setf (renderer-height renderer) h)
	(setf (renderer-width renderer) w))
      (setup-simple-program-info simple-program-info renderer))))

(defmethod tri-app:resize-fn ((app 2dr-app) new-width new-height)
  (gl:viewport 0 0 new-width new-height)
  (with-slots (renderer) app
    (setf (renderer-height renderer) new-width)
    (setf (renderer-width renderer) new-height)))

(defmethod tri-app:update-fn ((app 2dr-app) sdl-event)
  (let ((event-type (get-event-type sdl-event)))
    (format t "2dr-event-handler  type=~A:~%" event-type)
    (when (member event-type '(:sdl-event-key-up
			       :sdl-event-key-down))
      (let ((key-ev (sdl-event-key sdl-event)))
	(format t "~A: scancode=~A ~&" event-type (sdl-keyboard-event-scancode key-ev))))))

(defmethod tri-app:draw-fn ((app 2dr-app))
  (error "use :render-fn-p nil"))

#||
(setq $d2 (make-instance '2dr-app :render-fn-p nil :request-gl-p nil :gles-p nil))
(progn
(tri-app:cleanup-fn $d2)
(tri-app:launch $d2))
(tri-app:shutdown $d2)

;; reset if hijacked to %cl-glfw3:get-proc-address
(setq cl-opengl-bindings:*gl-get-proc-address* nil)
(gl:read-pixels 500 500 10 10 :rgba :unsigned-byte)
||#


(defstruct style (r 0)  (g 0) (b 0) (a 1))

(defmethod make-load-form ((obj style) &optional env)
  (make-load-form-saving-slots obj :slot-names '(r g b a)
			       :environment env))

(defstruct context
  (z-idx 0)
  (path nil)
  (transform #(1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))
  (state-stack)
  (line-width 1)
  (line-join "miter")
  (line-cap "butt")
  (stroke-style #s(style :R 0 :G 0 :B 0 :A 1))
  (fill-style #s(style :r 0 :g 0 :b 0 :a 1)))

#||
(in-main-thread ()
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit)
  (sdl-gl-swap-window (tri-app:win $d2)))

(in-main-thread ()   (sdl-gl-swap-window (tri-app:win $d2)))

(renderer-draw (slot-value $d2 'renderer)
	       #(-0.5 -0.5 0.5 -0.5 0.0 0.5)
	       #()
	       #(0.0 0.8 0.8 1))
||#
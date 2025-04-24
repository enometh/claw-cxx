;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Apr 24 06:07:25 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; MINIMAL TRIANGLE EXAMPLE FOR CEPL.CL-CLAW-SDL3

;;; ----------------------------------------------------------------------
;;;
;;; /7/gtk/EXT-LISP/cepl.examples/examples/triangle.lisp
;;;

(defpackage #:cepl.examples.minimal
  (:use #:cl #:cepl #:rtg-math))
(in-package #:cepl.examples.minimal)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col))

(defun-g tri-vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

(defun-g tri-frag ((color :vec4))
  color)

(defpipeline-g prog-1 ()
  (tri-vert pos-col)
  (tri-frag :vec4))


(defun init-triangle ()
  (when (and *array* (cepl:initialized-p *array*))
    (cepl:free-gpu-array *array*))
  (setq *array* (make-gpu-array (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1))
                                      (list (v!    0   0.5 0) (v! 1 0 0 1))
                                      (list (v! -0.5 -0.36 0) (v! 0 0 1 1)))
                                :element-type 'pos-col))
  (when (and *stream* (cepl:initialized-p *stream*))
    (cepl:free-buffer-stream *stream*))
  (setq *stream* (make-buffer-stream *array*)))

(defun step-demo ()
  (step-host)
  (clear)
  (map-g #'prog-1 *stream*)
  (swap))

;; NOTE: not cepl:repl but cepl.sdl3.repl
#+nil
(cepl.sdl3::repl)

#+nil
(sdl3:in-main-thread ()
  (init-triangle))

#+nil
(sdl3:in-main-thread ()
  (step-demo))

#+nil
(sdl3:in-main-thread ()
  (swap))

#+nil
(cepl:quit)

;;  NOTE: it should be possible to call (cepl.sdl3::repl) again after
;;  (cepl:quit). one may have to call (cepl.sdl3::sdl-shutdown) if the
;;  cepl repl state machine is somehow wedged.

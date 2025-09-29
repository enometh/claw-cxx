;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Sep 29 15:48:17 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; Scratchwork

(in-package "LIBDRM-USER")


(defun desc-plist-find-by-name (string desc-plist)
  (loop for ent in desc-plist
	for name = (getf ent :name)
	if (string-equal name string) return ent))

(defun desc-plist-from-props (c props-alist)
  (loop for (prop-id . value) in props-alist
	collect (get-prop-value-desc-plist c prop-id value)))

(defun proptest-lines->file (out-file &key start-line length)
  (cl-user::shell-command-to-string "proptest | head -n ~D | tail -n ~D > ~A"
				    (+ start-line length) length
				    out-file))

(defvar $proptest-crtc0-gamma-lut-start-line 906)
(defvar $proptest-crtc0-gamma-lut-length 512)

(defun proptest-crtc0-gama-lut-lines->file
    (out-file &key (start-line $proptest-crtc0-gamma-lut-start-line)
     (length $proptest-crtc0-gamma-lut-length))
  (proptest-lines->file out-file :start-line start-line :length length))

(defun 3x1024->1x4096 (a)
  (make-array  (* (cl:length (car a)) 4)
	       :element-type '(unsigned-byte 16)
	       :initial-contents
	       (loop for r across (car a)
		     for g across (elt a 1)
		     for b across (elt a 2)
		     append (cl:list r g b 0))))

(defun 1x4096->3x1024 (array)
  (loop with size = (/ (cl:length array) 4)
	for i below (cl:length array) by 4
	for r = (elt array i)
	for g = (elt array (+ 1 i))
	for b = (elt array (+ 2 i))
	collect r into rs collect g into gs collect b into bs
	finally (return (cl:values
			 (mapcar
			  (lambda (a)
			    (make-array size :element-type '(unsigned-byte 16)
					:initial-contents a))
			  (cl:list rs gs bs))
			 (mapcar 'cl:length (cl:list rs gs bs))))))

(error "JUNK AT EOF")

;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(conn-close $c)
(conn-open $c)

(setq $conn0 (elt (get-connector-properties $c) 0))
(setq $conn0-props (desc-plist-from-props $c (third $conn0)))
(desc-plist-find-by-name "DPMS" $conn0-props)

(setq $crtc0 (elt (get-crtc-properties $c) 0))
(setq $crtc0-props (desc-plist-from-props $c (third $crtc0)))

(setq $gamma-ramp-size
      (getf (desc-plist-find-by-name "GAMMA_LUT_SIZE" $crtc0-props)
	    :prop-value))

(setq $gamma-ramp-blob-id
      (getf (desc-plist-find-by-name "GAMMA_LUT" $crtc0-props)
	    :prop-value))

(setq $u8 (make-array (* $gamma-ramp-size 8)
		      :element-type '(unsigned-byte 8)
		      :initial-contents
		      (get-blob-u8 $c $gamma-ramp-blob-id)))

(user::string->file
 (with-output-to-string (s)
   (print-u8 $u8 s))
 "/tmp/u8.out")

(setq $u16 (make-array (* $gamma-ramp-size 4)
		      :element-type '(unsigned-byte 16)
		      :initial-contents
		      (get-blob-u16 $c $gamma-ramp-blob-id
				    :zero-out-reserved-field nil)))

(require 'binary-types)
(setq binary-types:*endian* :little-endian)

(user::string->file
 (with-output-to-string (s)
   (print-u8 (binary-types:split-bytes (coerce $u16 'cl:list) 16 8) s))
 "/tmp/u16a.out")

diff /tmp/u8.out /tmp/u16a.out

(user::string->file
 (with-output-to-string (s)
   (print-u16 $u16 s))
 "/tmp/u16.out")

diff /tmp/u8.out /tmp/u16.out

(equalp (coerce $u8 'cl:list)
	(hex-string-to-usb8 (user::slurp-file "/tmp/u8.out" nil :element-type 'character)))

(equalp (coerce $u16 'cl:list)
	(usb8->usb16-le (hex-string-to-usb8 (user::slurp-file "/tmp/u8.out" nil :element-type 'character) )))

(equalp (coerce $u16 'cl:list)
	(hex-string-to-usb16 (user::slurp-file "/tmp/u8.out" nil :element-type 'character)))

(require 'girlib)
(use-package "GDBUS")

(setq $a (make-instance 'dbus-base-object
	   :bus (dbus-session-bus)
	   :bus-name "org.gnome.Mutter.DisplayConfig"))
(setq $p (make-dbus-proxy-for-interface $a "org.gnome.Mutter.DisplayConfig"))

(progn
  (setq $res1 (multiple-value-list (proxy-call-sync $p "GetResources" nil)))
  (multiple-value-setq ($serial $crtc-id)
    (destructuring-bind (serial available-crtcs available-outputs available-modes max-screen-width max-screen-height) $res1
      (let* ((crtc-num 0)
	     (crtc-elt (elt available-crtcs crtc-num))
	     (crtc-id (car crtc-elt)))
	(cl:values serial crtc-id)))))

(proptest-crtc0-gama-lut-lines->file "/tmp/1.out" :start-line 389 :length 512)
diff /tmp/u16.out /tmp/1.out


(setq $res2 (multiple-value-list (proxy-call-sync $p "GetCrtcGamma"
						  (cl:list $serial $crtc-id))))

;; check
(equalp $res2 (multiple-value-list (proxy-call-sync $p "GetCrtcGamma"
						  (cl:list $serial $crtc-id))))

;; restore
(setq $res3 (multiple-value-list (proxy-call-sync $p "SetCrtcGamma"
						  (list* $serial
							 $crtc-id
							 $res2))))


(setq $inp (1x4096->3x1024 (hex-string-to-usb16 user::$maher-lut-contents)))

(setq $res3 (multiple-value-list (proxy-call-sync $p "SetCrtcGamma"
						  (list* $serial
							 $crtc-id
							 $inp))))

(equalp $inp  (multiple-value-list (proxy-call-sync $p "GetCrtcGamma"
						  (cl:list $serial
							   $crtc-id))))

(cl:list (subseq (car $res2) 0 10) (subseq (car $inp) 0 10))


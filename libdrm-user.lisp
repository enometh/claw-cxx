(defpackage "LIBDRM-USER"
  (:use "CL" "CLAW-CXX-LIBDRM")
  (:shadow "COUNT" "LENGTH" "LIST" "MAP" "MOD"
   "SIGNAL" "SEQUENCE" "TYPE" "VALUES"))
(in-package "LIBDRM-USER")


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defstruct conn fd res)

(defvar $c (make-conn))

(defun call-perror-fn (dispatch-fn fmt fmt-args)
  (apply dispatch-fn "~@?: ~A" fmt (append fmt-args (cl:list (cffi:foreign-funcall "strerror" :int (cffi:mem-ref (cffi:foreign-funcall  "__errno_location" :pointer) :int) :string)))))

(defun sys-error (fmt &rest fmt-args)
  (call-perror-fn #'error fmt fmt-args))

(defun sys-cerror (fmt &rest fmt-args)
  (with-simple-restart (cont "continue, ignoring sys-error")
    (apply #'sys-error fmt fmt-args)))

(defun conn-open (c &key (card-string "/dev/dri/card0"))
  (with-slots (fd res) c
    (assert (null fd) nil "close ~A first" c)
    (let ((ret (cffi:foreign-funcall "open" :string card-string
				     :int (logior #o00000002
						  #o02000000)
				     :int)))
      (if (< ret 0)
	  (sys-error "opening ~S" card-string)
	  (setq fd ret)))
    (setq res (drm-mode-get-resources fd))
    c))

#||
;; CCL ERROR: compiler macro calls funcall-dynamic-extent-form which
is not defined.
(compile nil (lambda (c)
	       (with-slots (res) c
		 (drm-mode-free-resources  res)
		 )e))
cobj::funcall-dynamic-extent-form
cobj::funcall-form-type
(cobj::funcall-dynamic-extent-form 'drm-mode-free-resources nil)
(cobj::define-struct-cobject-class %drm-mode-res)
(cobj::define-struct-cobject-class %drm-mode-property)
||#

(defun conn-close (c)
  (with-slots (fd res) c
    (when (and fd #+nil (> fd 0))
      (if (< (cffi:foreign-funcall "close" :int fd :int) 0)
	  (sys-error "closing fd ~D" fd))
      (setq fd nil))
    (when res
      ;; FIXME CCL BUG ON (drm-mode-free-resource)
      (%drm-mode-free-resources (cobj:cobject-pointer res))
      (setq res nil))
    c))

(defun get-connectors (c)
  (with-slots (res) c
    (loop for i below (drm-mode-res-count-connectors res)
	  collect (cobj:cref (drm-mode-res-connectors res) i))))

(defun get-crtcs (c)
  (with-slots (res) c
    (loop with crtcs = (%drm-mode-res-crtcs res)
	  for i below (%drm-mode-res-count-crtcs res)
	  collect (cobj:cref crtcs i))))

(defun cobj-nullp (obj)
  (cffi:null-pointer-p (cobj:cobject-pointer obj)))

(defmacro let-finally ((binding-var binding-form finally-form) &body body)
  `(let ((,binding-var nil))
     (unwind-protect
	  (progn
	    (setq ,binding-var ,binding-form)
	    ,@body)
       (when ,binding-var
	 ,finally-form))))

(defun get-prop-prop-values (props)
  "Return list of (PROP-ID .  VALUE)"
  (loop for i below (drm-mode-object-properties-count-props props)
	collect (cons (cobj:cref (drm-mode-object-properties-props props) i)
		      (cobj:cref (drm-mode-object-properties-prop-values props) i))))

(defun get-connector-properties (c)
  "Return list of (CONN-ID ID PROP-VALUES)"
  (with-slots (fd) c
    (loop for c-id in (get-connectors c) collect
	  (let-finally (c1 (drm-mode-get-connector fd c-id)
			   (drm-mode-free-connector c1))
	    (if (cffi:null-pointer-p (cobj:cobject-pointer c1))
		(sys-cerror "could not get connector ~d" c-id))
	    (assert (= c-id (drm-mode-connector-connector-id c1)))
	    (cl:list c-id
		     (drm-mode-get-connector-type-name
		      (drm-mode-connector-connector-type c1))
		     #+nil
		     (drm-mode-connector-connector-type-id c1)
		     (let-finally (props (drm-mode-object-get-properties fd c-id +DRM-MODE-OBJECT-CONNECTOR+)
					 (drm-mode-free-object-properties props))
		       (get-prop-prop-values props)))))))

#+nil
(get-connector-properties $c)

(defun get-crtc-properties (c)
  "Return list of (CRTC-ID ID PROP-VALUES)"
  (with-slots (fd) c
    (loop for c-id in (get-crtcs c) collect
	  (let-finally (crtc (drm-mode-get-crtc fd c-id)
			     (drm-mode-free-crtc crtc))
	    (if (cobj-nullp crtc)
		(sys-cerror "could not get crtc ~D" c-id))
	    (assert (= c-id (%drm-mode-crtc-crtc-id crtc)))
	    (cl:list c-id
		     #+nil
		     (%drm-mode-crtc-crtc-id crtc)
		     (drm-mode-crtc-crtc-id crtc)
		     (let-finally (props (drm-mode-object-get-properties fd c-id +DRM-MODE-OBJECT-CRTC+)
					 (drm-mode-free-object-properties props))
		       (get-prop-prop-values props)))))))

#+nil
(get-crtc-properties $c)



#||
(conn-open $c)
(conn-close $c)
(get-connectors $c)
(get-crtcs $c)
(cffi:null-pointer-p (cobj:cobject-pointer (drm-mode-get-connector (conn-fd $c) 10)))
(mapcar 'type-of (cl:list (conn-res $c) (drm-mode-get-resources (conn-fd $c))))
(cobj:cpointer-eq (conn-res $c) (drm-mode-get-resources (conn-fd $c)))
(cl:list (conn-res $c) (drm-mode-get-resources (conn-fd $c)))
||#

(defun cobj-show (obj &aux (old-obj obj) converted-p)
  (when (typep obj 'cobj:cpointer)
    (setq obj
	  (cobj:pointer-cobject (cobj:cobject-pointer obj)
				(cobj::cpointer-element-type obj)))
    (setq converted-p t))
  (let* ((class (class-of obj))
	 (class-name (class-name class))
	 (cobj-defn (cobj::cobject-class-definition class-name))
	 (acc-alist
	  (cobj::cobject-class-definition-slot-accessors cobj-defn))
	 (ret (loop for (sym . acc) in acc-alist
			   collect (cons sym (funcall acc obj)))))
    (cl:values (cons (cobj:cobject-pointer obj)
		     ret)
	       converted-p)))

#||
+DRM-MODE-OBJECT-CONNECTOR+
+DRM-MODE-OBJECT-CRTC+
(drm-mode-free-object-properties $props)
(setq $props (drm-mode-object-get-properties (conn-fd $c) 241 +DRM-MODE-OBJECT-CONNECTOR+))
(cobj-show $props)
(cobj-show (cobj:cref $props 0))
(setq $p6 (cobj:cref $props 6))
(cobj-show $p6)
(setq $p6-prop (drm-mode-get-property (conn-fd $c) 6))
(%dump-prop $p6-prop)
||#

(defun get-blob-u8 (c blob-id)
  (let-finally (blob (drm-mode-get-property-blob (conn-fd c) blob-id)
		     (drm-mode-free-property-blob blob))
    (if (cobj-nullp blob)
	nil
	(let* ((blob-data (%drm-mode-property-blob-data blob))
	       (blob-ptr (cobj:cobject-pointer blob-data))
	       (len (%drm-mode-property-blob-length blob)))
	  (loop for i below len
		collect (cffi:mem-aref blob-ptr :unsigned-char i))))))

#||
libdrm-2.4.125/include/drm/drm_mode.h:
struct drm_color_lut {
	/*
	 * Values are mapped linearly to 0.0 - 1.0 range, with 0x0 == 0.0 and
	 * 0xffff == 1.0.
	 */
	__u16 red;
	__u16 green;
	__u16 blue;
	__u16 reserved;
};
||#

(defun zero-out-reserved-field-u16 (u16)
  (let ((i 0) (count 0))
    (cl:map nil (lambda (x)
		  (when (and (= (cl:mod i 4) 3) (not (zerop x)))
		    (setf (elt u16 i) 0)
		    (incf i) (incf count)))
	    u16)
    (cl:values u16 count)))

(defun zero-out-reserved-field-u8 (u8)
  (let ((i 0) (count 0))
    (cl:map nil (lambda (x)
		  (when (and (find (cl:mod i 8) '(6 7)) (not (zerop x)))
		    (setf (elt u8 i) 0)
		    (incf i) (incf count)))
	    u8)
    (cl:values u8 count)))

(defun get-blob-u16 (c blob-id &key (zero-out-reserved-field nil))
  (let-finally (blob (drm-mode-get-property-blob (conn-fd c) blob-id)
		     (drm-mode-free-property-blob blob))
    (if (cobj-nullp blob)
	nil
	(let* ((blob-data (%drm-mode-property-blob-data blob))
	       (blob-ptr (cobj:cobject-pointer blob-data))
	       (len (/ (%drm-mode-property-blob-length blob) 2)))
	  (loop for i below len
		collect (if (and zero-out-reserved-field
				 (= (cl:mod i 4) 3))
			    0
			    (cffi:mem-aref blob-ptr :uint16 i)))))))

(defun print-u8 (u8 &optional (stream t))
  (loop for i below (cl:length u8)
	if (zerop (cl:mod i 16))
	do (format stream "~%~C~C~C" #\Tab #\Tab #\Tab)
	end
	do (format stream "~(~2,'0X~)" (elt u8 i))))

#+nil
(string-equal (with-output-to-string (s) (print-u8 #(#xf3 #x0a) s))
 "
			f30a")

(defun print-u16 (u16 &optional (stream t))
  (loop for i below (cl:length u16)
	for n = (elt u16 i)
	if (zerop (cl:mod i 8))
	do (format stream "~%~C~C~C" #\Tab #\Tab #\Tab)
	end
	do (format stream "~(~2,'0X~2,'0X~)"
		   (logand #xff n)
		   (logand #xff (ash n -8)))))

#+nil
(string-equal (with-output-to-string (s) (print-u16 '(#xaf3) s))
 "
			f30a")

(defun hex-string-to-usb8 (string &key (start 0) (end (cl:length string)))
  "Ignores all non-hex characters"
  (prog ((i start) a b ret)
   check-termination
     (unless (< i end)
       (return (if (or a b)
		   (nreconc ret (+ (* (or a 0) 16) (or b 0)))
		   (nreverse ret))))
     (unless a
       (setq a (digit-char-p (elt string i) 16))
       (incf i)
       (go check-termination))
     (unless b
       (setq b (digit-char-p (elt string i) 16))
       (incf i)
       (push (+ (* a 16) b) ret)
       (setq a nil b nil)
       (go check-termination))))

#+nil
(equalp (hex-string-to-usb8 "f30a") '(#xf3 #x0a))

(defun usb8->usb16-le (u8)
  (loop for i below (cl:length u8) by 2
	for a = (elt u8 i)
	for b = (elt u8 (1+ i))
	collect (+ (logand #xff a)
		   (ash (logand #xff b) 8))))

#+nil
(equalp (usb8->usb16-le '(#xf3 #x0a))  '(#xaf3))

(defun dump-blob (c blob-id &optional (stream t))
  (let-finally (blob (drm-mode-get-property-blob (conn-fd c) blob-id)
		     (drm-mode-free-property-blob blob))
    (when (cobj-nullp blob)
      (format stream "~%")
      (return-from dump-blob nil))
    (let ((blob-data (cobj:cobject-pointer (%drm-mode-property-blob-data blob))))
      (loop for i below (%drm-mode-property-blob-length blob)
	    if (zerop (cl:mod i 16))
	    do (format stream "~%~C~C~C" #\Tab #\Tab #\Tab)
	    end
	    do (format stream "~(~2,'0X~)" (cffi:mem-aref blob-data :unsigned-char i)))
      (terpri stream))))

#+nil
(dump-blob $c 242)

(defvar $drm-mode-flags '(+DRM-MODE-PROP-PENDING+
			  +DRM-MODE-PROP-IMMUTABLE+
			  +DRM-MODE-PROP-SIGNED-RANGE+
			  +DRM-MODE-PROP-RANGE+
			  +DRM-MODE-PROP-ENUM+
			  +DRM-MODE-PROP-BITMASK+
			  +DRM-MODE-PROP-BLOB+
			  +DRM-MODE-PROP-OBJECT+))

#||
(defun drm-mode-prop-type (n) (ash n 6))
(= +DRM-MODE-PROP-OBJECT+ (drm-mode-prop-type 1))
(= +DRM-MODE-PROP-SIGNED-RANGE+ (drm-mode-prop-type 2))
(= +DRM-MODE-PROP-EXTENDED-TYPE+ 65472)
||#

;; xf86drmMode.h static inlines
(defun drm-mode-get-property-type (prop)
  (let ((ret
	 (logand (%drm-mode-property-flags prop)
		 (logior +DRM-MODE-PROP-LEGACY-TYPE+
			 +DRM-MODE-PROP-EXTENDED-TYPE+))))
    (cl:values ret
	       (loop for i in $drm-mode-flags
		     for ok = (= ret (symbol-value i))
		     if ok
		     collect i))))

(defun drm-property-type-is (prop type)
  (= (drm-mode-get-property-type prop) type))

(defun %dump-prop (prop &optional (stream t))
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (format stream " ~A:~&"
	  (cobj:ccoerce (%drm-mode-property-name prop) 'string))
  (format stream "~C~Cflags:" #\Tab #\Tab)
  (let ((f (%drm-mode-property-flags prop)))
    (when (plusp (logand f +DRM-MODE-PROP-PENDING+))
      (format stream " pending"))
    (when (plusp (logand f +DRM-MODE-PROP-IMMUTABLE+))
      (format stream " immutable"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-SIGNED-RANGE+)
      (format stream " signed range"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-RANGE+)
      (format stream " range"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-ENUM+)
      (format stream " enum"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-BITMASK+)
      (format stream " bitmask"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-BLOB+)
      (format stream " blob"))
    (when (drm-property-type-is prop +DRM-MODE-PROP-OBJECT+)
      (format stream " object")))
  (terpri stream)
  (when (drm-property-type-is prop +DRM-MODE-PROP-SIGNED-RANGE+)
    (format stream "~C~Cvalues:" #\Tab #\Tab)
    (loop for i below (%drm-mode-property-count-values prop)
	  do (format stream " ~D" ;; (coerce U642I64 (prop->values[i]))
		     (cobj:cref (%drm-mode-property-values prop) i)))
    (terpri stream))
  (when (drm-property-type-is prop +DRM-MODE-PROP-RANGE+)
    (format stream "~C~Cvalues:" #\Tab #\Tab)
    (loop for i below (%drm-mode-property-count-values prop)
	  do (format stream " ~D"
		     (cobj:cref (%drm-mode-property-values prop) i)))
    (terpri stream))
  (cond ((drm-property-type-is prop +DRM-MODE-PROP-ENUM+)
	 (format stream "~C~Cenums:" #\Tab #\Tab)
	 (loop for i below (%drm-mode-property-count-enums prop)
	       for e = (cobj:cref (%drm-mode-property-enums prop) i)
	       do (format stream " ~A=~D"
			  (cobj:ccoerce (drm-mode-property-enum-name e) 'string)
			  (drm-mode-property-enum-value e)))
	 (terpri stream))
	((drm-property-type-is prop +DRM-MODE-PROP-BITMASK+)
	 (format stream "~C~Cvalues:" #\Tab #\Tab)
	 (loop for i below (%drm-mode-property-count-enums prop)
	       for b = (cobj:cref (%drm-mode-property-enums prop) i)
	       do (format stream " ~A=#x~X"
			  (cobj:ccoerce (drm-mode-property-enum-name b) 'string)
			  (ash 1 (drm-mode-property-enum-value b))))
	 (terpri stream))
	(t (assert (zerop (%drm-mode-property-count-enums prop))))))


#+nil
(%dump-prop $p6-prop t)

(defun dump-prop (c prop-id value &optional (stream t))
  (let-finally (prop (drm-mode-get-property (conn-fd c) prop-id)
		     (drm-mode-free-property prop))
    (format stream "~C~D" #\Tab prop-id)
    (when (cobj-nullp prop)
      (format stream "~%")
      (return-from dump-prop nil))
    (%dump-prop prop stream)
    (cond ((drm-property-type-is prop +DRM-MODE-PROP-BLOB+)
	   (format stream "~C~Cblobs:~%" #\Tab #\Tab)
	   (loop for i below (%drm-mode-property-count-blobs prop)
		 do (dump-blob c (cobj:cref (%drm-mode-property-blob-ids prop) i) stream)))
	  (t (assert (zerop (%drm-mode-property-count-blobs prop)))))
    (format stream "~C~Cvalue:" #\Tab #\Tab)
    (cond ((drm-property-type-is prop +DRM-MODE-PROP-BLOB+)
	   (dump-blob c value stream))
	  ((drm-property-type-is prop +DRM-MODE-PROP-SIGNED-RANGE+)
	   (format stream " ~D" value))
	  (t (format stream " ~D" value))))
  (terpri stream))

(defun list-object-properties (c id type &optional (stream t))
  (check-type type (member #.+DRM-MODE-OBJECT-CONNECTOR+ #.+DRM-MODE-OBJECT-CRTC+))
  (let-finally (props (drm-mode-object-get-properties
		       (conn-fd c) id type)
		      (drm-mode-free-object-properties props))
    (loop with p-props = (drm-mode-object-properties-props props)
	  with p-values = (drm-mode-object-properties-prop-values props)
	  for i below (drm-mode-object-properties-count-props props)
	  for p-id = (cobj:cref p-props i)
	  for p-value = (cobj:cref p-values i)
	  do (dump-prop c p-id p-value stream)
	  collect (cons p-id p-value))))

#+nil
(list-object-properties $c 241 +DRM-MODE-OBJECT-CONNECTOR+)

(defun list-connector-properties (c &optional (stream t))
  (with-slots (fd res) c
    (loop with conns = (drm-mode-res-connectors res)
	  for i below (drm-mode-res-count-connectors res)
	  for conn-id = (cobj:cref conns i)
	  do (let-finally (conn (drm-mode-get-connector fd conn-id)
				(drm-mode-free-connector conn))
	       (if (cobj-nullp conn)
		   (sys-cerror "could not get connector ~D" conn-id))
	       (format stream "Connector ~D (~A-~D)~%"
		       (drm-mode-connector-connector-id conn)
		       (drm-mode-get-connector-type-name
			(drm-mode-connector-connector-type conn))
		       (drm-mode-connector-connector-type-id conn))
	       (list-object-properties
		c (drm-mode-connector-connector-id conn)
		+DRM-MODE-OBJECT-CONNECTOR+)))))

(defun list-crtc-properties (c &optional (stream t))
  (with-slots (fd res) c
    (loop with crtcs = (drm-mode-res-crtcs res)
	  for i below (drm-mode-res-count-crtcs (conn-res $c))
	  for crtc-i = (cobj:cref crtcs i)
	  do (let-finally (crtc (drm-mode-get-crtc fd crtc-i)
				(drm-mode-free-crtc crtc))
	       (if (cobj-nullp crtc)
		   (sys-cerror "could not get crtc ~D" crtc-i))
	       (let ((id (%drm-mode-crtc-crtc-id crtc)))
		 (assert (= id crtc-i))
		 (format stream "CRTC ~D~%" id)
		 (list-object-properties c id +DRM-MODE-OBJECT-CRTC+ stream))))))

;; proptest interface
(defun list-all-properties (c)
  (list-connector-properties c)
  (list-crtc-properties c))

#+nil
(list-all-properties $c)

(defun set-property (c obj-id obj-type prop-id value)
  (check-type obj-type (member #.+DRM-MODE-OBJECT-CONNECTOR+
			       #.+DRM-MODE-OBJECT-CRTC+))
  (if (< (drm-mode-object-set-property (conn-fd c)
				       obj-id obj-type prop-id value))
      (sys-error "error setting property")))

(in-package "CLAW-CXX-SDL3-USER")

(defun make-point (x y)
  "Return an SDL_Point filled in with the arguments."
  (make-sdl-point :x x :y y))

#||
;; the c-macros aren't going in because they have to be copied.
(defmacro c-point ((wrapper-var) &body body)
  `(let ((,wrapper-var (make-sdl-point)))
     ,@body))

(defmacro c-points ((&rest wrappers) &body body)
  (if wrappers
      `(c-point (,(car wrappers))
         (c-points (,@(cdr wrappers)) ,@body))
      `(progn ,@body)))
||#

(defmethod print-object ((point sdl-point) stream)
  (print-unreadable-object (point stream :type t :identity t)
    (format stream "x ~A y ~A" (sdl-point-x point) (sdl-point-y point))))

(defun copy-point (point)
  "Allocate and return a new SDL_Point and make its slots be equal to the passed in SDL_Point."
  (make-sdl-point :x (sdl-point-x point) :y (sdl-point-y point)))

(defun copy-into-point (dest-point src-point)
  "Copy the information from the SDL_Point src-point into the SDL_Point dest-point. Return the
dest-point."
  (setf (sdl-point-x dest-point)  (sdl-point-x src-point))
  (setf (sdl-point-y dest-point)  (sdl-point-y src-point))
  dest-point)

(defun free-point (point)
  "Specifically free the SDL_Point structure."
  ;; WHAT DO ???
  )

;; used as a helper for with-points
(defmacro %with-point ((binding) &body body)
  (cond
    ((symbolp binding)
     `(let ((,binding (make-point 0 0)))
        (unwind-protect (progn ,@body)
	  (free-point ,binding))))
    ((= (length binding) 3)
     `(let ((,(first binding) (make-point ,@(cdr binding))))
	(unwind-protect (progn ,@body)
	  (free-point ,(first binding)))))
    (t
     (error "with-point: Must have a binding of either a symbol or a symbol and 2 forms which are ~
x y of a point"))))

(defmacro with-points (bindings &body body)
  "A LET-like convenient bindings facility for SDL_point structures. Raw symbols are bound
to (make-point 0 0).

  Example:

  (let ((a 1) (b 2))
    (with-points (foo
                  (qux 5 10)
                  (bar (1+ a) b)
       (list foo qux bar))))

  -> (#<SDL-FFI:SDL-POINT x 0 y 0>
      #<SDL-FFI:SDL-POINT x 5 y 10>
      #<SDL-FFI:SDL-POINT x 2 y 2>)"
  (if bindings
      `(%with-point (,(car bindings))
         (with-points ,(cdr bindings) ,@body))
      `(progn ,@body)))

#||
(cobj::define-struct-cobject-class (:struct sdl-point))
(setq $points (list (make-point 0 0) (make-point 1 1 ) (make-point 2 2)))
(cffi:foreign-type-size '(:struct sdl-point))
(setq $ptr (cffi:foreign-alloc 'sdl-point :count 3))
(cobj:pointer-cobject (cffi:mem-aptr $ptr '(:struct sdl-point)  0) 'sdl-point)
||#

(defun points* (&rest points)
  "XXX Return a cffi pointer to SDL_Point and the number of elements in it."
  (let* ((num-points (length points))
	 (ptr (cffi:foreign-alloc 'claw-cxx-sdl3:sdl-point :count (length points))))
    (loop :for i :from 0
	  :for point :in points
	  :do (copy-into-point (cobj:pointer-cobject
				(cffi:mem-aptr ptr '(:struct sdl-point) i)
				'sdl-point)
			       point))
    (values ptr num-points)))

#||
(cffi:foreign-type-size '(:struct sdl-rect))
(cobj::define-struct-cobject-class (:struct sdl-rect))
(make-sdl-rect)
||#

#||
;; skip c-macros
(defmacro c-rect ((r) &body body)
  `(let ((,r (make-sdl-rect)))
     ,@body))

(defmacro c-rects ((&rest wrappers) &body body)
  (if wrappers
      `(c-rect (,(car wrappers))
         (c-rects (,@(cdr wrappers)) ,@body))
      `(progn ,@body)))
||#

(defun make-rect (x y w h)
  "Allocate and return a new SDL_Rect filled in with the arguments."
  (let ((rect (make-sdl-rect)))
    (setf (sdl-rect-x rect) x
          (sdl-rect-y rect) y
          (sdl-rect-w rect) w
          (sdl-rect-h rect) h)
    rect))


(defmethod print-object ((rect sdl-rect) stream)
  (print-unreadable-object (rect stream :type t :identity t)
    (format stream "x ~A y ~A w ~A h ~A"
	    (sdl-rect-x rect)
	    (sdl-rect-y rect)
	    (sdl-rect-w rect)
	    (sdl-rect-h rect))))

(defun copy-rect (rect)
  "Allocate and return a new SDL_Rect and make its slots be equal to the passed in SDL_Rect."
  (make-rect (sdl-rect-x rect)
	     (sdl-rect-y rect)
	     (sdl-rect-w rect)
	     (sdl-rect-h rect)))

(defun copy-into-rect (dest-rect src-rect)
  "Copy the information from the SDL_Rect src-rect into the SDL_Rect dest-rect. Return the
dest-rect."
    (setf (sdl-rect-x src-rect) (sdl-rect-x src-rect)
	  (sdl-rect-y src-rect) (sdl-rect-y src-rect)
	  (sdl-rect-w src-rect) (sdl-rect-w src-rect)
	  (sdl-rect-h src-rect) (sdl-rect-h src-rect))
    dest-rect)

(defun free-rect (rect)
  "Specifically free the SDL_Rect structure."
  ;; WHAT DO??
  )


;;; skip let-rects

;; used as a helper for with-rects
(defmacro %with-rect ((binding) &body body)
  (cond
    ((symbolp binding)
     `(let ((,binding (make-rect 0 0 0 0)))
        (unwind-protect (progn ,@body)
	  (free-rect ,binding))))
    ((= (length binding) 5)
     `(let ((,(first binding) (make-rect ,@(cdr binding))))
        (unwind-protect (progn ,@body)
	  (free-rect ,(first binding)))))
    (t
     (error "with-rect: Must have a binding of either a symbol or a symbol and 4 forms which are ~
x y w h of a rectangle"))))

(defmacro with-rects (bindings &body body)
  "A LET-like convenient bindings facility for SDL_Rect structures. Raw symbols are bound
to (make-rect 0 0 0 0).

  Example:

  (let ((a 1) (b 2) (c 3) (d 4))
    (with-rects (foo
                 (qux 5 10 15 20)
                 (bar (1+ a) b c (* d 10)))
       (list foo qux bar)))

  -> (#<SDL-FFI:SDL-RECT x 0 y 0 w 0 z 0>
      #<SDL-FFI:SDL-RECT x 5 y 10 w 15 h 20>
      #<SDL-FFI:SDL-RECT x 2 y 2 w 3 d 40>)"
  (if (null bindings)
      `(progn ,@body)
      `(%with-rect (,(car bindings))
         (with-rects ,(cdr bindings) ,@body))))

(defun rects* (&rest rects)
  "Return a pointer to SDL_Rect and the number of elements in it."
  (let* ((num-rects (length rects))
	 (ptr (cffi:foreign-alloc '(:struct sdl-rect) :count num-rects)))
    (loop :for i :from 0
	  :for rect :in rects
	  :do (copy-into-rect (cobj:pointer-cobject
			       (cffi:mem-aptr ptr '(:struct sdl-rect) i)
			       'sdl-rect)
			      rect))
    (values ptr num-rects)))

#||
(setq $a (cffi:foreign-alloc '(:struct sdl-rect)))
(cobj:pointer-cobject $a 'sdl-rect)
(rects* (make-sdl-rect :x 1 :y 1 :w 10 :h 10))
||#

;;; The implementation of the SDL_rect.h methods.

(defun rect-empty (&rest rects)
  "Return T if the rectangle has no width or height."
  (every (lambda (rect)
	   (and (not (cffi:null-pointer-p (cobj:cobject-pointer rect)))
                 (or (<= (sdl-rect-w rect) 0)
                     (<= (sdl-rect-h rect) 0))))
         rects))

(defun %rect-equal (a b)
  "Return T if the two rectanges are valid and the slots are equal"
  (and (= (sdl-rect-x a) (sdl-rect-x b))
       (= (sdl-rect-y a) (sdl-rect-y b))
       (= (sdl-rect-w a) (sdl-rect-w b))
       (= (sdl-rect-h a) (sdl-rect-h b))))

(defun rect-equals (first-rect &rest rects)
  "Return T if the passed in SDL_Rect structures are valid and all slots are equal to each other."
  (dolist (rect rects)
    (when (not (%rect-equal first-rect rect))
      (return-from rect-equals nil)))
  t)

;; SDL_HasIntersection() => SDL_HasRectIntersection()

(defun has-intersect (first-rect &rest rects)
  "Return T if every SDL_Rect structure intersects every other SDL_Rect structure."
  (flet ((unique-pairs (list)
	   "Return all unique pair combinations of the list."
	   (mapcon (lambda (x)
		     (mapcar (lambda (y)
			       (list (car x) y))
			     (cdr x)))
		   list)))
    (loop :for (a b) :in (unique-pairs `(,first-rect ,@rects))
	  :do (unless (sdl-true-p (sdl-has-rect-intersection a b))
		(return-from has-intersect nil)))
    t))

;; SDL_IntersectRect() => SDL_GetRectIntersection()

(defun intersect-rect (first-rect &rest rects)
  "Return two values. The first one is T if the intersection of ALL rectangles results in a
non-empty intersection. The second value is the SDL_Rect of the intersection rectangle. If an empty
intersection is discovered, then NIL and an empty rectangle at the origin is returned. The second
value is always a newly allocated SDL_Rect structure."
  (let ((empty (make-rect 0 0 0 0))
        (intersect (copy-rect first-rect)))
    (dolist (rect rects)
      (unless (sdl-true-p (sdl-get-rect-intersection rect intersect intersect))
        (return-from intersect-rect (values nil empty))))
    (values t intersect)))

;; SDL_IntersectFRectAndLine() => SDL_GetRectAndLineIntersectionFloat()

(defun intersect-rect-and-line (rect x1 y1 x2 y2)
  "Returns five values where the first value is T if the coordinates of the line intersect RECT. The
remaining returned values represent the starting and ending coordinates of the line clipped to the
boundary of the rectangle."
  (cffi:with-foreign-objects ((x1pos :int)
			      (y1pos :int)
			      (x2pos :int)
			      (y2pos :int))
    (setf (cffi:mem-ref x1pos :int) x1)
    (setf (cffi:mem-ref y1pos :int) y1)
    (setf (cffi:mem-ref x2pos :int) x2)
    (setf (cffi:mem-ref y2pos :int) y2)
    (let ((intersected (sdl-true-p (%sdl-get-rect-and-line-intersection
				    (cobj:cobject-pointer rect)
				    x1pos y1pos x2pos y2pos))))
      (values intersected
	      (cffi:mem-ref x1pos :int)
	      (cffi:mem-ref y1pos :int)
	      (cffi:mem-ref x2pos :int)
	      (cffi:mem-ref y2pos :int)))))

;; SDL_UnionRect() => SDL_GetRectUnion(), returns bool

(defun union-rect (first-rect &rest rects)
  "Calculate and return the union of all rectangles passed in. The result will be one large
rectangle as a newly allocated SDL_Rect in which all others fit perfectly."
  (let ((union-rect (copy-rect first-rect)))
    (dolist (rect rects)
      (check-rc (sdl-get-rect-union rect union-rect union-rect)))
    union-rect))

;;; SDL_RectF

#||
(defmacro c-f-rect ((r) &body body)
  `(let ((,r sdl2-ffi:sdl-f-rect :from ,r))
     ,@body))

(defmacro c-f-rects ((&rest wrappers) &body body)
  (if wrappers
      `(c-f-rect (,(car wrappers))
         (c-f-rects (,@(cdr wrappers)) ,@body))
      `(progn ,@body)))

(define-struct-accessors (f-rect sdl2-ffi:sdl-f-rect)
  :x :y (width :w) (height :h))
||#

(defun make-f-rect (x y w h)
  (make-sdl-f-rect :x x :y y :w w :h h))

(defmethod print-object ((f-rect sdl-f-rect) stream)
  (print-unreadable-object (f-rect stream :type t :identity t)
    (format stream "x ~A y ~A w ~A h ~A" (sdl-f-rect-x f-rect)
	    (sdl-f-rect-y f-rect)
	    (sdl-f-rect-w f-rect)
	    (sdl-f-rect-h f-rect))))

(defun copy-f-rect (f-rect)
  "Allocate and return a new SDL_FRect and make its slots be equal to the passed in SDL_FRect."
  (make-f-rect (sdl-f-rect-x f-rect)
	       (sdl-f-rect-y f-rect)
	       (sdl-f-rect-w f-rect)
	       (sdl-f-rect-h f-rect)))

(defun copy-into-f-rect (dest-f-rect src-f-rect)
  "Copy the information from the SDL_FRect src-rect into the SDL_FRect dest-rect. Return the
dest-rect."
  (setf (sdl-f-rect-x dest-f-rect)  (sdl-f-rect-x src-f-rect))
  (setf (sdl-f-rect-y dest-f-rect)  (sdl-f-rect-y src-f-rect))
  (setf (sdl-f-rect-w dest-f-rect)  (sdl-f-rect-w src-f-rect))
  (setf (sdl-f-rect-h dest-f-rect)  (sdl-f-rect-h src-f-rect)))

(defun free-f-rect (f-rect)
  "Specifically free the SDL_FRect structure."
  ;; WHAT DO
  )

;; skip let-f-rects (bindings &body body)

;; used as a helper for with-f-rects
(defmacro %with-f-rect ((binding) &body body)
  (cond
    ((symbolp binding)
     `(let ((,binding (make-f-rect 0 0 0 0)))
        (unwind-protect (progn ,@body)
          (free-f-rect ,binding))))
    ((= (length binding) 5)
     `(let ((,(first binding) (make-f-rect ,@(cdr binding))))
        (unwind-protect (progn ,@body)
          (free-f-rect ,(first binding)))))
    (t
     (error "with-f-rect: Must have a binding of either a symbol or a symbol and 4 forms which are ~
x y w h of a rectangle"))))

(defmacro with-f-rects (bindings &body body)
  "A LET-like convenient bindings facility for SDL_FRect structures. Raw symbols are bound
to (make-f-rect 0 0 0 0).

  Example:

  (let ((a 1) (b 2) (c 3) (d 4))
    (with-f-rects (foo
                   (qux 5 10 15 20)
                   (bar (1+ a) b c (* d 10)))
       (list foo qux bar)))

  -> (#<SDL-FFI:SDL-F-RECT x 0 y 0 w 0 z 0>
      #<SDL-FFI:SDL-F-RECT x 5 y 10 w 15 h 20>
      #<SDL-FFI:SDL-F-RECT x 2 y 2 w 3 d 40>)"
  (if (null bindings)
      `(progn ,@body)
      `(%with-f-rect (,(car bindings))
         (with-f-rects ,(cdr bindings) ,@body))))

(defun f-rects* (&rest f-rects)
  "Return a pointer to SDL_FRect and the number of elements in it."
  (let* ((num-f-rects (length f-rects))
	 (ptr (cffi:foreign-alloc '(:struct sdl-f-rect) :count num-f-rects)))
    (loop :for i :from 0
	  :for rect :in f-rects
	  :do (copy-into-rect (cobj:pointer-cobject
			       (cffi:mem-aref ptr '(:struct sdl-f-rect) i)
			       'sdl-f-rect)
			      rect))
    (values ptr num-f-rects)))

(defun f-rect-empty (&rest f-rects)
  "Return T if the rectangle has no width or height."
  (every (lambda (f-rect)
	   (and (not (cffi:null-pointer-p (cobj:cobject-pointer f-rect)))
                 (or (<= (sdl-f-rect-w f-rect) 0.0)
                     (<= (sdl-f-rect-h f-rect) 0.0))))
         f-rects))

(defun %f-rect-equal (a b)
  "Return T if the two rectanges are valid and the slots are equal"
  (and (= (sdl-f-rect-x a) (sdl-f-rect-x b))
       (= (sdl-f-rect-y a) (sdl-f-rect-y b))
       (= (sdl-f-rect-w a) (sdl-f-rect-w b))
       (= (sdl-f-rect-h a) (sdl-f-rect-h b))))

(defun f-rect-equals (first-f-rect &rest f-rects)
  "Return T if the passed in SDL_FRect structures are valid and all slots are equal to each other."
  (dolist (f-rect f-rects)
    (when (not (%f-rect-equal first-f-rect f-rect))
      (return-from f-rect-equals nil)))
  t)

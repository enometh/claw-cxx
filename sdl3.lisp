(in-package "CLAW-CXX-SDL3-USER")
#+nil
(require 'trivial-channels)

;;; ----------------------------------------------------------------------
;;;
;;; sdl2.lisp
;;;

(define-condition sdl-error (error)
  ((string :initarg :string :initform nil :accessor sdl-error-string))
  (:report (lambda (c s)
             (with-slots (string) c
               (format s "SDL Error: ~A" string)))))

(define-condition sdl-rc-error (sdl-error)
  ((code :initarg :rc :initform nil :accessor sdl-error-code))
  (:report (lambda (c s)
             (with-slots (code string) c
               (format s "SDL Error (~A): ~A" code string)))))

(define-condition sdl-continue (condition) ())
(define-condition sdl-quit (condition) ())

(defun sdl-true-p (integer-bool)
  "Use this function to convert truth from a low level wrapped SDL function returning an SDL_true
into CL's boolean type system."
  (=  claw-cxx-sdl3::+true+ integer-bool))

(defvar *the-main-thread* nil)
(defvar *main-thread-channel* nil)
(defvar *main-thread* nil)
(defvar *lisp-message-event* nil)
(defvar *wakeup-event* nil)

;;; NAMING CONVENTION: check-<foo>
;;; If <foo> names a specific value (true, false, zero, null, etc),
;;; check-<foo> shall error `(when <foo> ...)`.  E.g., `(check-false
;;; x)` will *error* when `x` is false.
;;; If <foo> names something that can have an error state (like a
;;; return code), `(check-<foo> x)` shall error when `x` is in that
;;; state.

(defmacro check-zero (form)
  (alexandria:with-gensyms (rc)
    `(let ((,rc ,form))
       (when (zerop ,rc)
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro  check-rc (form)
  (alexandria:with-gensyms (rc)
    `(let ((,rc ,form))
       (unless ,rc 			;sdl3 returns bool
         (error 'sdl-rc-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-nullptr (form)
  (alexandria:with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (cffi:null-pointer-p (cobj:cobject-pointer ,wrapper))
           (error 'sdl-error :string (sdl-get-error))
           ,wrapper))))


(defmacro in-main-thread ((&key background no-event) &body b)
  (alexandria:with-gensyms (fun channel)
    `(let ((,fun (lambda () ,@b)))
       (if (or *main-thread-channel* *main-thread*)
           (if *main-thread*
               (funcall ,fun)
               ,(if background
                    `(progn
                       (trivial-channels:sendmsg *main-thread-channel* (cons ,fun nil))
                       (values))
                    `(let ((,channel (trivial-channels:make-channel)))
                       (trivial-channels:sendmsg *main-thread-channel* (cons ,fun ,channel))
                       ,(unless no-event
                          '(push-event *wakeup-event*))
                       (let ((result (trivial-channels:recvmsg ,channel)))
                         (etypecase result
                           (list (values-list result))
                           (error (error result)))))))
           (error "No main thread, did you call SDL_Init?")))))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defun handle-message (msg)
  (let ((fun (car msg))
        (chan (cdr msg))
        (condition))
    (handler-bind ((sdl-continue
                     (lambda (c)
                       (declare (ignore c))
                       (when chan (trivial-channels:sendmsg chan nil))
                       (return-from handle-message)))
                   (sdl-quit
                     (lambda (c)
                       (declare (ignore c))
                       (quit)
                       (return-from handle-message))))
      (handler-bind ((error (lambda (e) (setf condition e))))
        (if chan
            (trivial-channels:sendmsg chan (multiple-value-list (funcall fun)))
            (funcall fun))))))

(defun recv-and-handle-message ()
  (let ((msg (trivial-channels:recvmsg *main-thread-channel*)))
    (handle-message msg)))

(defun get-and-handle-messages ()
  (loop :as msg = (and *main-thread-channel*
                       (trivial-channels:getmsg *main-thread-channel*))
        :while msg :do
          (handle-message msg)))

(defmacro without-fp-traps (&body body)
  #+sbcl
  `(sb-int:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun sdl-main-thread ()
  (without-fp-traps
    (let ((*main-thread* (bt:current-thread)))
      (loop :while *main-thread-channel* :do
        (block loop-block
          (restart-bind ((continue (lambda (&optional v)
                                     (declare (ignore v))
                                     (signal 'sdl-continue))
                                   :report-function
                                   (lambda (stream)
                                     (format stream "Return to the SDL3 main loop.")))
                         (abort (lambda (&optional v)
                                  (declare (ignore v))
                                  (signal 'sdl-quit))
                                :report-function
                                (lambda (stream)
                                  (format stream "Abort, quitting SDL3 entirely."))))
            (recv-and-handle-message)))))))

(defun ensure-main-channel ()
  (unless *main-thread-channel*
    (setf *main-thread-channel* (trivial-channels:make-channel))))

#+nil
(defun make-this-thread-main (&optional function)
  "Designate the current thread as the SDL3 main thread. This function will not return until
`SDL3:QUIT` is handled. Users of this function will need to start other threads before this call, or
specify `FUNCTION`.

If `FUNCTION` is specified, it will be called when the main thread channel is ensured. This is like
calling `IN-MAIN-THREAD`, except it allows for a potentially single-threaded application. This
function does **not** return just because `FUNCTION` returns; it still requires `SDL3:QUIT` be
processed.

This does **not** call `SDL3:INIT` by itself. Do this either with `FUNCTION`, or from a separate
thread."
  (ensure-main-channel)
  (when (functionp function)
    (trivial-channels:sendmsg *main-thread-channel* (cons function nil)))
  (sdl-main-thread))

#||
(gethash 'sdl-init-flags cffi::*default-type-parsers*)
(eq (find-symbol "SDL-INIT-FLAGS" 'claw-cxx-sdl3)
    (find-symbol "SDL-INIT-FLAGS" 'claw-cxx-sdl3-user))
(eq 'sdl-init-flags
    (find-symbol "SDL-INIT-FLAGS" 'claw-cxx-sdl3-user))
(cffi:foreign-enum-value 'CLAW-CXX-SDL3:SDL-INIT-FLAGS :everything)
(cffi::ensure-parsed-base-type 'sdl-init-flags)
(cffi::parse-type 'sdl-init-flags)
(cffi:foreign-enum-keyword 'sdl-init-flags sdl-init-flags)
(cffi:foreign-enum-keyword 'claw-cxx-sdl3::sdl-init-flags 0)
(= (handle-sdl-init-flags '(:audio :video)) 46)
(= (handle-sdl-init-flags ':everything) 4294967295)
||#

(defun handle-sdl-init-flags (sdl-init-flags)
  (etypecase sdl-init-flags
    (integer sdl-init-flags)
    (null (handle-sdl-init-flags :everything))
    (symbol
     (if (eql sdl-init-flags :everything)
	 (- (expt 2 (* 8 (cffi:foreign-type-size :unsigned-int))) 1)
	 (let* ((name (format nil "+~:@(SDL-INIT-~A~)+" sdl-init-flags))
		(sym (find-symbol name :claw-cxx-sdl3)))
	   (assert sym nil "~A: ~A not found" sdl-init-flags name)
	   (symbol-value sym))))
    (cons (cond ((endp (cdr sdl-init-flags))
		 (assert (symbolp (car sdl-init-flags)))
		 (handle-sdl-init-flags (car sdl-init-flags)))
		(t (assert (symbolp (car sdl-init-flags)))
		   (+ (handle-sdl-init-flags (car sdl-init-flags))
		      (handle-sdl-init-flags (cdr sdl-init-flags))))))))


(defun init (&rest init-flags)
  "Initialize SDL3 with the specified subsystems. Initializes everything by default."
  (unless *wakeup-event*
    (setf *wakeup-event* (make-sdl-event)))
  (unless *main-thread-channel*
    (ensure-main-channel)

    ;; If we did not have a main-thread channel, make a default main thread.
    #-(and (or sbcl ccl) darwin)
    (setf *the-main-thread* (bt:make-thread #'sdl-main-thread :name "SDL3 Main Thread"))

    ;; On OSX, we need to run in the main thread; some implementations allow us to safely
    ;; do this. On other platforms (mainly GLX?), we just need to run in a dedicated thread.
    #+(and ccl darwin)
    (let ((thread (find 0 (ccl:all-processes) :key #'ccl:process-serial-number)))
      (setf *the-main-thread* thread)
      (ccl:process-interrupt thread #'sdl-main-thread)))
    #+(and sbcl darwin)
    (let ((thread (sb-thread:main-thread)))
      (setf *the-main-thread* thread)
      (when (not (eq thread (bt:current-thread)))
        (sb-thread:interrupt-thread thread #'sdl-main-thread)))

  (in-main-thread (:no-event t)
    ;; HACK! glutInit on OSX uses some magic undocumented API to correctly make the calling thread
    ;; the primary thread. This allows cl-sdl2 to actually work. Nothing else seemed to work at all
    ;; to be honest.
    #+(and ccl darwin)
    (cl-glut:init)
    (let ((init-flags (handle-sdl-init-flags init-flags)))
      (check-rc (sdl-init init-flags))
      (unless *lisp-message-event*
        (setf *lisp-message-event* (sdl-register-events 1)
	      (sdl-event-type *wakeup-event*) *lisp-message-event*)))))

(defun init* (flags)
  "Low-level function to initialize SDL3 with the supplied subsystems. Useful
   when not using cl-sdl3's threading mechanisms."
  (sdl-init (handle-sdl-init-flags flags)))

(defun was-init (&rest flags)
  (let ((ret (handle-sdl-init-flags flags)))
    (when ret
      (values (/= 0 (sdl-was-init ret)) t))))

(defun quit ()
  "Shuts down SDL3."
  (in-main-thread (:background t)
    (let ((mtc *main-thread-channel*))
      (sdl-quit)
      (setf *main-thread-channel* nil)
      (setf *lisp-message-event* nil)
      (when mtc (trivial-channels:sendmsg mtc nil))))
  #-(and sbcl darwin)
  (when (and *the-main-thread*
             (not (eq *the-main-thread* (bt:current-thread))))
    (handler-case
        (bt:join-thread *the-main-thread*)
      (error (e)
        (declare (ignore e))
        (setf *main-thread-channel* nil)))
    (setf *the-main-thread* nil))
  (when *the-main-thread*
    (setf *the-main-thread* nil)))

(defun quit* ()
  "Low-level function to quit SDL3. Useful when not using cl-sdl3's
   threading mechanisms."
  (sdl-quit))

(defmacro with-init ((&rest sdl-init-flags) &body body)
  `(progn
     (init ,@sdl-init-flags)
     (unwind-protect
          (in-main-thread () ,@body)
       (quit))))

(defun niy (message)
  (error "SDL3 Error: Construct Not Implemented Yet: ~A" message))

#+nil
(assert (=  (+ (* 1000000  +sdl-major-version+)
	       (* 1000  +sdl-minor-version+)
	       (* +sdl-micro-version+))))

(defun version ()
  (values-list
   (loop with ver = (sdl-get-version)
	 for x in '(1000000 1000 1) with num and den
	 do
	 (multiple-value-setq (num den)
	   (truncate ver x))
	 (setq ver den)
	 collect num)))

#+nil
(equalp (multiple-value-list (version))
	(multiple-value-list (version-wrapped)))

(defun version-wrapped ()
  (values +sdl-major-version+
          +sdl-minor-version+
	  +sdl-micro-version+
	  #+nil
          +sdl-patchlevel+))

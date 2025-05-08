(uiop:define-package :claw.util
  (:use :cl :alexandria)
  (:import-from :claw-utils
                #:common-prefix)
  (:export #:+known-platforms+
           #:+byte-size+

           #:with-evaluated-variables
           #:with-evaluated-lists
           #:find-path
           #:list-all-known-include-paths
           #:list-all-known-framework-paths
           #:default-c-name-to-lisp

           #:get-timestamp
           #:common-prefix

           #:parse-renaming-pipeline
           #:with-symbol-renaming
           #:c-name->lisp
           #:string+
           #:parse-infix

           #:with-local-cpu
           #:with-local-environment
           #:with-windows-environment
           #:local-environment
           #:local-platform

           #:with-temporary-directory

           #:remove-template-argument-string
           #:extract-template-argument-string
           #:split-template-argument-string-into-literals
           #:reformat-template-argument-string
           #:split-template-name-into-groups
           #:join-groups-into-template-name

           #:ignore-functions

           #:format-claw-timestamp-text
	   #:use-spec-package
           #:make-compiler-option #:make-compiler-options
           #:claw-cxx-defsystems
   ))
(uiop:define-package :claw.util.infix
  (:use))
(cl:in-package :claw.util)


(declaim (special *symbol-package*
                  *symbol-type*
                  *symbol-renaming-pipeline*
                  *hit-count*))


(define-constant +byte-size+ 8)

(define-constant +path-search-regex+
  "\\> search starts here:\\s*((.|\\s)*)?\\s*End of search list"
  :test #'equal)

(define-constant +stupid-darwin-framework-postfix+
  " (framework directory)"
  :test #'equal)


(define-constant +known-platforms+
    '("i686-pc-linux-gnu"
      "x86_64-pc-linux-gnu"
      "powerpc64-pc-linux-gnu"
      "powerpc64le-pc-linux-gnu"
      "i686-pc-windows-msvc"
      "x86_64-pc-windows-msvc"
      "i686-pc-windows-gnu"
      "x86_64-pc-windows-gnu"
      "i686-apple-darwin9"
      "x86_64-apple-darwin9"
      "i686-apple-darwin-gnu"
      "x86_64-apple-darwin-gnu"
      "i386-unknown-freebsd"
      "x86_64-unknown-freebsd"
      "i386-unknown-openbsd"
      "x86_64-unknown-openbsd"
      "arm-pc-linux-gnu")
  :test #'equal)


;;;
;;; TEMPLATE ARGUMENT/PARAMETER MANIPULATION
;;;
(defparameter *function-parameter-list-extractor* (ppcre:create-scanner "\\(.*\\("))

(defun remove-parameter-list-string (name)
  (ppcre:regex-replace-all *function-parameter-list-extractor* name ""))


(defun substring-trim (name start-idx end-idx)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq name start-idx end-idx)))


(defun split-template-name-into-groups (name)
  (labels ((%weird-char-p (idx)
             (and (< idx (length name))
                  (>= idx 0)
                  (char= (aref name idx) #\=)))
           (%extract (pos)
             (loop with result = nil
                   with len = (length name)
                   with last-end = pos
                   for idx from pos
                   when (= idx len)
                     do (unless (= last-end len)
                          (push (substring-trim name last-end len) result))
                        (return (values (nreverse result) len))
                   when (char= (aref name idx) #\,)
                     do (unless (= last-end idx)
                          (push (substring-trim name last-end idx) result)
                          (setf last-end (1+ idx)))
                   when (and (char= (aref name idx) #\>)
                             (not (%weird-char-p (1+ idx))))
                     do (unless (= last-end idx)
                          (push (substring-trim name last-end idx) result))
                        (return (values (nreverse result) (1+ idx)))
                   when (and (char= (aref name idx) #\<)
                             (not (%weird-char-p (1+ idx))))
                     do (when (> (- idx pos) 1)
                          (push (substring-trim name pos idx) result))
                        (multiple-value-bind (groups end)
                            (%extract (1+ idx))
                          (push groups result)
                          (setf idx (1- end)
                                last-end end)))))
    (%extract 0)))


(defun join-groups-into-template-name (groups)
  (format nil "~{~A~}"
          (loop for (group next) on groups
                collect (cond
                          ((listp group)
                           (format nil "<~A>" (join-groups-into-template-name group)))
                          ((listp next) group)
                          (t (format nil "~A," group))))))


(defun remove-template-argument-string (name)
  (let* ((groups (split-template-name-into-groups name))
         (last-group (first (last groups))))
    (join-groups-into-template-name (if (listp last-group)
                                        (butlast groups)
                                        groups))))


(defun extract-template-argument-string (name)
  (let ((group (first (last (split-template-name-into-groups name)))))
    (when (listp group)
      (join-groups-into-template-name (list group)))))


(defun split-template-argument-string-into-literals (name)
  (let ((name (subseq name 1 (1- (length name)))))
    (flet ((next-idx (current-idx)
             (let ((char (aref name current-idx)))
               (flet ((pos (closing-char)
                        (loop with depth = 1
                              for idx from (1+ current-idx) below (length name)
                              for current-char = (aref name idx)
                              when (char= current-char char)
                                do (incf depth)
                              when (char= current-char closing-char)
                                do (decf depth)
                              until (= depth 0)
                              finally (return idx))))
                 (switch (char :test #'char=)
                   (#\< (pos #\>))
                   (#\( (pos #\)))
                   (#\[ (pos #\]))
                   (t (1+ current-idx)))))))
      (loop with args = nil
            with start-idx = 0
            with current-idx = 0
            with len = (length name)
            while (< current-idx len)
            for char = (aref name current-idx)
            if (char= #\, char)
              do (push (substring-trim name start-idx current-idx) args)
                 (incf current-idx)
                 (setf start-idx current-idx)
            else
              do (setf current-idx (next-idx current-idx))
            finally (return (nreverse (list* (substring-trim name start-idx current-idx) args)))))))


(defun reformat-template-argument-string (name)
  (flet ((trim-around (name char &optional new)
           (ppcre:regex-replace-all (format nil "\\s*~A\\s*" char) name (or new char))))
    (let* ((name (ppcre:regex-replace-all "\\s+" name " "))
           (name (trim-around name ","))
           (name (trim-around name "\\(" "("))
           (name (trim-around name "\\)" ")"))
           (name (trim-around name "\\[" "["))
           (name (trim-around name "\\]" "]"))
           (name (trim-around name "\\<" "<"))
           (name (trim-around name "\\>" ">")))
      name)))

;;;
;;; PATH SEARCH
;;;
(defun %find-asdf-component-child (component child)
  #+(and asdf (not mk-defsystem))
  (or (asdf:find-component component child)
      (error "Component ~S child not found: ~S"
             (asdf:component-pathname component) child))
  #+mk-defsystem
  (or (mk::find-component component child)
      (error "Component ~S child not found: ~S"
             (mk::component-source-pathname component) child)))


(defun asdf-path (component &rest path)
  (if (rest path)
      (apply #'asdf-path (%find-asdf-component-child component (first path)) (rest path))
      (etypecase (first path)
        ((or string pathname)
         (merge-pathnames (first path)
			  #+(and asdf (not mk-defsystem))
			  (asdf:component-pathname component)
			  #+mk-defsystem
			  (or (case (mk::component-type component)
				((:system :defsystem :subsystem)
				 (mk::component-root-dir component :source))
				(otherwise
				 (mk::component-full-pathname component :source)))
			      *default-pathname-defaults*)
			  nil))
        (null #+(and asdf (not mk-defsystem))
	      (asdf:component-pathname component)
	      #+mk-defsystem
	      (or (case (mk::component-type component)
		    ((:system :defsystem :subsystem)
		     (mk::component-root-dir component :source))
		    (otherwise
		     (mk::component-full-pathname component :source)))
		  *default-pathname-defaults*))
        (t (asdf-path (%find-asdf-component-child component (first path)))))))


(defun path-or-asdf (form)
  (etypecase form
    ((or string pathname) form)
    (list (apply #'asdf-path (#+(and asdf (not mk-defsystem))
				asdf:find-system
				#+mk-defsystem
				mk::find-system
				(first form) :error)
		 (rest form)))))

(defun ensure-version-nil (pathname)
  #+sbcl
  (make-pathname :defaults pathname :version nil)
  #-sbcl
  pathname)


(defun find-path (relative &key system path)
  (let ((relative (ensure-list relative)))
    (if (or path (not system))
        (flet ((%relative (base rel)
		 (let ((base (uiop:ensure-directory-pathname base)))
		   (uiop:merge-pathnames*  (ensure-version-nil
					    (typecase rel
					      (pathname rel)
					      (t (string rel))))
					   base))))
          (reduce #'%relative relative :initial-value (ensure-version-nil
						       (or path
                                                          *default-pathname-defaults*))))
        (path-or-asdf (append (list system) relative)))))

;;;
;;; INCLUDE PATHS
;;;
(defun trim-terminal-output (output)
  (string-trim '(#\Tab #\Space #\Newline #\Return) output))


(defun convert-msys-path (path)
  (trim-terminal-output
   (uiop:run-program (format nil "cygpath -m '~A'" (namestring path))
                     :output :string :error-output :string)))


(defun mingwp ()
  (and (featurep :windows)
       (search "MINGW" (trim-terminal-output
                        (uiop:run-program "echo %MSYSTEM%" :force-shell t :output :string :error-output :string)))
       t))


(defun dump-include-paths (lang &optional (executable "gcc"))
  (handler-case
      (let* ((command (format nil "echo | ~A -x~A ~@[~A~] -E -v -"
                              executable lang
                              (when (and (string= lang "c++")
                                         (starts-with-subseq "clang" executable))
                                "-stdlib=libc++")))
             (paths (with-output-to-string (out)
                      (uiop:run-program command
                                        :output out :error-output out)))
             (bounds (third (multiple-value-list (ppcre:scan +path-search-regex+ paths)))))
        (when bounds
          (let ((paths (ppcre:split "(\\r|\\n)+\\s*" (subseq paths (aref bounds 0) (aref bounds 1)))))
            (if (mingwp)
                (mapcar #'convert-msys-path paths)
                paths))))
    (t (c)
      (warn "Failed to obtain `~A` search paths for language ~A: ~A" executable lang c)
      nil)))


(defun %darwin-framework-path-p (path)
  (ends-with-subseq +stupid-darwin-framework-postfix+ path :test #'equal))


(defun list-all-known-paths ()
  (let ((lc-all (uiop:getenv "LC_ALL")))
    (setf (uiop:getenv "LC_ALL") "C")
    (unwind-protect
         (remove-duplicates
          (append (unless (emptyp (dump-gcc-version "clang"))
                    (dump-include-paths "c" "clang"))
                  (unless (emptyp (dump-gcc-version "clang++"))
                    (dump-include-paths "c++" "clang++"))
                  (unless (emptyp (dump-gcc-version))
                    (append (dump-include-paths "c")
                            (dump-include-paths "c++")))
                  (unless (emptyp (dump-gcc-version "x86_64-w64-mingw32-gcc"))
                    (dump-include-paths "c" "x86_64-w64-mingw32-gcc"))
                  (unless (emptyp (dump-gcc-version "x86_64-w64-mingw32-g++"))
                    (dump-include-paths "c++" "x86_64-w64-mingw32-g++")))
          :test #'equal
          :from-end t)
      (when lc-all
        (setf (uiop:getenv "LC_ALL") lc-all)))))


(defun list-all-known-include-paths ()
  (remove-if #'%darwin-framework-path-p (list-all-known-paths)))


(defun list-all-known-framework-paths ()
  (flet ((cut-darwin-postfix (path)
           (subseq path 0 (- (length path) (length +stupid-darwin-framework-postfix+)))))
    (mapcar #'cut-darwin-postfix
            (remove-if (complement #'%darwin-framework-path-p) (list-all-known-paths)))))


(defun dump-gcc-version (&optional (executable "gcc"))
  (handler-case
      (trim-terminal-output
       (with-output-to-string (out)
         (uiop:run-program (format nil "~A -dumpversion" executable) :output out)))
    (t () "")))

;;;
;;; EVALUATION
;;;
(defmacro with-evaluated-lists ((&rest bindings) &body body)
  (let ((rebindings (loop for binding in bindings
                          collect (destructuring-bind (name &optional list)
                                      (ensure-list binding)
                                    `(,name (eval `(list ,@,(or list name))))))))
    `(let (,@rebindings)
       ,@body)))


(defmacro with-evaluated-variables ((&rest bindings) &body body)
  (let ((rebindings (loop for binding in bindings
                          collect (destructuring-bind (name &optional value)
                                      (ensure-list binding)
                                    `(,name (eval (first ,(or value name))))))))
    `(let (,@rebindings)
       ,@body)))

;;;
;;; PLATFORM
;;;
(defvar *local-os* nil)
(defvar *local-environment* nil)
(defvar *windows-environment* nil)
(defvar *local-cpu* nil)


(defmacro with-local-cpu ((cpu) &body body)
  `(let ((*local-cpu* ,cpu))
     ,@body))


(defmacro with-local-environment ((env) &body body)
  `(let ((*local-environment* ,env))
     ,@body))


(defmacro with-windows-environment ((env) &body body)
  `(let ((*windows-environment* ,env))
     ,@body))


(defun local-cpu ()
  (or *local-cpu*
      #+x86-64 "x86_64"
      #+(and (not (or x86-64 freebsd)) x86) "i686"
      #+(and (not x86-64) x86 freebsd) "i386"
      #+(and ppc64 big-endian) "powerpc64"
      #+(and ppc64 little-endian) "powerpc64le"
      #+arm "arm"))


(defun local-vendor ()
  #+(or linux windows) "pc"
  #+darwin "apple"
  #+(not (or linux windows darwin)) "unknown")


(defun local-os ()
  (or *local-os*
      #+linux "linux"
      #+windows "windows"
      #+darwin "darwin"
      #+freebsd "freebsd"
      #+openbsd "openbsd"
      #-(or linux windows darwin freebsd openbsd) (error "Unknown operating system")))


(defun local-environment (os)
  (or (and (equal "windows" os)
           *windows-environment*)
      *local-environment* "gnu"))


(defun local-platform ()
  (let ((os (local-os)))
    (format nil "~{~A~^-~}" (remove-if #'null (list (local-cpu)
                                                    (local-vendor)
                                                    os
                                                    (local-environment os))))))


(defun default-c-name-to-lisp (string &optional (package *package*))
  (let* ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2"))
         (string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2"))
         (string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2"))
         (string (ppcre:regex-replace-all "\\:\\:|\\s" string "+"))
         (string (ppcre:regex-replace-all "," string "+"))
         (string (if (ppcre:all-matches "^(:_|_)" string)
                     (let ((position (position #\_ string :test (complement #'equal))))
                       (nsubstitute #\% #\_ string :end position))
                     string))
         (string (nsubstitute #\- #\_ string)))
    (format-symbol package "~A" (uiop:standard-case-symbol-name string))))


;;;
;;; RENAMING
;;;
(defun make-scanners (list)
  (flet ((%to-scanner (regex-action)
           (cons (ppcre:create-scanner (car regex-action)) (cdr regex-action))))
    (mapcar #'%to-scanner list)))


(defmacro with-symbol-renaming ((in-package renaming-pipeline) &body body)
  `(let ((*symbol-renaming-pipeline* (make-scanners ,renaming-pipeline))
         (*symbol-package* ,in-package))
     ,@body))


(defun pipeline-rename (name)
  (loop with *hit-count* = 0
        with string = (format nil "~A" name)
        for (scanner-regex . scanner-action) in *symbol-renaming-pipeline*
        when (ppcre:scan-to-strings scanner-regex string)
          do (setf string (funcall scanner-action string)
                   *hit-count* (1+ *hit-count*))
        finally (return string)))


(defun c-name->lisp (name &optional type)
  (when name
    (let* ((*symbol-package* (or *symbol-package* *package*))
           (*symbol-type* type)
           (name (pipeline-rename name)))
      (default-c-name-to-lisp name (or *symbol-package* *package*)))))


(defun %%by-removing-prefix (prefix)
  (cons (format nil "^~A.+" prefix)
        (lambda (name)
          (subseq name (length prefix)))))


(defun %%by-removing-postfix (postfix)
  (cons (format nil "^.+~A$" postfix)
        (lambda (name)
          (subseq name 0 (- (length name) (length postfix))))))


(defun %by-removing-prefixes (&rest prefixes)
  (flet ((by-prefix-length (this-prefix that-prefix)
            (> (length this-prefix)
               (length that-prefix))))
    (mapcar #'%%by-removing-prefix (stable-sort prefixes #'by-prefix-length))))


(defun by-removing-prefixes (configuration)
  `(%by-removing-prefixes ,@configuration))


(defun %by-removing-postfixes (&rest prefixes)
  (flet ((by-postfix-length (this-prefix that-prefix)
           (> (length this-prefix)
              (length that-prefix))))
    (mapcar #'%%by-removing-postfix (stable-sort prefixes #'by-postfix-length))))


(defun by-removing-postfixes (configuration)
  `(%by-removing-postfixes ,@configuration))


(defun %by-changing (from to)
  (list (cons (format nil "^~A$" from)
              (lambda (name) (declare (ignore name)) (string to)))))


(defun by-changing (configuration)
  `(%by-changing ,@configuration))


(defun %by-changing-postfix (from to)
  (list (cons (format nil "~A$" from)
              (lambda (name)
                (string+ (subseq name 0 (- (length name) (length from))) to)))))


(defun by-changing-postfix (configuration)
  `(%by-changing-postfix ,@configuration))


(defun %switch-package (package)
  (list (cons ".*" (lambda (name)
                     (setf *symbol-package* package)
                     name))))


(defun switch-package (new-package)
  `(%switch-package ',(first new-package)))


(defun %except-for (types &rest pipelines)
  (list (cons ".*" (lambda (name)
                     (if (member *symbol-type* types :test #'eq)
                         name
                         (apply-pipeline pipelines name))))))


(defun except-for (configuration)
  (multiple-value-bind (types pipelines)
      (loop for (type . rest) on configuration
            while (keywordp type)
            collect type into types
            finally (return (values types (list* type rest))))
    `(%except-for ',types ,@(collect-renaming-pipelines pipelines))))


(defun %by-replacing (regex replacement)
  (list (cons regex (lambda (name)
                      (ppcre:regex-replace-all regex name replacement)))))


(defun by-replacing (configuration)
  (destructuring-bind (regex replacement) configuration
   `(%by-replacing ,regex ,replacement)))


(defun %only-for (types &rest pipelines)
  (list (cons ".*" (lambda (name)
                     (if (member *symbol-type* types :test #'eq)
                         (apply-pipeline pipelines name)
                         name)))))


(defun only-for (configuration)
  (multiple-value-bind (types pipelines)
      (loop for (type . rest) on configuration
            while (keywordp type)
            collect type into types
            finally (return (values types (list* type rest))))
    `(%only-for ',types ,@(collect-renaming-pipelines pipelines))))


(defun apply-pipeline (processors name)
  (let ((*symbol-renaming-pipeline* (reduce #'append processors)))
    (pipeline-rename name)))


(defun %in-pipeline (&rest processors)
  (list (cons ".*" (lambda (name)
                     (apply-pipeline processors name)))))


(defun in-pipeline (configuration)
  `(%in-pipeline ,@(collect-renaming-pipelines configuration)))


(defun %by-removing-complex-prefix (regex symbols-to-cut)
  (list (cons regex (lambda (name) (subseq name symbols-to-cut)))))


(defun by-removing-complex-prefix (configuration)
  `(%by-removing-complex-prefix ,@configuration))


(defun %by-prepending (prefix)
  (list (cons ".*" (lambda (name) (concatenate 'string prefix name)))))


(defun by-prepending (configuration)
  `(%by-prepending ,@configuration))


(defun %if-none-matched (&rest processors)
  (list (cons ".*" (lambda (name)
                     (if (zerop *hit-count*)
                         (apply-pipeline processors name)
                         name)))))


(defun if-none-matched (configuration)
  `(%if-none-matched ,@(collect-renaming-pipelines configuration)))


(defun collect-renaming-pipelines (configuration)
  (loop for description in configuration
        collect (parse-renaming-pipeline description)))


(defun parse-renaming-pipeline (description)
  (when-let ((descriptor (first description)))
    (funcall
     (eswitch (descriptor :test #'string=)
       ('in-pipeline #'in-pipeline)
       ('by-changing #'by-changing)
       ('by-changing-postfix #'by-changing-postfix)
       ('by-replacing #'by-replacing)
       ('by-removing-prefixes #'by-removing-prefixes)
       ('by-removing-postfixes #'by-removing-postfixes)
       ('by-removing-complex-prefix #'by-removing-complex-prefix)
       ('by-prepending #'by-prepending)
       ('switch-package #'switch-package)
       ('if-none-matched #'if-none-matched)
       ('except-for #'except-for)
       ('only-for #'only-for))
     (rest description))))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun date (&key (stream *standard-output*)
	     (utime (get-universal-time) utime-supplied-p) tz
	     uutime)
  (when uutime
    (when utime-supplied-p (warn "ignoring UTIME using UUTIME."))
    (setq utime (+ +unix-epoch+ uutime)))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (if tz
	  (decode-universal-time utime tz)
	  (decode-universal-time utime))
    (when daylight-p (decf zone))	; check
    (format stream "~a ~a ~2,' d ~2,'0d:~2,'0d:~2,'0d ~4d ~?"
	    (ecase day
	      (0 "Mon") (1 "Tue") (2 "Wed") (3 "Thu") (4 "Fri")
	      (5 "Sat") (6 "Sun"))
	    (ecase month
	      (1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May") (6 "Jun")
	      (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct") (11 "Nov") (12 "Dec"))
	    date hour minute second year
	    "~:[+~;-~]~2,'0d~2,'0d"
	    (multiple-value-bind (hour min) (truncate zone 1)
	      (list (plusp zone) (abs hour) (* 60 (abs min)))))))

;;;
;;; VARIOUS
;;;
(defun get-timestamp ()
  (date :stream nil))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))


(defun parse-infix (string &key (case :preserve))
  (unless (emptyp string)
    (handler-case
        (let ((*readtable* (named-readtables:find-readtable 'claw-infix:infix))
              (*package* (find-package :claw.util.infix)))
          (claw-infix:with-reader-case (case)
            (claw-infix:string->prefix string)))
      (serious-condition (e)
        (warn "Failed to parse infix expression: ~A~%~A" e string)
        string))))


(defmacro with-temporary-directory ((&key pathname) &body body)
  (with-gensyms (tmp-file tmp-dir)
    `(uiop:with-temporary-file (:pathname ,tmp-file)
       (let* ((,tmp-dir (merge-pathnames (format nil "~A.dir/" (pathname-name ,tmp-file))
                                         (uiop:pathname-directory-pathname ,tmp-file)))
              ,@(when pathname
                  `((,pathname ,tmp-dir))))
         (unwind-protect
              (progn
                (ensure-directories-exist ,tmp-dir)
                ,@body)
           (uiop:delete-directory-tree ,tmp-dir :validate (constantly t)))))))



(defparameter *include-archives* t)
(defparameter *include-objects* nil)


(defun repack-blob-archives (target &rest libs)
  (labels ((expand-dirs (file-o-dir pattern)
             (if (uiop:directory-pathname-p file-o-dir)
                 (when *include-archives*
                   (uiop:directory-files file-o-dir pattern))
                 file-o-dir))
           (expand-dirs-with-archives (file-o-dir)
             (expand-dirs file-o-dir "**/*.a"))
           (expand-dirs-with-objects (file-o-dir)
             (expand-dirs file-o-dir "**/*.o")))
    (with-temporary-directory (:pathname path)
      (loop for lib in (flatten (mapcar #'expand-dirs-with-archives libs))
            for lib-native = (uiop:native-namestring lib)
            for lib-tmp-dir = (merge-pathnames
                               (uiop:enough-pathname (uiop:ensure-directory-pathname lib) "/")
                               path)
            do (ensure-directories-exist lib-tmp-dir)
               (uiop:with-current-directory (lib-tmp-dir)
                 (uiop:run-program `("ar" "x" ,lib-native)))
            collect lib-tmp-dir)
      (let ((objects (remove-duplicates
                      (mapcar #'uiop:native-namestring
                              (append
                               (when *include-objects*
                                 (flatten (mapcar #'expand-dirs-with-objects libs)))
                               (uiop:directory-files path "**/*.o")))
                      :test #'string=)))
        (uiop:delete-file-if-exists target)
        (uiop:run-program `("ar" "rcs" ,(uiop:native-namestring target) ,@objects))
        objects))))


(defun format-claw-timestamp-text (&optional stream)
  (flet ((%format-claw-timestamp-text (stream)
           (format stream ";; Generated by :claw at ")
	   (date :stream stream)))
    (if stream
        (%format-claw-timestamp-text stream)
        (with-output-to-string (stream)
          (%format-claw-timestamp-text stream)))))

(defun use-spec-package (spec-package &optional (package *package*)
			 &key dry-run-p)
  "Access all exported symbols in SPEC-PACKAGE via PACKAGE.
Conflicts are avoided by SHADOWING-IMPORT-ing the already accessible
symbols in PACKAGE (which would conflict) first, before
USE-PACKAGE-ing the SPEC-PACKAGE.  Returns the list of symbols which
have to be accessed explictly (i.e. via SPEC-PACKAGE::SYM)."
  (labels ((get-exports (spec-package)
	     (loop for sym being each external-symbol of spec-package
		   when (eql (symbol-package sym) spec-package)
		   collect sym))
	   (get-conflicts (spec-package &optional (package *package*))
	     (loop for ext-sym in (get-exports spec-package)
		   for nam = (symbol-name ext-sym)
		   for (sym status) = (multiple-value-list
				       (find-symbol nam package))
;; ;madhu 250402 wtf was this assert about?
;;		   do (assert (not (eql (symbol-name sym) spec-package)))
		   if (and status (not (eql sym ext-sym)))
		   collect sym)))
    (setq spec-package (find-package spec-package))
    (setq package (find-package package))
    (unless dry-run-p
      (unuse-package spec-package package))
    (let ((conflicts (get-conflicts spec-package package)))
      (unless dry-run-p
	(shadowing-import conflicts package)
	(use-package spec-package package))
      (mapcar (lambda (x)
		(let ((sym (find-symbol (symbol-name x) spec-package)))
		  (assert (eql (symbol-package sym) spec-package))
		  sym))
	      conflicts))))

;; claw:generate-wrapper generates a bogus asd but we presently cough
;; up defsystems "by hand". see claw-cxx-defsystems.

;;  1. wrap up the bindings boilerplate for x86 systems used in
;;  claw-cxx-defwrapper

;; all MAKE-* functions return lisp forms.

(defstruct (if-feature (:print-function print-feature-hack))
  feature form)

(defstruct (if-not-feature (:include if-feature)))

(defun print-feature-hack (obj stream depth)
  (declare (ignore depth))
  (if (not (if-feature-feature obj))
      (print-unreadable-object (obj stream :type t :identity t))
      (format stream "#~C:~A ~S"
	      (etypecase obj
		(if-not-feature #\-)
		(if-feature #\+))
	      (if-feature-feature obj)
	      (if-feature-form obj))))

(defun make-bindings-module ()
  `(:module "bindings"
    :components
    ((:file "x86_64-pc-linux-gnu"
      :if-feature
      (:and :x86-64 :linux)
      #+lispworks-personal-edition
      ,@(list
	 (make-if-feature :feature :lispworks-personal-edition
			  :form :load-only)
	 (make-if-feature :feature :lispworks-personal-edition
			  :form t)))
     (:file "i686-pc-linux-gnu"
      :if-feature (:and :x86 :linux)))))

#+nil
(make-bindings-module)

;; handle `compiler-options' for the `:claw-cxx-adapter' language.

;; default compiler-options: these can be a string or a list of
;; strings. the user can define these in his own package and supply
;; them to make-compiler-options with the corresponding keywords.  the
;; defaults here are used by make-compiler-options if not overridden.

(defvar $default-cflags '("-O0"  "-ggdb"))
(defvar $default-ldflags '("-Wl,-O1 -Wl,--as-needed" "-ldl"))
(defvar $system-includes-paths nil)
(defvar $includes-paths nil)
(defvar $link-libs nil)
(defvar $link-libs-paths nil)

;; some ob utils that belong elsewhere
(defun ensure-list-args (x)
  (cond ((consp x)
	 (cond ((and (endp (cdr x))
		     (consp (car x)))
		;; fudge (&rest args) with (&optional args)
		(car x))
	       ((null (car x)) nil)
	       (t x)))
	((null x) nil)
	(t (list x))))

(defun ensure-string-args (x)
  (if (stringp x)
      x
      (format nil "~{~A~^ ~}" (ensure-list-args x))))

(defun make-compiler-option (switch &rest values)
  (let ((args (ensure-list-args values)))
    (when args
      (let ((fmt (format nil  "~~{~A ~~A~~^ ~~}" switch)))
	(format nil fmt args)))))

#||
(let (($system-includes-paths '("/usr/local/include" "/tmp/include")))
  (make-compiler-option "-isystem" $system-includes-paths))
(make-compiler-option "-L" "/debian/usr/lib/x86_64-linux-gnu/")
(user::map-concatenate
		  'string 'identity
	 (list nil nil nil)
	 " ")
(defvar $bar 'bar)
(defun foo (&key ((:bar $bar) $bar)) $bar)
(foo)
(foo :bar 'barf)
(make-compiler-options)
||#

(defun make-compiler-options
    (&key
     ((:default-cflags $default-cflags) $default-cflags)
     ((:system-includes-paths $system-includes-paths) $system-includes-paths)
     ((:default-ldflags $default-ldflags) $default-ldflags)
     ((:link-libs $link-libs) $link-libs)
     ((:link-libs-paths $link-libs-paths) $link-libs-paths)
     ((:include-paths $includes-paths) $includes-paths))
  "This returns a list (:cflags \"cflags-string\" :ldflags
\"ldflags-string\") which can be supplied as the `:compiler-options'
property to the `:claw-cxx-adapter' language :file defsystem component."
  (list :cflags (user::map-concatenate
		 'string 'identity
		 (delete nil
			 (list (ensure-string-args $default-cflags)
			       (make-compiler-option
				"-isystem" $system-includes-paths)
			       (make-compiler-option "-I" $includes-paths)))
		 " ")
	:ldflags (user::map-concatenate
		  'string 'identity
		  (delete nil
			  (list (ensure-string-args $default-ldflags)
				(make-compiler-option "-l" $link-libs)
				(make-compiler-option "-L" $link-libs-paths)))
		  " ")))

;;  2. wrap up the c-adapter boilerplate for x86 systems used in
;;  claw-cxx-defwrapper.

(defun make-adapter-module (&optional compiler-options)
  `(:module "lib"
    :components
    ((:file "adapter.x86_64-pc-linux-gnu"
      :source-extension "c"
      :if-feature (:and :x86-64 :linux)
      :language :claw-cxx-adapter
      :compiler-options
      ,@(if compiler-options
	    `(,compiler-options)
	    `(,(make-compiler-options)))))))

(defun make-defsystem-boiler
    (name &key source-pathname binary-pathname depends-on
     (source-extension "lisp")
     components)
  `(mk:defsystem ,name
     ,@(and source-pathname `(:source-pathname ,source-pathname))
     ,@(and binary-pathname `(:binary-pathname ,binary-pathname))
     ,@(and source-extension `(:source-extension ,source-extension))
     ,@(and depends-on `(:depends-on ,depends-on))
     ,@(and components `(:components ,components))))

;; wrap up the boilerplates to generate separate systems 1) for the
;; generated bindings and 2) for the dll. override the location for
;; the object files generated when compiling the shared object with
;; :dll-pathname

(defun make-defsystems (name &key source-pathname binary-pathname
			dll-pathname
			compiler-options
			(generate-adapter-p t))
  (let* ((system-name (string-downcase name))
	 (dll-system-name (concatenate 'string system-name ".library"))
	 (bindings-system-name (concatenate 'string system-name ".bindings")))
    `(progn
       ,(make-defsystem-boiler
	 bindings-system-name
	 :source-pathname source-pathname
	 :binary-pathname binary-pathname
	 :source-extension "lisp"
	 :depends-on '("uiop" "cffi")
	 :components `((:module "gen"
			:components
			(,(make-bindings-module)))))
       ,@(and generate-adapter-p
	      `(,(make-defsystem-boiler
	       dll-system-name
	       :source-pathname source-pathname
	       :binary-pathname (or dll-pathname binary-pathname)
	       :source-extension "lisp"
	       :depends-on '("uiop" "cffi")
	       :components
	       `((:module "gen"
		  :components (,(make-adapter-module
				 compiler-options))))))))))

(defvar $claw-cxx-defsystems-source-form nil
  "Remember source form of the last call to `claw-cxx-defsystems'")

(defmacro claw-cxx-defsystems (name &key source-pathname binary-pathname
			       dll-pathname
			       compiler-options
			       (generate-adapter-p t))
  "CLAW-CXX-DEFSYSTEMS defines the XXX.bindings and
XXX.library system defititions which can be used independently of
claw.  These are used to compile and load the generated lisp and the
generated c-adapter bindings that are created by claw-wrapper.  The
adapter is generated only if generate-adapter-p was supplied.

NOte that compiling the adapter sources to a sharedlib via MK:OOS and
loading the shared lib via MK:OOS requires CLAW-CXX/UTIL system to be
loaded first.

The most recently defined source forms defined by this macro can be
retrieved with LAST-BINDINGS-SOURCE and LAST-LIBRARY-SOURCE.  The last
caller can use these to persist the defsystem definitions to disk if
he so desires."
  (setq $claw-cxx-defsystems-source-form
	(make-defsystems name :source-pathname source-pathname
			 :binary-pathname binary-pathname
			 :dll-pathname dll-pathname
			 :compiler-options compiler-options
			 :generate-adapter-p generate-adapter-p)))

(defun last-bindings-source ()
  "Return the (mk:defsystem xxx.bindings ...) source form which was defined by the last call to claw-cxx-defsystems."
  (cadr $claw-cxx-defsystems-source-form))

(defun last-library-source ()
  "Return the (mk:defsystem xxx.library ...) source form which was defined by the last call to claw-cxx-defsystems. if it exists."
  (caddr $claw-cxx-defsystems-source-form))

;; claw-cxx-defsystem is now bogus, useless and unexported. it does
;; not allow the dll dest path to be shared between different lisp
;; compilers. the adapters may need to be compiled before the bindings
;; can be loaded which is a chicken and egg problem. besides the user
;; surely wants to use the name of this system for the some other
;; system which generates the bindings and adapters.

(defmacro claw-cxx-defsystem (name &key source-pathname binary-pathname
			      compiler-options (generate-adapter-p t))
  (make-defsystem-boiler
   name
   :source-pathname source-pathname
   :binary-pathname binary-pathname
   :source-extension "lisp"
   :depends-on '("uiop" "cffi"  "claw-cxx/util")
   :components `((:module "gen"
		  :components
		  (,(make-bindings-module)
		    ,@(and generate-adapter-p
			   `(,(make-adapter-module compiler-options))))))))

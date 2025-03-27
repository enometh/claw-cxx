;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Mar 27 6:33:35 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; link extraction
;;;
(in-package "CL-CLAW-MUPDF")

#||
(fz-drop-context $ctx)
(defvar $ctx (fz-new-context-imp (cffi:null-pointer) (cffi:null-pointer) +fz-store-default+ +fz-version+))
(fz-register-document-handler $ctx (cffi::foreign-symbol-pointer "pdf_document_handler"))

(fz-drop-document $doc)
(defvar $doc (fz-open-document $ctx "/dev/shm/1.pdf"))
(fz-count-pages $ctx $doc)

(fz-drop-page $ctx $page)
(defvar $page (fz-load-page $ctx $doc 130))

(cffi:foreign-free $stext-opts)
(defvar $stext-opts (cffi:foreign-alloc 'fz-stext-options))
(cffi:foreign-funcall "memset"
    :pointer $stext-opts
    :int 0
    :size (cffi:foreign-type-size 'fz-stext-options)
    :pointer)
(setf (cffi:foreign-slot-value $stext-opts 'fz-stext-options 'flags)
       (logior +FZ-STEXT-PRESERVE-LIGATURES+
	      +FZ-STEXT-PRESERVE-WHITESPACE+
	      +FZ-STEXT-MEDIABOX-CLIP+))

(fz-drop-stext-page $ctx $stext-page)
(defvar $stext-page (fz-new-stext-page-from-page $ctx $page $stext-opts))

(let ((nblocks 0))
  (process-stext-page $ctx $stext-page (lambda (ctx block-ptr)
					 (incf nblocks)))
  nblocks)

(defvar $b (cffi:foreign-slot-value $stext-page 'fz-stext-page 'first-block))
(= (cffi:foreign-slot-value $b 'fz-stext-block 'claw-cxx-mupdf::type)
   +fz-stext-block-text+)

#+nil
(stext-block-first-line $b)

(let ((nlines 0))
  (process-stext-block
   $ctx $b
   (lambda (ctx line-ptr)
     (incf nlines)))
  nlines)
||#

(defun intersects (a b)
  "Check if intersection of rectangles a and b  is not empty."
  (cond ((or (/= 0 (fz-is-empty-rect a))
	     (/= 0 (fz-is-infinite-rect a))
	     (/= 0 (fz-is-empty-rect b))
	     (/= 0 (fz-is-infinite-rect b)))
	 nil)
	(t (cffi:with-foreign-object (c 'fz-rect)
	     (fz-make-rect c (cffi:foreign-slot-value a 'fz-rect 'x0)
			   (cffi:foreign-slot-value a 'fz-rect 'y0)
			   (cffi:foreign-slot-value a 'fz-rect 'x1)
			   (cffi:foreign-slot-value a 'fz-rect 'y1))
	     (fz-intersect-rect c a b)
	     (cond ((/= 0 (fz-is-empty-rect c))
		    nil)
		   (t t))))))

#||
(setq $r1 (cffi:foreign-alloc 'fz-rect))
(setq $r2 (cffi:foreign-alloc 'fz-rect))
(setq $r3 (cffi:foreign-alloc 'fz-rect))

(defun dump-rect (a)
  (typecase a
    (cons (list (getf a 'x0) (getf a 'y0)
		(getf a 'x1) (getf a 'y1)))
    (t (list (cffi:foreign-slot-value a 'fz-rect 'x0)
	     (cffi:foreign-slot-value a 'fz-rect 'y0)
	     (cffi:foreign-slot-value a 'fz-rect 'x1)
	     (cffi:foreign-slot-value a 'fz-rect 'y1)))))


(fz-make-rect $r1 0.0 100.0 100.0 0.0)
(dump-rect $r1)
(fz-make-rect $r2 50.0 150.0 150.0 0.0)
(dump-rect $r2)
(fz-is-empty-rect $r1)
(fz-is-empty-rect $r2)
(fz-intersect-rect $r3 $r1 $r2)
(dump-rect $r3)
(intersects $r1 $r2)
(fz-is-empty-rect $r1)

||#

(defun get-lines-in-stext-block (ctx stext-block &key (reverse-p t) bbox)
  (let (lines)
    (process-stext-block
     ctx stext-block
     (lambda (ctx line-ptr)
       (when (or (not bbox)
		 (intersects bbox (cffi:foreign-slot-pointer line-ptr 'fz-stext-line 'bbox)))
	 (push
	  (with-output-to-string (s)
	    (process-stext-line
	     ctx line-ptr
	     (lambda (ctx char-ptr)
	       (declare (ignore ctx))
	       (write-char (code-char (cffi:foreign-slot-value char-ptr 'fz-stext-char 'c))
			   s))))
	  lines))))
    (if reverse-p
	(nreverse lines)
	lines)))


#||
(get-lines-in-stext-block $ctx $b)
(cffi:foreign-slot-value $stext-page 'fz-stext-page 'mediabox)
(dump-rect (cffi:foreign-slot-value $b 'fz-stext-block 'bbox))
(fz-make-rect $r3 0.0 0.0 100.0 100.0)
(intersects (cffi:foreign-slot-pointer $b 'fz-stext-block 'bbox) $r3)
(fz-make-rect $r3 0.0 0.0 100.0 100.0)
(cffi:foreign-slot-value $b 'fz-stext-block 'bbox)
(fz-make-rect $r3 298.0 45.0 299.0 50.0)
(intersects (cffi:foreign-slot-pointer $b 'fz-stext-block 'bbox) $r3)
(get-lines-in-stext-block $ctx $b :bbox $r3)
||#

(defun get-lines-in-stext-page (ctx stext-page &key bbox)
  (let (lines)
    (process-stext-page
     ctx stext-page
     (lambda (ctx stext-block)
       (when (= (cffi:foreign-slot-value stext-block 'fz-stext-block 'claw-cxx-mupdf::type)
		+fz-stext-block-text+)
	 (when (or (not bbox)
		   (intersects (cffi:foreign-slot-pointer stext-block 'fz-stext-block 'bbox)
			       bbox))
	   (setq lines (nconc (get-lines-in-stext-block ctx stext-block :reverse-p nil) lines))))))
    (nreverse lines)))


#||
(get-lines-in-stext-page $ctx $stext-page)
(fz-make-rect $r3 298.0 45.0 299.0 50.0)
(get-lines-in-stext-page $ctx $stext-page :bbox $r3)
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

#||
(fz-drop-link $ctx $orig-links)
(defvar $orig-links (fz-load-links $ctx $page))
(defvar $links $orig-links)
(cffi:foreign-slot-value $links 'fz-link 'uri)
(cffi:foreign-slot-value $links 'fz-link 'rect)
(cffi:foreign-slot-value $links 'fz-link 'next)

(funcall  (lambda (s) (string-trim #(#\Space #\Tab #\Newline) s))
	  " foo ")

   (cffi:foreign-slot-value $links 'fz-link 'uri))

(let ((cffi::*default-foreign-encoding* :latin-1)
      (cffi::*default-character-encoding* :latin-1))
  (list
   (cffi:foreign-slot-value $links 'fz-link 'uri)
   (user::map-concatenate 'string (lambda (s) (string-trim #(#\Space #\Tab #\Newline) s))
			  (get-lines-in-stext-page $ctx $stext-page :bbox
						   (cffi:foreign-slot-pointer $links 'fz-link 'rect))
			  " ")))
(unless (cffi:null-pointer-p (cffi:foreign-slot-value $links 'fz-link 'next))
  (setq $links (cffi:foreign-slot-value $links 'fz-link 'next)))
||#

(defun extract-links-in-page (ctx page stext-opts &key external-links-only-p)
  (let* ((orig-links (fz-load-links ctx page))
	 (links orig-links) ret)
    (unless (cffi:null-pointer-p links)
      (let ((stext-page (fz-new-stext-page-from-page ctx page stext-opts)))
	(unless (cffi:null-pointer-p stext-page)
	  (loop (let* ((uri (cffi:foreign-slot-value links 'fz-link 'uri))
		       (rect (cffi:foreign-slot-pointer links 'fz-link 'rect))
		       (next (cffi:foreign-slot-value links 'fz-link 'next))
		       (lines (get-lines-in-stext-page ctx stext-page :bbox (unless (cffi:null-pointer-p rect) rect)))
		       (text (when lines (user::map-concatenate 'string (lambda (s) (string-trim #(#\Space #\Tab #\Newline) s)) lines
 " "))))
		  (when (or (not external-links-only-p)
			    (/= 0 (fz-is-external-link ctx uri)))
		    (push (cons uri text) ret))
		  (cond ((cffi:null-pointer-p next)
			 (return))
			(t (setq links next)))))
	  (fz-drop-stext-page ctx stext-page)))
      (fz-drop-link ctx orig-links))
    (nreverse ret)))

#+nil
(let ((cffi::*default-foreign-encoding* :latin-1)
      (cffi::*default-character-encoding* :latin-1))
  (extract-links-in-page $ctx $page $stext-opts))


;; doc iterators
(defvar $page-no nil)

(defun process-pages (ctx doc func)
  (loop for $page-no below (fz-count-pages ctx doc)
	do
	(let ((page (fz-load-page ctx doc $page-no)))
	  (unless (cffi:null-pointer-p page)
	    (funcall func ctx page)
	    (fz-drop-page ctx page)))))

#+nil
(let (ret)
  (process-pages $ctx $doc
		 (lambda (ctx page)
		   (setq ret (append ret (extract-links-in-page ctx page $stext-opts)))))
  ret)

#+nil
(extract-links-in-page $ctx $page $stext-opts)

(defun extract-links (file &key external-links-only-p)
  (let ((ctx (fz-new-context-imp (cffi:null-pointer) (cffi:null-pointer) +fz-store-default+ +fz-version+)))
    (unless (cffi:null-pointer-p ctx)
      (fz-register-document-handler ctx (cffi::foreign-symbol-pointer "pdf_document_handler"))
      (prog1 (let ((doc (fz-open-document ctx file)))
	       (unless (cffi:null-pointer-p doc)
		 (cffi:with-foreign-object (stext-opts 'fz-stext-options)
		   (cffi:foreign-funcall "memset"
		     :pointer stext-opts
		     :int 0
		     :size (cffi:foreign-type-size 'fz-stext-options)
		     :pointer)
		   (setf (cffi:foreign-slot-value stext-opts 'fz-stext-options 'flags)
			 (logior +FZ-STEXT-PRESERVE-LIGATURES+
				 +FZ-STEXT-PRESERVE-WHITESPACE+
				 +FZ-STEXT-MEDIABOX-CLIP+))
		   (prog1 (loop for page-no below (fz-count-pages ctx doc)
				append (let ((page (fz-load-page ctx doc page-no)))
					 (unless (cffi:null-pointer-p page)
					   (prog1 (extract-links-in-page ctx page stext-opts
									 :external-links-only-p
									 external-links-only-p)
					     (fz-drop-page ctx page)))))
		     (fz-drop-document ctx doc)))))
	(fz-drop-context ctx)))))


#+nil
(extract-links "/dev/shm/1.pdf")
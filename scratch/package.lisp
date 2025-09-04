;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Sep 04 10:13:30 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(cl:defpackage "2DR"
  (:shadowing-import-from "CL"
   "CONDITION" "POSITION" "REM" "CLOSE" "SECOND" "TYPE" "FORMAT" "WRITE" "READ" "MOD" "LENGTH" "PHASE" "REMOVE" "FUNCTION")
  (:use "CL" "SDL3" "CLAW-CXX-SDL3"))

(cl:in-package "2DR")



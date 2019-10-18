;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
(defpackage #:cluster-engine/tests
  (:use #:cl
        #:cluster-engine
        #:rove))
|#

(defpackage #:cluster-engine/tests
  (:use #:cl
        #:cluster-engine
        #:FiveAM)
  (:export #:run!
	   #:all-tests))


(in-package #:cluster-engine/tests)

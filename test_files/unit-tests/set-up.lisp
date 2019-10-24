;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setting up
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cluster-engine/tests)

(declaim (optimize (speed 0) (space 0) (debug 3)))

;; Cluster engine is silent
(setf *verbose-i/o?* nil)


(def-suite cluster-engine-tests
    :description "The top-level suite of all Cluster Engine tests.")


;; Interactive debugging 
(setf *on-error* :DEBUG)
;; (setf *on-error* :BACKTRACE)
;; (setf *on-error* NIL)
(setf *on-failure* :debug)

;; TMP: reduce number of trials for speeding up during test developments
(setf *num-trials* 10)
;; (setf *num-trials* 100)



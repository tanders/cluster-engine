;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:cluster-engine/tests)


(defun cluster-shorthand (no-of-variables rules list-of-domains
			  &key (metric-domain '((4 4))) (rnd? T) (debug? nil))
  "Slight variant of function cluster-engine::clusterengine where the function lambda list is rearranged for shorter function calls. See the orig definition for further documentation."
  (cluster-engine::clusterengine no-of-variables
                                 rnd?
                                 debug?
                                 rules
                                 metric-domain
                                 list-of-domains))

(defun get-rhythms (cluster-engine-result)
  "Return list of rhythmic sequences only."
  (tu:at-even-position (butlast cluster-engine-result)))


(defun get-pitches (cluster-engine-result)
  "Return list of rhythmic sequences only."
  (tu:at-odd-position (butlast cluster-engine-result)))


(defun get-time-signatures (cluster-engine-result)
  "Return list of time signatures only."
  (first (last cluster-engine-result)))



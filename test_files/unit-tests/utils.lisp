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

(defun get-voices (cluster-engine-solution)
  "Return list of voices, with each voice represented as a list of notes/rests in the form (<duration> <pitch>)."
  (mapcar #'tu:mat-trans (tu:plist->pairs (butlast cluster-engine-solution))))


(defun all-elements-equal? (xs &key (test #'equal))
  "Returns T if all elements in xs are equal."
  (let ((first-elt (first xs)))
    (every (lambda (x) (funcall test first-elt x)) (rest xs))))
; (all-elements-equal? '(1 1 1))


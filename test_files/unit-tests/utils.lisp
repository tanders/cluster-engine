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

(defun get-rhythms (cluster-engine-solution)
  "Return list of rhythmic sequences only."
  (tu:at-even-position (butlast cluster-engine-solution)))


(defun get-pitches (cluster-engine-solution)
  "Return list of rhythmic sequences only."
  (tu:at-odd-position (butlast cluster-engine-solution)))


(defun get-time-signatures (cluster-engine-solution)
  "Return list of time signatures only."
  (first (last cluster-engine-solution)))


(defun get-voices (cluster-engine-solution)
  "Return list of voices, with each voice represented as a list of notes/rests in the form (<duration> <pitch>)."
  (mapcar #'tu:mat-trans (tu:plist->pairs (butlast cluster-engine-solution))))


(defun get-starts (rhythms)
  "Expects result of GET-RHYTHMS and return a list of start time lists, where each start time is a ratio."
  (loop for durs in rhythms
     collect (butlast ; skip end of last note
	      (tu:dx->x durs 0))))


(defun get-keyword-voices (cluster-engine-solution)
  "Return list of voices, with each voice is represented as a list of notes/chords/rests in the form (:start <start-time> :duration <duration> :pitch <pitch>)."
  (let* ((durationss (get-rhythms cluster-engine-solution))
	 (startss (get-starts durationss))
	 (pitchess (get-pitches cluster-engine-solution)))
    ;; (mapcar #'tu:mat-trans startss durationss pitchess)
    (loop
       for durs in durationss
       for starts in startss
       for pitches in pitchess
       collect (loop for (start dur pitch) in (tu:mat-trans (list starts durs pitches))
		    collect (list :start start :duration dur :pitch pitch)))
    ))


(defun is-note? (event)
  (numberp (get-pitch event)))


(defun is-rest? (event)
  (equal (get-pitch event) NIL))


(defun is-chord? (event)
  (let ((pitches (get-pitch event)))
    (and (listp pitches)
	 (numberp (first pitches)))))


(defun get-start (event)
  (getf event :start))


(defun get-duration (event)
  (getf event :duration))


(defun get-pitch (event)
  (getf event :pitch))


(defun all-elements-equal? (xs &key (test #'equal))
  "Returns T if all elements in xs are equal."
  (let ((first-elt (first xs)))
    (every (lambda (x) (funcall test first-elt x)) (rest xs))))
; (all-elements-equal? '(1 1 1))


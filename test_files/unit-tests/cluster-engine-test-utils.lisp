;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
Utility functions for defining Cluster Engine tests 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cluster-engine/test-utils)

(def-suite testing-utils-tests
    :description "A separate top-level test-suite."
    :in cluster-engine/tests::cluster-engine-tests)

(in-suite testing-utils-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cluster Engine top-level
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cluster-shorthand (no-of-variables rules list-of-domains
			  &key (metric-domain '((4 4))) (rnd? T) (debug? nil))
  "Slight variant of function CLUSTER-ENGINE::CLUSTERENGINE where the function lambda list is rearranged for shorter function calls. See the orig definition for further documentation."
  (cluster-engine::clusterengine no-of-variables
                                 rnd?
                                 debug?
                                 rules
                                 metric-domain
                                 list-of-domains))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cluster Engine result processing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-rhythms (cluster-engine-solution)
  "Return list of note duration lists, where each duration is a ratio."
  (tu:at-even-position (butlast cluster-engine-solution)))


(defun get-pitches (cluster-engine-solution)
  "Return list of note pitch lists, where each pitch is an integer (MIDI pitch), a list of pitches for a chord, or NIL for a rest."
  (tu:at-odd-position (butlast cluster-engine-solution)))


(defun get-time-signatures (cluster-engine-solution)
  "Return list of time signatures, where each time signature is a list (<num> <denom>)."
  (first (last cluster-engine-solution)))


(defun get-voices (cluster-engine-solution)
  "Return list of voices, with each voice is represented as a list of notes/chords/rests in the form (<duration> <pitch>)."
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
; (get-pitch NIL) ; => NIL


(defun get-events-time-points (keyword-voice time-points)
  "Return list with the notes/chords/rests in KEYWORD-VOICE (notes in the format returned by GET-KEYWORD-VOICES) that sound at TIME-POINTS (list of reals);  events either started exactly at time point or started before."
  (loop for time-point in time-points
     ;; not most efficient, but result is correct, and for testing that should be fine...
     collect (find-if (lambda (event)
			(let ((start (get-start event)))
			  (and (<= start time-point)
			       (> (+ start (get-duration event)) time-point))))
		      keyword-voice ; :from-end T
		      )))


(test get-events-time-points
  "Test get-notes-at-starts"
  (let ((voice '((:START 0 :DURATION 1/8 :PITCH 67) (:START 1/8 :DURATION 1/4 :PITCH 63)
		 (:START 3/8 :DURATION 1/8 :PITCH 79)))
	(time-points '(0 1/4 1)))
    (is (equal (get-events-time-points voice time-points)
	       ;; First matches exactly, and 2nd sounds still at given start time points
	       '((:START 0 :DURATION 1/8 :PITCH 67)
		 (:START 1/8 :DURATION 1/4 :PITCH 63)
		 NIL
		 )))))

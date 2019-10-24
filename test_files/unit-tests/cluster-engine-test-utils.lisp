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
;;; FiveAM generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-selection (&key (length (gen-integer :min 0 :max 10))
			elements)
  "Return a generator that picks `length' values from `elements' without repeating them. Must be called less often than length of xs."
  (lambda ()
    (let ((elements-copy (copy-list elements)))
      (loop for i from (funcall length) downto 1
	 for pos = (random (length elements-copy))
	 collect (tu:pop-nth elements-copy pos)))))
#|
(setf my-gen (gen-selection :length (gen-integer :min 1 :max 3) :elements '((-1/2) (-1/4) (-1/8) (1/8) (1/4) (1/2))))
(funcall my-gen)
|#


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
;;; Convenience templates for randomised or algorithmically generated domain specifications
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rhythm-domain-template*
  (mapcar #'list
	  (sort (loop for i from 0 to 4
		   for x = (/ 1 (expt 2 i))
		   append (list (- x) x (* 3/2 x) (* -3/2 x)))
		#'<))
  "A range of standard rhythmic domain values to select from.")

(defparameter *pitch-domain-template*
  (loop for pitch from 36 to 84
     collect (list pitch))
  "A range of standard pitch domain values to select from.")

(defparameter *pitch-pairs*
  (tu:map-pairwise #'append *pitch-domain-template*)
  "An (unefficiently large) list of lists of two pitches each.")

(defparameter *metric-domain-template*
  (loop for demon in '(2 4 8)
     append (loop for num in '(1 2 3 4 5 6 7 9 12)
	       collect (list num demon)))
  "A range of standard time signatures to select from.")


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

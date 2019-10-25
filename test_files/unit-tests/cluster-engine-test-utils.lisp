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

(defun gen-select-one (&key candidates)
  "Return a generator that picks a value from CANDIDATES."
  (lambda ()
    (nth (random (length candidates)) candidates)))
#|
(setf my-gen (gen-select-one :candidates '((-1/2) (-1/4) (-1/8) (1/8) (1/4) (1/2))))
(funcall my-gen)
|#

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

(defun gen-ratio (&key
		    ;; (numerator (gen-integer :min -7 :max 7))
		    ;; no grace-notes for now
		    (numerator (gen-select-one :candidates '(-5 -4 -3 -2 -1 1 2 3 4 5 6 6))) 
		    (denominator (gen-select-one :candidates '(1 2 4 8 16))))
  "Return a generator that produces a ratio. NUMERATOR and DENOMINATOR are both integer generators with default values suitable for standard Cluster Engine rhythmic values."
  (lambda ()
    (/ (funcall numerator) (funcall denominator))))
#|
(setf my-gen (gen-ratio))
(funcall my-gen)

;; Generate a rhythmic motif
(setf my-gen (gen-list :length (gen-integer :min 1 :max 5) :elements (gen-ratio)))
(funcall my-gen)

;; Generate a rhythmic domain
(setf my-gen (gen-list :length (gen-integer :min 1 :max 5) 
		       :elements (gen-list :length (gen-integer :min 1 :max 5) :elements (gen-ratio))))
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
	      (tu:dx->x (mapcar #'abs durs) 0))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FiveAM test short-hands for certain Cluster Engine tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Does this really need to be a macro?
;; TODO: Turn rhythm-domain and pitch-domain into optional args?
(defmacro test-harmonic-constraint (name &body body)
  "Set up FiveAM test for harmonic constraint in a randomised CSP.

* Arguments:
  - NAME (symbol): Name of test.
  - DOCSTRING (string): Compulsary. 
  - CONSTRAINTS (list of functions): Cluster engine rule(s) to test.
  - TEST-CONDITION (unary Boolean function): Applied  to every simultaneous pitch-pair. All must return T for test to succeed.
  Keyword args
  - VOICE-NUMBER (integer, default 2): 
"
  (destructuring-bind (docstring constraints test-condition &key (voice-number 2))
      body
    `(test ,name 
       ,docstring
       (for-all ((no-of-variables (gen-integer :min 1 :max 15))
		 ;; NOTE: With complex rhythm domains,  r-pitch-pitch *can* result in Cluster Engine internal error (even with a rule always returning T).
		 ;; (rhythm-domain (gen-selection :length (gen-integer :min 1 :max (length *rhythm-domain-template*))
		 ;; 			       :elements *rhythm-domain-template*))
		 (pitch-domain (gen-selection :length (gen-integer :min 2 :max (length *pitch-domain-template*))
					      :elements *pitch-domain-template*)))
	 (let* ((rhythm-domain '((-1 -3/4 -1/2 -3/8 -1/4 -1/8 -1/16 1 3/4 1/2 3/8 1/4 1/8 1/16)))
		(voices-solution
		 (get-keyword-voices
		  (cluster-shorthand no-of-variables
				     ,constraints
				     (tu:one-level-flat
				      (make-list ,voice-number
						 :initial-element (list rhythm-domain pitch-domain))))))
		;; BUG: not generalised for more than 2 voices
		(first-voice (first voices-solution))
		(first-voice-pitches (mapcar #'get-pitch first-voice))	     
		(matching-2nd-voice-pitches (mapcar #'get-pitch
						    (get-events-time-points (second voices-solution)
									    (mapcar #'get-start first-voice))))
		(sim-pitch-pairs (tu:mat-trans (list first-voice-pitches matching-2nd-voice-pitches))))
	   (is (every ,test-condition sim-pitch-pairs)))))))



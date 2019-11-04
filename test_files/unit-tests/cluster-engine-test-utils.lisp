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
  "Return a generator that picks LENGTH values (int or generator) from ELEMENTS (list of ints or generator) without repeating them. Must be called less often than length of ELEMENTS."
  (lambda ()
    (let ((length* (if (functionp length)
		       (funcall length)
		       length))
	  (elements-copy (if (functionp elements)
			     (copy-list (funcall elements))
			     (copy-list elements))))
      (assert (<= length* (length elements-copy))
	      (length elements)
	      "Cannot pick ~A different elements from ~A." length* elements)
      (loop for i from length* downto 1
	 for pos = (random (length elements-copy))
	 collect (tu:pop-nth elements-copy pos)))))
#|
(setf my-gen (gen-selection :length (gen-integer :min 1 :max 3) :elements '((-1/2) (-1/4) (-1/8) (1/8) (1/4) (1/2))))
(funcall my-gen)
|#

(defun gen-ratio (&key
		    ;; (numerator (gen-integer :min -7 :max 7))
		    ;; no grace-notes for now
		    (numerator (gen-select-one :candidates '(-5 -4 -3 -2 -1 1 2 3 4 5 6))) 
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
  "A range of standard rhythmic domain values (only individual notes and rests, no motifs) to select from.")

(defparameter *even-rhythm-domain-template*
  (let ((motif-no 0)
	(motifs ())
	(motif-gen (gen-list :length (gen-integer :min 1 :max 5) :elements (gen-ratio))))
    (loop while (< motif-no 100)
       for motif = (funcall motif-gen)
       when (let ((motif-dur (apply #'+ (mapcar #'abs motif))))
	      (and
	       ;; Possible duration values
	       (member motif-dur '(1 1/2 1/4 1/8 1/16))
	       (not (member motif motifs :test #'equal)))
	      ;; (and (member (numerator motif-dur) '(1 2 4))
	      ;;      (member (denominator motif-dur) '(1 2 4))
	      ;;      (<= motif-dur 1))
	      )
       do (progn
	    (setf motifs (cons motif motifs))
	    (setf motif-no (1+ motif-no))))
    (sort motifs #'< :key (lambda (motif) (apply #'+ (mapcar #'abs motif)))))
  "A range of randomised standard rhythmic domain values to select from (individual rests, notes or rhythmic motifs), where the duration of each domain value is one of the possible values set.")

(defparameter *even-rhythm-domain-template-with-gracenotes*
  (let ((motif-no 0)
	(motifs ())
	(motif-gen (gen-list :length (gen-integer :min 1 :max 5)
			     :elements (gen-ratio :numerator (gen-integer :min -6 :max 7)))))
    (loop while (< motif-no 100)
       for motif = (funcall motif-gen)
       when (let ((motif-dur (apply #'+ (mapcar #'abs motif))))
	      (and
	       ;; Possible duration values
	       (member motif-dur '(1 1/2 1/4 1/8 1/16))
	       (not (member motif motifs :test #'equal))))
       do (progn
	    (setf motifs (cons motif motifs))
	    (setf motif-no (1+ motif-no))))
    (sort motifs #'< :key (lambda (motif) (apply #'+ (mapcar #'abs motif)))))
  "A range of randomised standard rhythmic domain values to select from (individual rests, notes or rhythmic motifs), where the duration of each domain value is one of the possible values set.")

(defparameter *1/4-rhythm-domain-template*
  (let ((motif-no 0)
	(motifs ())
	(motif-gen (gen-list :length (gen-integer :min 1 :max 5)
			     :elements (gen-ratio :denominator (gen-select-one :candidates '(1 2 3 4 5 6 8 10 12 16))))))
    (loop while (< motif-no 30)
       for motif = (funcall motif-gen)
       when (let ((motif-dur (apply #'+ (mapcar #'abs motif))))
	      (and
	       ;; Possible duration values
	       (= motif-dur 1/4)
	       (not (member motif motifs :test #'equal))))
       do (progn
	    (setf motifs (cons motif motifs))
	    (setf motif-no (1+ motif-no))))
    (sort motifs #'< :key (lambda (motif) (first motif))))
  "A range of randomised standard rhythmic domain values to select from (individual rests, notes or rhythmic motifs), where the duration of each domain value is 1/4 and which also allows for triplets and quintuplets.")

(defparameter *simple-rhythm-domain-template*
  (mapcar #'list '(-1 -1/2 -1/4 -1/8 -1/16 1/16 1/8 1/4 1/2 1))
  "A range of simple rhythmic domain values (only individual notes and rests, no motifs) to select from.")

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
;; (get-start '(:START 0 :DURATION 1 :PITCH '(60 64 67)))
;; (get-start '(:START 0 :DURATION -1/8 :PITCH NIL))

(defun get-duration (event &optional (abs? T))
  "Return duration of event. Rests are represented with a negative duration, but by default always a positive duration is returned by this function, which can be switched of with ABS?."
  (let ((dur (getf event :duration)))
    (if abs? (abs dur) dur)))


(defun get-pitch (event)
  (getf event :pitch))
; (get-pitch NIL) ; => NIL


;; BUG: Grace notes that happen exactly at time-point currently not handled properly -- they would be returned and notes following them would be ignored, because always one event per time-point is returned
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

;; TODO: Randomise voice-number
;; OK: Randomise rhythm-domain somewhat again: allow for a number of different reasonable precomposed rhythms (select motifs with gen-selection)
;; OK: Does this really need to be a macro?
;; OK: Turn rhythm-domain and pitch-domain into optional args?
(defun test-harmonic-constraint (constraints test-condition
				 &key (voice-number 2)
				   (rhythm-domain
				    (gen-selection
				     :length (gen-integer :min 2 :max 10)
				     :elements (gen-select-one :candidates
							       (list *rhythm-domain-template*
								     *even-rhythm-domain-template*
								     ;; TMP: For Aiva I currently don't test grace notes
								     ;; *even-rhythm-domain-template-with-gracenotes*
								     ;; *1/4-rhythm-domain-template*
								     *simple-rhythm-domain-template*))))
				   (pitch-domain (gen-selection :length (gen-integer :min 5
										     :max (length *pitch-domain-template*))
								:elements *pitch-domain-template*))
				   (stop-time (gen-ratio :numerator (gen-integer :min 0 :max 8)
							 :denominator (gen-select-one :candidates '(1 2 4))))
				   (harmony-domains NIL))
  "Set up FiveAM test (still to be wrapped in a TEST call) for harmonic constraint in a randomised CSP.

* Arguments:
  - CONSTRAINTS (list of functions): Cluster engine rule(s) to test.
  - TEST-CONDITION (unary Boolean function): Applied  to every simultaneous pitch-pair. All must return T for test to succeed.
  Keyword args
  - VOICE-NUMBER (integer): number of voices in the CSP.
  - RHYTHM-DOMAIN (generator): a rhythm domain for all voices of the CSP.
  - PITCH-DOMAIN (generator): a pitch domain for all voices of the CSP.
  - STOP-TIME (generator): time at which to stop the search.
  - HARMONY-DOMAINS (domain specs): if non-NIL, the first voice of the CSP is a sequence of scales and the second voice a sequence of chords, which together represent the underlying harmony. HARMONY-DOMAINS contains the rhythm and pitch domain of these voices in the form (<scales-rhythm-domain> <scales-pitch-domain> <chords-rhythm-domain> <chords-pitch-domain>).

BUG: Grace notes not handled properly yet.
"
  (for-all ((stop-time stop-time)
	    (rhythm-dom rhythm-domain
			;; Not only rests in the rhythm domain
			(some #'plusp (tu:flat rhythm-dom)))
			;; (every (lambda (motif)
			;; 	 (or (= (length motif) 1)
			;; 	     (some #'plusp motif)))
			;;        rhythm-dom))
	    (pitch-dom pitch-domain))
    (let* ((no-of-variables 1000)
	   (voices-domains (tu:one-level-flat
			    (make-list voice-number
				       :initial-element (list rhythm-dom pitch-dom))))
	   (solution (cluster-shorthand no-of-variables
					(rules->cluster
					 (stop-rule-time (tu:arithmeric-series 1 0 voice-number) stop-time :and)
					 constraints)
					(if harmony-domains
					    (append harmony-domains voices-domains)
					    voices-domains)))
	   (voices (get-keyword-voices solution))
	   ;; Start times of all harmonic slices up to stop-time
	   (all-start-times (sort (remove-duplicates
				   (tu:flat (loop for voice in voices
					       collect (remove-if (lambda (time) (>= time stop-time))
							(mapcar #'get-start voice)))))
				  #'<))
	   ;; BUG: grace notes note handled properly -- see comment at get-events-time-points def.
	   (sim-pitches (tu:mat-trans (loop for voice in voices
					 collect (mapcar #'get-pitch
							 (get-events-time-points voice all-start-times)))))
	   )
      (is (every test-condition sim-pitches)
	  ;; TMP: For debugging print a bit more in case of errors.
	  "For Cluster Engine solution~%  ~A~%and voices~%  ~A~%sim-pitches is~%  ~A~%where some sim pitches list violates given test~%  ~A~%stop-time was~%  ~A~%."
	  solution voices sim-pitches test-condition stop-time))))



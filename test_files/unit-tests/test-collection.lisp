;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
;; ASDF interface for running all tests
(asdf:test-system :cluster-engine)

;; Run some test-suite directly -- first re-load tests
(progn
  (asdf:load-system :cluster-engine/tests)
  (run! 'polyphonic-pitch-rules-tests))

(progn
  (asdf:load-system :cluster-engine/tests)
  (run! '8e-R-pitch-pitch_3-different-PCs))


(progn
  (asdf:load-system :cluster-engine/tests)
  (run! 'testing-utils-tests))

(run! 'cluster-engine-tests)


(asdf:load-system :cluster-engine)
(asdf:load-system :FiveAM)

|#

#|
;; Generating an html test coverage output
;; See https://lispcookbook.github.io/cl-cookbook/testing.html#code-coverage

;;; Load SB-COVER
(require :sb-cover)

;;; Turn on generation of code coverage instrumentation in the compiler
(declaim (optimize sb-cover:store-coverage-data))

;;; Load the code, ensuring that it's recompiled with the new optimization
;;; policy.
(asdf:oos 'asdf:load-op :cluster-engine :force t)
(asdf:oos 'asdf:load-op :cluster-engine/FiveAM-tests :force t)

;;; Run the test suite.
(run! 'cluster-engine-tests)

;; Load and run also cluster-rules-tests
(asdf:oos 'asdf:load-op :cluster-rules/tests :force t)
(run! 'cluster-rules-tests)

;; Produce a coverage report, set the output directory:
;; TODO: Make path platform independent -- somehow path of ASDF system...
;; NOTE: Some problems reading Cluster Engine source files...
(sb-cover:report "/Users/torsten/common-lisp/cluster-engine/coverage/")

;; Finally, turn off instrumentation
(declaim (optimize (sb-cover:store-coverage-data 0)))

;; Open resulting HTML file with browser
|#

#|
;; Collecting profiling info

;; Generating profiling info with slime
;; The profiling commands are based on CMUCL’s profiler -- from which SBCL is forked
;; See menu SLIME > Profiling 
;; https://common-lisp.net/project/slime/doc/html/Profiling.html
|#


(in-package #:cluster-engine/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setting up
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 0) (space 0) (debug 3)))


;; Cluster engine is silent
(setf *verbose-i/o?* nil)

;; Interactive debugging 
(setf *on-error* :DEBUG)
;; (setf *on-error* :BACKTRACE)
;; (setf *on-error* NIL)
(setf *on-failure* :debug)

;; TMP: reduce number of trials for speeding up during test developments
(setf *num-trials* 10)
;; (setf *num-trials* 100)


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
;;; Domain tests 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite cluster-engine-tests
    :description "The top-level suite of all Cluster Engine tests.")

(def-suite domain-tests
    :description "Testing domain declarations etc."
    :in cluster-engine-tests)

(in-suite domain-tests)

(test only-rhythm-domain
  "Randomised test with only rhythm domain: solution contains only values in rhythm domain"
  (for-all ((no-of-variables (gen-integer :min 2 :max 10))
	    ;; At least two domain values to make sure this is a search problem (avoid "All engines are locked!" error)
	    ;; TODO: Instead, in that case return the only possible solution
	    (rhythm-domain (gen-selection :length (gen-integer :min 2 :max (length *rhythm-domain-template*))
					  :elements *rhythm-domain-template*)))
    (let ((flat-rhythm-domain (tu:flat rhythm-domain))
	  (voice-rhythm-solution (first
				  (get-rhythms
				   (cluster-shorthand no-of-variables
						      '()  ; no rules
						      (list rhythm-domain
							    nil ; no pitches
							    ))))))
      (is (every (lambda (rhythm) (member rhythm flat-rhythm-domain))
		 voice-rhythm-solution))
      ;; NOTE: ;; This condition is not guarenteed (it can be broken, though that is rare for this simple case)
      ;; (is (= (length voice-rhythm-solution) no-of-variables))
      )))
;; Get docstring
;; (fiveam::description (get-test 'only-rhythm-domain))


;; NOTE: Numbers leading test titles refer to the number of Cluster Engine PWGL tutorial patches from which they are derived
;; (with some help from the previously existing translation of these patches in plain lisp in the test_files folder)
(test 2_only-rhythm-domain_with-motifs
  "Test using a rhythm domain with motifs"
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 1234))
	 (voice-rhythm-solution (first (get-rhythms
					(cluster-shorthand 4 
							   '()  ; no rules
							   ;; rhythm domain with motifs
							   '(((1/16 1/16 1/8) (1/16 -1/8 1/16) (1/12 1/12 1/12))
							     nil ; no pitches
							     ))))))
    ;; 4 motifs a 3 notes - 12 notes
    (is (equal voice-rhythm-solution '(1/12 1/12 1/12
				       1/16 1/16 1/8
				       1/16 1/16 1/8
				       1/16 -1/8 1/16)))))


(test only-pitch-domain
  "Randomised test with only pitch domain: solution contains only values in pitch domain"
  (for-all ((no-of-variables (gen-integer :min 2 :max 10))
	    ;; At least two domain values to make sure this is a search problem (avoid "All engines are locked!" error)
	    (pitch-domain (gen-selection :length (gen-integer :min 2 :max (length *pitch-domain-template*))
					 :elements *pitch-domain-template*)))
    (let ((flat-pitch-domain (tu:flat pitch-domain))
	  (voice-pitch-solution (first
				 (get-pitches
				  (cluster-shorthand no-of-variables
						     '()  ; no rules
						     (list '((1/4)) ; on rhythmic value must be given
							   pitch-domain
							   ))))))
      (is (every (lambda (pitch) (member pitch flat-pitch-domain))
		 voice-pitch-solution))
      )))


(test 3a_only-pitch-domain_with-motifs
  "Test using a pitch domain with motifs"
  ;; Needs let* seeded random state is in scope of cluster-shorthand call	 
  (let* ((*random-state* (sb-ext:seed-random-state 4321))
	 (voice-pitch-solution (first (get-pitches
				       (cluster-shorthand 7 
							  '()  ; no rules
							  '(((1/4)) 
							    ;; pitch domain with motifs
							    ((60 63 65) (67 66 65))
							    ))))))
    ;; 7 notes (rhythmic motif only 1 note long)
    (is (equal voice-pitch-solution '(67 66 65
				      60 63 65
				      60)))))


(test only-pitch-domain_with-chords
  "Test using a pitch domain with chords"
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 1111))
	 (voice-pitch-solution (first (get-pitches
				       (cluster-shorthand 4
							  '()  ; no rules
							  '(((1/4)) 
							    ;; pitch domain with chords
							    (((60 63 65)) ((67 66 65)))
							    ))))))
    ;; 4 chords
    (is (equal voice-pitch-solution '((60 63 65)
				      (67 66 65)
				      (67 66 65)
				      (67 66 65))))))


(test only-pitch-domain_with-chord-motifs
  "Test using a pitch domain with chord motifs"
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 1111))
	 (voice-pitch-solution (first (get-pitches
				       (cluster-shorthand 12
							  '()  ; no rules
							  '(((1/4)) 
							    ;; pitch domain with chords motifs
							    (((60 63 65) (67 66 65)) ((62) (59)))
							    ))))))
    ;; 
    (is (equal voice-pitch-solution '((60 63 65) (67 66 65)
				      (62) (59)
				      (62) (59)
				      (62) (59)
				      (60 63 65) (67 66 65)				      
				      (60 63 65) (67 66 65))))))


(test 3b-only-pitch-domain_with-interval-motifs
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 1234))
	 (voice-pitch-solution (first (get-pitches
				       (cluster-shorthand 12
							  '()  ; no rules
							  '(((1/4)) 
							    ;; pitch domain with motifs stating intervals
							    ((60) (m 12 -1 -1) (m -12 1 1))
							    ))))))
    ;; 
    (is (equal voice-pitch-solution '(60 60 60
				      72 71 70
				      82 81 80
				      68 69 70)))))


(test 3c-only-pitch-domain_with-chords-and-interval-motifs
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 12345))
	 (voice-pitch-solution (first (get-pitches
				       (cluster-shorthand 7
							  '()  ; no rules
							  '(((1/4)) 
							    ;; pitch domain with chords and motifs stating intervals
							    (((60 69)) (m (12 4 7)) (m (-12 3 7)))
							    ))))))
    ;; 
    (is (equal voice-pitch-solution '((60 69) (72 76 83) (60 69) (48 51 58) (60 64 71) (72 76 83) (60 63 70))))))


(test only-metric-domain
  "Randomised test with only pitch domain: solution contains only values in pitch domain"
  (for-all ((no-of-variables (gen-integer :min 2 :max 10))
	    (metric-domain (gen-selection :length (gen-integer :min 2 :max (length *metric-domain-template*))
					  :elements *metric-domain-template*)))
    (let ((time-sigs-solution (get-time-signatures
			       (cluster-shorthand no-of-variables
						  '()  ; no rules
						  '(((1/4)) 
						    ((62)))
						  :metric-domain metric-domain))))
      (is (every (lambda (ts) (member ts metric-domain :test #'equal))
		 time-sigs-solution)))))


;; TODO: Add rules that actually test effect of domain declaration with metric-domain
;; arg tuplets only has an effect if using the rule r-metric-hierarchy.
;; arg alt-beatlength affect rules that constrain events located on beats
(test 3d-only-metric-domain_using-function-metric-domain
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 1234))
	 (time-sigs-solution (get-time-signatures
			      (cluster-shorthand 12
						 '()  ; no rules
						 '(((1/4)) 
						   ((62)))
						 :metric-domain (metric-domain '(4 4) '(1 2 3 4) nil
									       '(6 8) '(1 3) 3/8)))))
    ;; 
    (is (equal time-sigs-solution '((6 8) (6 8) (4 4) (6 8))))))


#|
get-time-signatures

(test 3d-metric-domain
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 12345))
	 (voice-pitch-solution (first (get-pitches
				       (cluster-shorthand 7
							  '()  ; no rules
							  '(((1/4)) 
							    ((62)))
							  :metric-domain '((4 4) (5 8))
							  )))))
    ;; 
    (is (equal voice-pitch-solution '((60 69) (72 76 83) (60 69) (48 51 58) (60 64 71) (72 76 83) (60 63 70))))))
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single voice constraints tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite constrain-one-voice-tests
    :description "Testing individual constraints restricting a single voice."
    :in cluster-engine-tests)

(in-suite constrain-one-voice-tests)

(test R-rhythms-one-voice_all-rhythms-equal
  "Randomised test of R-rhythms-one-voice rule: all rhythmic values are equal."
  (for-all ((no-of-variables (gen-integer :min 2 :max 10))
	    (rhythm-domain (gen-selection :length (gen-integer :min 2 :max (length *rhythm-domain-template*))
					  :elements *rhythm-domain-template*)))
    (let ((voice-rhythm-solution (first
				  (get-rhythms
				   (cluster-shorthand no-of-variables
						      (R-rhythms-one-voice #'(lambda (x y) (= x y)) 0 :durations)
						      (list rhythm-domain
							    nil ; no pitches
							    )))))
					; (first-rhythm (first voice-rhythm-solution))
	  )
      (is (all-elements-equal? voice-rhythm-solution))
      ;; (is (every (lambda (rhythm) (= rhythm first-rhythm))
      ;; 		 voice-rhythm-solution))
      )))


(test 5b-R-index-rhythms-one-voice
  "Randomised test with R-index-rhythms-one-voice: first note always with set duration."
  (for-all ((no-of-variables (gen-integer :min 2 :max 10))	    
	    (rhythm-domain (gen-selection :length (gen-integer :min 2 :max (length *rhythm-domain-template*))
					  :elements *rhythm-domain-template*)))
    (let* (;; Some random rhythmic value from rhythm-domain
	   (rhythmic-value (first (nth (random (length rhythm-domain))
				       rhythm-domain)))
	   (voice-rhythm-solution (first (get-rhythms
					  (cluster-shorthand
					   no-of-variables
					   ;; Rule: first duration has set random rhythmic-value
					   (R-index-rhythms-one-voice (lambda (x) (= x rhythmic-value))				     
								      '(0) 0 :position-for-duration)
					   (list rhythm-domain
						 nil) ; no pitches
					   )))))
      (is (= rhythmic-value (first voice-rhythm-solution)))
      )))


(test 5c-r-index-pitches-one-voice
  "Randomised test with r-index-pitches-one-voice: first and third pitch motifs are equal."
  (for-all ((no-of-variables (gen-integer :min 6 :max 20)) ;; must be long enough for three 2-note motifs
	    ;; Pitch domain consists of 2-note motifs only (with uneven motif duration checking rule will be difficult...)
	    (pitch-domain (gen-selection :length (gen-integer :min 6 :max 20)
					 :elements *pitch-pairs*)))
    (let ((voice-pitch-solution (first
				 (get-pitches
				  (cluster-shorthand no-of-variables
						     ;; first and third pitch cells are equal
						     (r-index-pitches-one-voice (lambda (x y) (equal x y)) 
										'(0 2) 0 :index-for-cell)
						     (list '((1/4) (1/8) (1/16) (3/8))
							   pitch-domain
							   ))))))
      ;; 1st and 3rd pitch motifs (i.e., the set sublists below) should be equal
      (is (equal (subseq voice-pitch-solution 0 2) (subseq voice-pitch-solution 4 6)))
      )))


(test 5d-R-pitches-one-voice
  "Randomised test with R-pitches-one-voice: Every third pitch is equal."
  (for-all ((no-of-variables (gen-integer :min 6 :max 20)) ;; must be long enough
	    (pitch-domain (gen-selection :length (gen-integer :min 6 :max 20)
					 :elements *pitch-domain-template*)))
    (let ((voice-pitch-solution (first
				 (get-pitches
				  (cluster-shorthand no-of-variables
						     ;; first and third pitch cells are equal
						     (R-pitches-one-voice (lambda (p1 p2 p3 p4)
									    (declare (ignore p2 p3))
									    (equal p1 p4))
									  0 :pitches)
						     (list '((1/4) (1/8) (1/16) (3/8))
							   pitch-domain
							   ))))))
      (is (all-elements-equal? (tu:at-position voice-pitch-solution 3 0)))
      (is (all-elements-equal? (tu:at-position voice-pitch-solution 3 1)))
      (is (all-elements-equal? (tu:at-position voice-pitch-solution 3 2)))
      )))



(test 5e-R-rhythms-one-voice
  "Randomised test with R-rhythms-one-voice: Every fourth rhythmic value is equal."
  (for-all ((no-of-variables (gen-integer :min 8 :max 20)) ;; must be long enough
	    (rhythm-domain (gen-selection :length (gen-integer :min 4 :max 15)
					 :elements *rhythm-domain-template*)))
    (let ((voice-rhythm-solution (first
				 (get-rhythms
				  (cluster-shorthand no-of-variables
						     (R-rhythms-one-voice (lambda (d1 d2 d3 d4 d5)
									    (declare (ignore d2 d3 d4))
									    (equal d1 d5))
									  0 :durations)
						     (list rhythm-domain
							   '((62))
							   ))))))
      (is (all-elements-equal? (tu:at-position voice-rhythm-solution 4 0)))
      (is (all-elements-equal? (tu:at-position voice-rhythm-solution 4 1)))
      (is (all-elements-equal? (tu:at-position voice-rhythm-solution 4 2)))
      (is (all-elements-equal? (tu:at-position voice-rhythm-solution 4 3)))
      )))



(defun is-twelve-tone-row? (pitches)
  (= (length (remove-duplicates pitches))
     12))

(test 5e-R-pitches-one-voice_all-interval-row
  "Test R-pitches-one-voice: all-interval row."
  (let ((voice-pitch-solution
	 (first
	  (get-pitches
	   (cluster-shorthand 12
			      ;; Current pitch not member of any existing pitch
			      (R-pitches-one-voice (lambda (x) (not (member (car (last x)) (butlast x) )))
						   0 :all-pitches)
			      (list '((1/4) (1/8) (1/16) (3/8))
				    (mapcar #'list (loop for p from 60 to 71 collect p))
				    ))))))
    (is (is-twelve-tone-row? voice-pitch-solution))))



(test 5f-r-only-m-motifs
  "Randomised test: Rule r-only-m-motifs ensures that except for initial pitch only motifs declared with intervals are used."
  (for-all ((lowest-pitch (gen-integer :min 24 :max 36))
	    (no-of-variables (gen-integer :min 2 :max 8)))
    (let* ((voice-pitch-solution (first (get-pitches
					 (cluster-shorthand no-of-variables
							    (r-only-m-motifs 0)
							    `(((1/4)) 
							      ;; Motifs with only upwards intervals
							      ((,lowest-pitch) (m 2 2) (m 1 1))
							      ))))))
      ;; Because all motifs define only upwards intervals, the only explicit pitch in the domain must be the lowest.
      (is (= 1 (length (remove-if-not (lambda (x) (= x lowest-pitch)) voice-pitch-solution)))
	  "lowest-pitch occurs only once, i.e. only (upwards) motifs are picked except initial value.")
      (is (= (nth 0 voice-pitch-solution) lowest-pitch)
	  "lowest-pitch is initial first value."))))


(defun get-start-time-position (durations start-time)
  "Translates the durations of a note/rest sequence into their start times and then returns the position of the duration at the given start time."
  (let ((start-times (tu:dx->x (mapcar #'abs durations) 0)))
    (position start-time start-times)))
;; (get-start-time-position '(1 1 2 1) 2) ; => 2

#|
;; TMP:
(get-start-time-position '(-1/8 -1/8 1/16 1/16 1/8 3/16 1/4 -1/16 3/16 1/4 -1/8 1/4 1/16 -1/16 -1/16) 2)

(length '(-1/8 -1/8 1/16 1/16 1/8 3/16 1/4 -1/16 3/16 1/4 -1/8 1/4 1/16 -1/16 -1/16))
(apply #'+ (mapcar #'abs '(-1/8 -1/8 1/16 1/16 1/8 3/16 1/4 -1/16 3/16 1/4 -1/8 1/4 1/16 -1/16 -1/16)))

(apply #'+ '(1/16 1/16 1/8 1/16 1/16 1/8 1/16 1/16 1/8 1/8 1/16 1/16 1/8))
|#

#|
;; BUG: This test does not work yet. The no-syncopation can be contradicted and thus there can be no new note starting at time-point and thus R-rhythms-one-voice-at-timepoints does not apply any constraint.
;; Possibly this is a new bug, because I think no-syncopation worked before
(Test 5f-R-rhythms-one-voice-at-timepoints
  "Randomised test with R-rhythms-one-voice-at-timepoints: Constraint to hold at the specified time points: note duration starting at that time is 1/4."
  (for-all ((no-of-variables (gen-integer :min 9 :max 20)) ;; must be long enough
	    (time-point (gen-integer :min 1 :max 2)) ;; TMP: reduced max time point
	    ;; (rhythm-domain-part (gen-selection :length (gen-integer :min 2 :max 5)
	    ;; 				       :elements *rhythm-domain-template*))
	    )
    (let* (; (rhythm-domain (append '((1/16) (1/4)) rhythm-domain-part))
	   (rhythm-domain '((1/16) (1/8) (1/4)))
	   (constrained-duration 1/4) ;; could also be randomised, but hey :)
	   (voice-rhythm-solution
	    (first (get-rhythms
		    (cluster-shorthand no-of-variables
				       (rules->cluster
					(R-rhythms-one-voice-at-timepoints (lambda (x) (equal x (list 0 constrained-duration)))
									   0 (list time-point) :dur-start)
					;; Time points are only whole numbers, i.e. 1st beats of bars
					;; Rule no-syncopation forces that there is always a note starting at a downbeat
					(cr:no-syncopation :voices 0 :metric-structure :1st-beat))
				       (list rhythm-domain
					     '((62)))
				       :metric-domain '((4 4))))))
	   (time-point-pos (get-start-time-position voice-rhythm-solution time-point)))
      (is (= (nth time-point-pos voice-rhythm-solution)
	     constrained-duration))
      )))
|#


(test 6a-R-index-rhythm-pitch-one-voice
  "Testing R-index-rhythm-pitch-one-voice: relation of notes at positions idx1 and idx2 constrained according to defined rule."
  (for-all ((no-of-variables (gen-integer :min 6 :max 15))
	    (idx1 (gen-integer :min 0 :max 3))
	    (idx2 (gen-integer :min 1 :max 12)
		  ;; Generated idx2 will comply with this guard, ensuring order of indices etc.
		  (and  
		   (< idx1 idx2)
		   (< idx2 no-of-variables))))
    (flet ((rule (n1 n2)
	     ;; each note is list: (<dur> <pitch>)
	     (let ((dur1 (first n1)))
	       (if (= dur1 1/4)
		   (equal n2 '(1/4 60))
		   (equal n1 n2)))))
      (let* ((solution-voice (first (get-voices
				     (cluster-shorthand no-of-variables
							(R-index-rhythm-pitch-one-voice #'rule
											(list idx1 idx2) 0 :nth-note)
							`(((1/4) (1/8) (1/16) (3/8)) 
							  ,(loop for p from 60 to 71 collect (list p))
							  ))))))
	(is (funcall #'rule (nth idx1 solution-voice) (nth idx2 solution-voice))
	    "Notes at position idx1 and idx2 do comply with rule.")))))



(test 6b-R-rhythm-pitch-one-voice
  "Testing r-rhythm-pitch-one-voice: the duration of a note conditions its pitch."
  (for-all ((no-of-variables (gen-integer :min 1 :max 15)))
    (flet ((rule (note)
	     ;; note is list: (<dur> <pitch>)
	     (let ((dur (first note))
		   (pitch (second note)))
	       (= pitch 
		  (case dur 
		    (1/16 60) 
		    (1/8 67)  
		    (1/4 72)
		    (t 0)))))
	   (is-grace-note? (note)
	     "Duration of gracenote is 0."
	     (= 0 (first note))))
      (let* ((solution-voice (first (get-voices
				     (cluster-shorthand no-of-variables
							(R-rhythm-pitch-one-voice #'rule
										  0 :rhythm/pitch :exclude-gracenotes)
							`(((1/4) (1/8) (1/16) (3/8) (0)) 
							  ,(loop for p from 60 to 71 collect (list p))
							  ))))))
	(is (every #'rule (remove-if #'is-grace-note? solution-voice)))))))


#|
;; BUG: Cluster Engine can (but does not have to) result in :no-solution
(test 6b-rhythm-pitch-rule-one-voice_with-gracenotes
  "Testing r-rhythm-pitch-one-voice: the duration of a note conditions its pitch and this time gracenote pitches are constrained as well."
  (for-all ((no-of-variables (gen-integer :min 1 :max 15)))
    (flet ((rule (note)
	     ;; note is list: (<dur> <pitch>)
	     (let ((dur (first note))
		   (pitch (second note)))
	       (= pitch 
		  (case dur 
		    (1/16 60) 
		    (1/8 67)  
		    (1/4 72)
		    (0 62)
		    (t 0))))))
      (let* ((solution-voice (first (get-voices
				     (cluster-shorthand no-of-variables
							(R-rhythm-pitch-one-voice #'rule
										  0 :rhythm/pitch :normal)
							`(((1/4) (1/8) (1/16) (3/8) (0)) 
							  ,(loop for p from 60 to 72 collect (list p))
							  ))))))
	(is (every #'rule solution-voice))))))
|#


(test 6c-R-rhythm-pitch-one-voice
  "Testing r-rhythm-pitch-one-voice: two consecutive notes are constrained. If a note follows a rest, its pitch is 60, otherwise its pitch is higher than its predecessor."
  (for-all ((no-of-variables (gen-integer :min 2 :max 15)))
    (flet ((rule (note1 note2)
	     ;; Each note is list: (<dur> <pitch>)
	     (let ((dur1 (first note1))
		   (pitch1 (second note1))
		   (pitch2 (second note2)))
	       (if pitch2 ;; note2 not a rest
		   (if (minusp dur1)
		       ;; if note1 is a rest
		       (= pitch2 60)
		       (> pitch2 pitch1))
		   t))))
      (let* ((solution-voice (first (get-voices
				     (cluster-shorthand no-of-variables
							(R-rhythm-pitch-one-voice #'rule
										  0 :include-rests :exclude-gracenotes)
							`(((1/16) (1/8) (1/4) (-1/8)) 
							  ,(loop for p from 60 to 79 collect (list p))
							  ))))))
	(is (every #'identity (tu:map-neighbours #'rule solution-voice)))))))


(test 6d-R-rhythm-pitch-one-voice
  "Testing r-rhythm-pitch-one-voice: note segments (phrases) between rests are constrained. All note pitches must be equal to their preceeding note unless there is a rest, then a new pitch can be set."
  (for-all ((no-of-variables (gen-integer :min 5 :max 15)))
    (flet ((rule (note1 note2)
	     ;; Each note is list: (<dur> <pitch>)
	     (let ((pitch1 (second note1))
		   (pitch2 (second note2)))
	       (= pitch1 pitch2))))
      (let* ((solution-voice (first (get-voices
				     (cluster-shorthand no-of-variables
							(R-rhythm-pitch-one-voice #'rule
										  0 :rhythm/pitch-segment :exclude-gracenotes)
							`(((1/16) (1/8) (1/4) (-1/8)) 
							  ,(loop for p from 60 to 79 collect (list p))
							  ))))))
	(is (every #'identity
		   (tu:map-neighbours (lambda (note1 note2)
					(let ((dur1 (first note1))
					      (dur2 (first note2)))
					  (if (or (minusp dur1) (minusp dur2))
					      ;; If either note is a rest, rule was not applied
					      T
					      (funcall #'rule note1 note2))))
				      solution-voice)))))))




(test 6e-r-mel-interval-one-voice
  "Testing r-mel-interval-one-voice: quarter notes are followed by a fourth and eighth notes by an interval smaller than a maj. third."
  (for-all ((no-of-variables (gen-integer :min 5 :max 15)))
      (let* ((solution-voice (first (get-voices
				     (cluster-shorthand no-of-variables
							(rules->cluster
							 (r-mel-interval-one-voice 0 :normal :normal
										   := 1/4 := 5)
							 (r-mel-interval-one-voice 0 :normal :normal
										   := 1/8 :smaller-than 4))
							`(((1/8) (1/4)) 
							  ,(loop for p from 60 to 79 collect (list p))
							  ))))))
	(is (every #'identity
		   (tu:map-neighbours (lambda (note1 note2)
					(let ((dur1 (first note1))
					      (pitch1 (second note1))
					      (pitch2 (second note2)))
					  (cond
					    ((= dur1 1/4) (= (abs (- pitch2 pitch1))
							     5))
					    ((= dur1 1/8) (< (abs (- pitch2 pitch1))
							     4)))))
				      solution-voice))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Metric constraints tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite metric-rules-one-voice-tests
    :description "Testing constraints restricting the metric structure of a single voice."
    :in cluster-engine-tests)

(in-suite metric-rules-one-voice-tests)


(test 7a-R-metric-hierarchy
  "Non-randomised testing R-metric-hierarchy: beats are only subdivided into a 'grid' of 3 or 4 possible note onsets, which reduces the rhythmic complexity (e.g., triplet and 1/8th or 1/16th-notes cannot be freely combined without a note starting on a beat in between."
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 1234))
	 (voice-rhythm-solution (first (get-rhythms
					(cluster-shorthand 21
							   (R-metric-hierarchy 0
									       ;; rests are not constrained
									       :durations)
							   '(;; Rhythmic domain also allows for 8th-note triplets
							     ((1/12) (1/16) (1/8) (1/4))
							     ((62)))
							   :metric-domain '((4 4))
							   ))))
	 ;; Translate start times such that 1 corresponds to quarter note, so that beats can be seen easily (each whole number)
	 (start-times (mapcar (lambda (time) (* time 4.0))
			      (butlast ;; skip last (end of last note)
			       (tu:dx->x voice-rhythm-solution 0)))))
    (is (equal start-times
	       '(0.0 1.0
		 ;; 8th triplet followed by quarter etc -- but still in grid of triplets 
		 1.3333334 2.3333333 2.6666667
		 ;; back on the beat and only here 1/16th note etc. following.
		 3.0 3.25 4.25 5.25 5.75 6.75 7.75 8.25
		 9.25 9.75 10.75 11.0
		 ;; Again triplet grid
		 11.333333 12.333333 12.666667 13.0) ;  13.333333
	       ))))


(test R-meter-note_no-syncopation-across-bars
  "Randomised test with r-meter-note - no syncopations across bar lines."
  (for-all ((no-of-variables (gen-integer :min 8 :max 30)))
    (let* ((rhythm-solution
	    (get-rhythms ; rhythms of all voices (one nesting level more)
	     (cluster-shorthand no-of-variables
				(R-meter-note (lambda (x) (= x 0))
					      0 :1st-beat :offset :norm)
				'(;; Rhythmic domain also allows for 8th-note triplets
				  ((1/12) (1/16) (1/8) (1/4))
				  ((62)))
				:metric-domain '((4 4)))))
	   (voice-starts (first (get-starts rhythm-solution)))	  
	   )
      (is (every (lambda (int) (member int voice-starts))
		 ;; Each new bar must start with a new note. In 4/4 meter, the start time of a bar is always an integer,
		 ;; and so all integers up to the ending of the score must be contained in start-times.
		 (loop for i from 0 to (floor (tu:last-element voice-starts))
		    collect i)))
      )))


(test 7b-r-meter-note
  "Randomised r-meter-note test: Every bar starts with a 1/4-note."
  (for-all ((no-of-variables (gen-integer :min 8 :max 30)) 
	    )
    (let* ((bar-start-dur 1/4)
	   (voice-rhythm-solution
	    (first
	     (get-rhythms
	      (cluster-shorthand no-of-variables
				 (r-meter-note (lambda (x) (equal x (list 0 bar-start-dur)))
					       0 :1st-beat :offset_dur :norm)
				 '(;; Rhythmic domain also allows for 8th-note triplets
				   ((1/12) (1/16) (1/8) (1/4))
				   ((62)))
				 :metric-domain '((4 4))))))
	   (start-times (butlast ;; skip last (end of last note)
			 (tu:dx->x voice-rhythm-solution 0)))
	   ;; Each new bar must start with a new note. In 4/4 meter, the start time of a bar is always an integer,
	   ;; and so all integers up to the ending of the score must be contained in start-times.
	   ;; Their positions are then used to access their durations.
	   (bar-start-positions (loop for i from 0 to (floor (tu:last-element start-times))
				   collect (position i start-times)))
	   (bar-start-durations (loop for pos in bar-start-positions collect (nth pos voice-rhythm-solution))))
      (is (every (lambda (dur) (= dur bar-start-dur)) bar-start-durations))
      )))


(test 7c-R-note-meter
  "Randomised test with r-note-meter: every note of specified-dur (1/4) must start on a beat."
  (for-all ((no-of-variables (gen-integer :min 8 :max 30)) 
	    )
    (let* ((specified-dur 1/4)
	   (voice-rhythm-solution
	    (first
	     (get-rhythms
	      (cluster-shorthand no-of-variables
				 (rules->cluster
				  (R-note-meter (lambda (x)
						  (if (= (first x) specified-dur)
						      (= (second x) 0)
						      t))
						0 :d_offs :beats :incl-rests :normal)
				  ;; Redundant rule, but simplifying rhythm makes it more likely that  1/4-notes can occur
				  (cluster-engine::R-metric-hierarchy 0 :durations))
				 '(;; Rhythmic domain also allows for 8th-note triplets
				   ((1/12) (1/16) (1/8) (1/4))
				   ((62)))
				 :metric-domain '((4 4))))))
	   (start-times (butlast ;; skip last (end of last note)
			 (tu:dx->x voice-rhythm-solution 0)))
	   (durations-with-starts (tu:mat-trans (list voice-rhythm-solution start-times)))
	   (quarter-notes-with-starts (remove-if-not (lambda (x) (= x 1/4)) durations-with-starts :key #'first))
	   )
      ;; Start on a beat: start mod 1/4 is 0
      ;; (is (every (lambda (start) (= (mod start 1/4) 0)) quarter-note-starts))
      (is (every (lambda (dur-n-start)
		   (let ((start (second dur-n-start)))
		     (= (mod start specified-dur) 0)))
		 quarter-notes-with-starts))
      )))


(test 7d-R-meter-note
  "non-randomised R-meter-note test: every note on beat in 6/8 meter is C4 (60) and every note on beat in 4/4 meter is G4 (67)."
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 4321))
	 (solution 
	  (cluster-shorthand 21
			     (rules->cluster
			      (R-meter-note (lambda (note)
					      (let ((pitch (third note))
						    (meter (fourth note)))
						(if (equal meter '(6 8))
						    (= pitch 60)
						    ;; otherwise 4/4
						    (= pitch 67))))
					    0 :beats :offset_dur_pitch_meter :norm)
			      ;; Redundant rule, but simplifying rhythm makes it more likely that notes on beat occur
			      (cluster-engine::R-metric-hierarchy 0 :durations)
			      ;; Another redundant rule disallowing syncopations: makes time signature changes more likely
			      (R-meter-note (lambda (x) (= x 0)) 0 :1st-beat :offset :norm))
			     `(;; Small rhythmic domain, also making notes on beat likely
			       ((1/8) (3/8) (1/4))
			       ,(loop for p from 60 to 79 collect (list p)))
			     :metric-domain (metric-domain '(4 4) '(1 2 3 4) nil ; nil: default beat length
							   ;; 6/8 beat is 3/8 long
							   '(6 8) '(1 3) 3/8)
			     ))
	 (time-sigs (get-time-signatures solution))
	 (voice (first (get-voices solution))))
    (is (equal (list voice time-sigs)
	       '((;; bar 1: 6/8, where accents are 60
		  (1/4 60) (3/8 60) (1/8 74)
		  ;; bar 2: 4/4, where accents are 67
		  (3/8 67) (1/4 67) (1/8 66) (1/4 67)
		  ;; bar 3: 6/8, where accents are 60
		  (3/8 60) (1/4 60) (1/8 68)
		  ;; remaining bars: 4/4, where accents are 67
		  (1/8 67) (3/8 67) (1/4 67) (1/8 67) (1/8 64) (1/4 67)
		  (3/8 67) (3/8 67) (1/8 67) (1/4 67) (3/8 67))
		 ((6 8) (4 4) (6 8) (4 4) (4 4) (4 4)))))
    ))


(test 7e-r-note-meter
  "non-randomised r-note-meter test: notes in 4/4 meter on beat are restricted to C-major and notes in 6/8 to F#-major."
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 4321))
	 (solution 
	  (cluster-shorthand 21
			     (rules->cluster
			      (r-note-meter (lambda (note)
					      (let* ((meter (third note))
						     (pitch (fourth note))
						     (pc (mod pitch 12)))
						(if (equal '(4 4) meter)
						    ;; C-major for 4/4
						    (member pc '(0 2 4 5 7 9 11))
						    ;; F#-major for 6/8
						    (member pc '(1 3 5 6 8 10 11)))))
					    0 :d_offs_m_n :beats :incl-rests :normal :true/false)
			      ;; Redundant rule, but simplifying rhythm makes it more likely that notes on beat occur
			      (cluster-engine::R-metric-hierarchy 0 :durations)
			      ;; Another redundant rule disallowing syncopations: makes time signature changes more likely
			      (R-meter-note (lambda (x) (= x 0)) 0 :1st-beat :offset :norm))
			     `(;; Small rhythmic domain, also making notes on beat likely
			       ((1/8) (3/8) (1/4))
			       ,(loop for p from 60 to 72 collect (list p)))
			     :metric-domain (metric-domain '(4 4) '(1 2 3 4) nil ; nil: default beat length
							   ;; 6/8 beat is 3/8 long
							   '(6 8) '(1 3) 3/8)
			     ))
	 (time-sigs (get-time-signatures solution))
	 (voice (first (get-voices solution))))
    (is (equal (list voice time-sigs)
	       '((;; bar 1: 6/8 in F# major
		  (1/4 70) (1/8 65) (1/8 65) (1/8 61) (1/8 70)
		  ;; bar 2
		  (3/8 65) (1/8 68) (1/4 70)
		  ;; bar 3: 4/4 in C-major
		  (1/8 67) (3/8 67) (1/4 60) (1/8 71) (1/8 72)
		  ;; bar 4
		  (1/4 69) (3/8 67) (1/4 64) (1/8 69)
		  ;; bar 5: 6/8
		  (1/8 65) (1/4 68) (3/8 71)
		  ;; bar 6: 4/4
		  (3/8 64))
		 ((6 8) (6 8) (4 4) (4 4) (6 8) (4 4)))
	       ))
    ))

#|
(let ((*package* (find-package :cluster-engine/tests)))
  (funcall #'rule (nth idx1 solution-voice) (nth idx2 solution-voice)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Polyphonic constraints tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite polyphonic-pitch-rules-tests
    :description "Testing individual polyphonic constraints on pitches."
    :in cluster-engine-tests)

(in-suite polyphonic-pitch-rules-tests)


(test 8a-R-pitch-pitch_no-voice-crossing
  "Testing R-pitch-pitch: no voice crossing between voices 1 and 2."
  (for-all ((no-of-variables (gen-integer :min 5 :max 15)))
    (flet ((rule (pitches)
	     (>= (first pitches) (second pitches))))
      (let* ((pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule '(0 1) '(0) :all :no_grace :pitch)
				  `(;; voice 1
				    ((1/4)) ; all notes equal duration
				    ,pitch-domain
				    ;; voice 2
				    ((1/4)) 
				    ,pitch-domain
				    ))))
	     ;; Simultaneous pitches of voice 1 and 2
	     (sim-pitch-pairs (tu:mat-trans pitches-solution)))
	(is (every #'rule sim-pitch-pairs))))))


(test 8a-R-pitch-pitch_only-specified-intervals
  "Testing R-pitch-pitch: constrain harmonic intervals between voices 1 and 2 to certain allowed intervals."
  (for-all ((allowed-intervals (gen-selection :length (gen-integer :min 2 :max 6)
					      :elements (loop for p from 1 to 11 collect p)))
	    (no-of-variables (gen-integer :min 5 :max 15)))
    (flet ((rule (pitches)
	     (let* ((p1 (first pitches))
		    (p2 (second pitches))
		    (pc-interval (mod (- p2 p1) 12)))
	       (member pc-interval allowed-intervals))))
      (let* ((pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule '(0 1) '(0) :all :no_grace :pitch)
				  `(;; voice 1
				    ((1/4)) ; all notes equal duration
				    ,pitch-domain
				    ;; voice 2
				    ((1/4)) 
				    ,pitch-domain
				    ))))
	     (sim-pitch-pairs (tu:mat-trans pitches-solution)))
	(is (every #'rule sim-pitch-pairs))
	))))


(test 8b-R-pitch-pitch_on-beat
  "Testing R-pitch-pitch: on beat, no voice crossing between voices 1 and 2."
  (for-all ((no-of-variables (gen-integer :min 10 :max 16)))
    (flet ((rule (pitches)
	     (>= (first pitches) (second pitches))))
      (let* ((pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule '(0 1) '(0) :beat :no_grace :pitch)
				  `(;; voice 1
				    ((1/8)) ;; NOTE: every 2nd note not on beat in 4/4
				    ,pitch-domain
				    ;; voice 2
				    ((1/8)) 
				    ,pitch-domain
				    ))))
	     (sim-pitch-pairs (tu:mat-trans pitches-solution)))
	;; Every even note is on beat in 4/4
	(is (every #'rule (tu:at-even-position sim-pitch-pairs)))
	;; ;; Testing intervals not on beat -- some should contradict rule (high likelyhood, but not completely certain).
	;; ;; Consider therefore removing the clause again (it worked at least often)
	;; (is (notevery #'rule (tu:at-odd-position sim-pitch-pairs)))
	))))


(test 8c-R-pitch-pitch_consecutive-time-slices
  "Testing R-pitch-pitch: constrain consecutive harmonic intervals between voices 1 and 2 to differ."
  (for-all ((no-of-variables (gen-integer :min 8 :max 16)))
    (flet ((rule (pitches1 pitches2)	     
	     (let* ((interval1 (apply #'- pitches1))
		    (interval2 (apply #'- pitches2)))
	       (not (equal interval1 interval2)))))
      (let* ((pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule '(0 1) '(0) :all :no_grace :pitch)
				  `(;; voice 1
				    ((1/4)) ; all notes equal duration
				    ,pitch-domain
				    ;; voice 2
				    ((1/4)) 
				    ,pitch-domain
				    ))))
	     (sim-pitch-pairs (tu:mat-trans pitches-solution)))
	(is (every #'identity
		   ;; apply rule to two consecutive pairs of simultaneous pitches, which returns a list of Booleans
		   (tu:map-neighbours #'rule sim-pitch-pairs)))
	))))


(test 8c-R-pitch-pitch_1st-voice
  "Testing R-pitch-pitch: for every note onset in voice 1, constrain harmonic intervals between voices 1 and 2 to the allowed intervals."
  (for-all ((allowed-intervals (gen-selection :length (gen-integer :min 2 :max 6)
					      :elements (loop for p from 1 to 11 collect p)))
	    (no-of-variables (gen-integer :min 4 :max 10)))
    (flet ((rule (pitches)
	     (let ((pitch1 (first pitches))
		   (pitch2 (second pitches)))
	       (if (and pitch1 pitch2) ; no rests
		   (let ((pc-interval (mod (- pitch2 pitch1) 12)))
		     (member pc-interval allowed-intervals))
		   T))))
      (let* ((rhythm-domain '((1/16 1/16 1/8) (1/4) (1/8 1/8) (3/8 1/16)))
	     (pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (voices-solution
	      (get-keyword-voices
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule '(0 1) '(0) :1st-voice :no_grace :pitch)
				  (list ;; voice 1
				   rhythm-domain pitch-domain
				   ;; voice 2
				   rhythm-domain pitch-domain
				   ))))
	     (first-voice (first voices-solution))
	     (first-voice-pitches (mapcar #'get-pitch first-voice))	     
	     (matching-2nd-voice-pitches (mapcar #'get-pitch
						 (get-events-time-points (second voices-solution)
									 (mapcar #'get-start first-voice))))
	     (sim-pitch-pairs (tu:mat-trans (list first-voice-pitches matching-2nd-voice-pitches))))
	(is (every #'rule sim-pitch-pairs))
	))))


(test 8d-R-pitch-pitch_three-voices
  "Testing R-pitch-pitch: constrain three voices -- the simultaneous interval between voice 1 and 2 is the same as between voice 2 and 3."
  (for-all ((no-of-variables (gen-integer :min 1 :max 15)))
    (flet ((rule (pitches)
	     (let* ((p1 (first pitches))
		    (p2 (second pitches))
		    (p3 (third pitches))
		    (interval1 (- p1 p2))
		    (interval2 (- p2 p3)))	     
	     (= interval1 interval2))))
      (let* ((pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule '(0 1 2) '(0) :all :no_grace :pitch)
				  `(;; voice 1
				    ((1/4)) ; all notes equal duration
				    ,pitch-domain
				    ;; voice 2
				    ((1/4)) 
				    ,pitch-domain
				    ;; voice 3
				    ((1/4)) 
				    ,pitch-domain
				    ))))
	     ;; Simultaneous pitches of voices 1, 2 and 3
	     (sim-pitch-triplets (tu:mat-trans pitches-solution)))
	(is (every #'rule sim-pitch-triplets))))))


(test 8e-R-pitch-pitch_no-voice-crossing
  "Testing R-pitch-pitch: for three voices, no voice crossing between voice pairs."
  (for-all ((no-of-variables (gen-integer :min 1 :max 10)))
    (flet ((rule (pitches)
	     (let* ((p1 (first pitches))
		    (p2 (second pitches)))
	       (>= p1 p2))))
      (let* (;; ? TODO: randomise pitch domain?
	     (pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule
						 '((0 1) (1 2)) '(0) :beat :no_grace :pitch)
				  `(;; voice 1
				    ((1/4)) ; all notes equal duration
				    ,pitch-domain
				    ;; voice 2
				    ((1/4)) 
				    ,pitch-domain
				    ;; voice 3
				    ((1/4)) 
				    ,pitch-domain
				    ))))
	     ;; Simultaneous pitches of voices 1, 2 and 3
	     (sim-pitch-triplets (tu:mat-trans pitches-solution))
	     ;; Sim voice pairs of voices 1 & 2 and then voice 2 & 3
	     (sim-pitch-pairs (loop for triple in sim-pitch-triplets
				 append (loop for (start end) in '((0 2) (1 3))
					   collect (subseq triple start end)))))
	(is (every #'rule sim-pitch-pairs))))))


(test 8e-R-pitch-pitch_3-different-PCs
  "Testing R-pitch-pitch: for three voices, simultaneous notes should have different PCs."
  (for-all ((no-of-variables (gen-integer :min 1 :max 10)))
    (flet ((rule (pitches)
	     (= (length (remove-duplicates (pitch->pc pitches)))
		3)))
      (let* (;; ? TODO: randomise pitch domain?
	     (pitch-domain (loop for p from 60 to 79 collect (list p)))
	     (pitches-solution
	      (get-pitches
	       (cluster-shorthand no-of-variables
				  (R-pitch-pitch #'rule
				       '(0 1 2) '(0) :beat :no_grace :pitch)
				  `(;; voice 1
				    ((1/4)) ; all notes equal duration
				    ,pitch-domain
				    ;; voice 2
				    ((1/4)) 
				    ,pitch-domain
				    ;; voice 3
				    ((1/4)) 
				    ,pitch-domain
				    ))))
	     ;; Simultaneous pitches of voices 1, 2 and 3
	     (sim-pitch-triplets (tu:mat-trans pitches-solution)))
	(is (every #'rule sim-pitch-triplets))))))


(test 8e-R-chords
  "Testing R-chords: resulting chords match allowed chords; chord transpositions and inversions are possible."
  (for-all ((no-of-variables (gen-integer :min 1 :max 15)))
    (let* ((pitch-domain (loop for p from 60 to 84 collect (list p)))
	   (allowed-chords-for-R-chords '((4 7) (3 7)))
	   ;; ;; Corresponding untransposed PC sets
	   ;; (allowed-chord-pc-sets (loop for chord in allowed-chords-for-R-chords
	   ;; 			     ;; pitches->pc-normal-form not necessary for major and minor triads,
	   ;; 			     ;; but in case chords are changed...
	   ;; 			     collect (pitches->pc-normal-form (cons 0 chord))))
	   ;; Manually collected for simplicy. These could be generated algorithmically by generating all possible subsets of allowed-chords-for-R-chords and apply to each (pc-transpose-to-0 (pitches->pc-normal-form my-subset))
	   (allowed-chord-pc-sets '((0 3 7) (0 4 7) (0 3) (0 4) (0 5) (0)))
	   (pitches-solution
	    (get-pitches
	     (cluster-shorthand no-of-variables
				(cluster-engine::R-chords '(0 1 2) allowed-chords-for-R-chords
							  '(0) :all :exclude-gracenotes)
				`(;; voice 1
				  ;; all notes equal duration
				  ((1/4)) ,pitch-domain
				  ;; voice 2
				  ((1/4)) ,pitch-domain
				  ;; voice 3
				  ((1/4)) ,pitch-domain))))
	   ;; Simultaneous pitches of voices 1, 2 and 3
	   (sim-pitches (tu:mat-trans pitches-solution))
	   ;; Untransposed PC sets in normal form (like the chords are)
	   (sim-pc-sets (loop for pitches in sim-pitches
			   collect (pc-transpose-to-0 (pitches->pc-normal-form pitches)))))
      ;; For every sim-pc-set match some chord: every pc from sim-pc-set matches some chord pc by matching every of its pcs
      (is (every (lambda (sim-pc-set)
		   (some (lambda (allowed-chord)
			     ;; untransposed-normal-form can be incomplete chord, so make sure every pitch is part of chord
			     (every (lambda (pc) (member pc allowed-chord))
				    sim-pc-set))
			   allowed-chord-pc-sets))
		 sim-pc-sets)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testing other constraints
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite other-rules-tests
    :description "Testing other constraints."
    :in cluster-engine-tests)

(in-suite other-rules-tests)

;; TODO: (apply #'ce:r-predefine-meter r-predefine-meter-args)

;; TODO: (apply #'ce:stop-rule-time stop-rule-time-args)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; Example with fixture
(def-fixture in-test-environment ()
  "Set up and tear down the test environment."
  (setup-code)
  (&body)
  (teardown-code))

(def-test a-test ()
  "Test in clean environment."
  (with-fixture in-test-environment ()
    (is-true (some-function))))

|#


#|
;; NOTE: Tests can be evaluated interactively (like in rove), and in case of fail drop into debugger
(for-all ((x (gen-integer :min 1 :max 10))
          (y (gen-integer :min 1 :max 10)))
  "Test doc string"
  (is (>= x y)))


(get-pitches
 (cluster-shorthand 4 
		    '()  ; no rules
		    '(((1/4))
		      ((60) (62))
		      )))
|#

#|
;; TMP:
(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))
(dummy-tests)

;; ? TMP:
(test only-rhythm-domain-1
  (let* ((*random-state* (sb-ext:seed-random-state 1234))
	 (result (get-rhythms
		  (cluster-shorthand 4 
				     '()  ; no rules
				     ;; rhythm domain 
				     '(((1/4) (1/8) (-1/4))
				       nil ; no pitches
				       )))))
    (is (equal result '((-1/4 1/4 1/4 1/8))))))


|#

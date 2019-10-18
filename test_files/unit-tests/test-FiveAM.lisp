;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
;; ASDF interface for running all tests
(asdf:test-system :cluster-engine)

;; Run test-suite directly
(run! 'cluster-engine-tests)


(asdf:load-system :cluster-engine/FiveAM-tests)

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
  (let* (;; Seeded random state
	 (*random-state* (sb-ext:seed-random-state 4321))
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
;;; Single constraints tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite single-constraints-tests
    :description "Testing domain declarations etc."
    :in cluster-engine-tests)

(in-suite single-constraints-tests)

(test all-rhythms-equal
  "Randomised test of R-rhythms-one-voice rule: all rhythmic values are equal."
  (for-all ((no-of-variables (gen-integer :min 2 :max 10))
	    (rhythm-domain (gen-selection :length (gen-integer :min 2 :max (length *rhythm-domain-template*))
					  :elements *rhythm-domain-template*)))
    (let* ((voice-rhythm-solution (first
				  (get-rhythms
				   (cluster-shorthand no-of-variables
						      (ce::R-rhythms-one-voice #'(lambda (x y) (= x y)) 0 :durations)
						      (list rhythm-domain
							    nil ; no pitches
							    )))))
	   ; (first-rhythm (first voice-rhythm-solution))
	   )
      (is (all-elements-equal? voice-rhythm-solution))
      ;; (is (every (lambda (rhythm) (= rhythm first-rhythm))
      ;; 		 voice-rhythm-solution))
      )))


(test 5b-index-rules-one-engine
  "Randomised  test with R-index-rhythms-one-voice: first note always with set duration."
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
					   (ce:R-index-rhythms-one-voice
					    #'(lambda (x) (= x rhythmic-value))				     
					    '(0) 0 :position-for-duration)
					   (list rhythm-domain
						 nil) ; no pitches
					   )))))
      (is (= rhythmic-value (first voice-rhythm-solution)))
      )))


(test 5c-index-rules-one-engine
  "Randomised test with r-index-pitches-one-voice: first and third pitch motifs are equal."
  (for-all ((no-of-variables (gen-integer :min 6 :max 20)) ;; must be long enough for three 2-note motifs
	    ;; Pitch domain consists of 2-note motifs only (with uneven motif duration checking rule will be difficult...)
	    (pitch-domain (gen-selection :length (gen-integer :min 6 :max 20)
					 :elements *pitch-pairs*)))
    (let* ((voice-pitch-solution (first
				  (get-pitches
				   (cluster-shorthand no-of-variables
						      ;; first and third pitch cells are equal
						      (r-index-pitches-one-voice 
						       #'(lambda (x y) (equal x y)) 
						       '(0 2) 0 :index-for-cell)
						      (list '((1/4) (1/8) (1/16) (3/8))
							    pitch-domain
							    ))))))
      ;; 1st and 3rd pitch motifs (i.e., the set sublists below) should be equal
      (is (equal (subseq voice-pitch-solution 0 2) (subseq voice-pitch-solution 4 6)))
      )))


(test 5d-wildcard-rules-one-engine
  "Randomised test with R-pitches-one-voice: Every third pitch is equal."
  (for-all ((no-of-variables (gen-integer :min 6 :max 20)) ;; must be long enough
	    (pitch-domain (gen-selection :length (gen-integer :min 6 :max 20)
					 :elements *pitch-domain-template*)))
    (let* ((voice-pitch-solution (first
				  (get-pitches
				   (cluster-shorthand no-of-variables
						      ;; first and third pitch cells are equal
						      (cluster-engine::R-pitches-one-voice
						       #'(lambda (p1 p2 p3 p4)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

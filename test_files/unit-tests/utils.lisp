;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
Utility functions for defining Cluster Engine tests 
|#

(in-package #:cluster-engine/tests)


(def-suite testing-utils-tests
    :description "A separate top-level test-suite.")

(in-suite testing-utils-tests)

;; NOTE: Some definitions copied here from my only internally updated library ta-utilities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun best-if (xs comparison)
  "Return the best of XS with respect to COMPARISON (binary Boolean function). In case of ties, the first best is returned."
  (let ((x1 (first xs)))
    (loop for x2 in (rest xs)
       when (funcall comparison x2 x1)
       do (setf x1 x2))
    x1))
; (best-if '(3 5 2 4 1 7 4) #'<)
; (best-if '(3 5 2 4 1 7 4) #'>)
; (best-if '(3 5 2 4 1 7 4) #'=)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pitch class utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch->pc ((pitch integer))
  "Translate a pitch (MIDI note number) into a pitch class (integer), assuming 12-EDO."
  (mod pitch 12))

(defmethod pitch->pc ((pitches list))
  "Translate a list of pitches (MIDI note numbers) into corresponding pitch classes (integers), assuming 12-EDO."
  (mapcar #'pitch->pc pitches))
; (pitch->pc '(60 64 67))

(defun pc-transpose-to-0 (pc-set)
  "Transpose PC-SET such that first element is 0."
  (let ((first (first pc-set)))
    (loop for pc in pc-set
       collect (pitch->pc (- pc first)))))
; (pc-transpose-to-0 '(2 6 9))

;; Algorithm inspired by http://openmusictheory.com/normalOrder.html (and https://www.mta.ca/pc-set/pc-set_new/pages/page04/page04.html)
(defun pitches->pc-normal-form (pitches)
  "Translate a list of pitches (MIDI note numbers) into a PC set in normal form (represented by a list of integers).

NOTE: Somewhat simplified: in case of tie between not only outmost but also 2nd-output interval in terms for compactness, simply the first of tie is chosen. This could be fixed with recursive function, but I don't need that for my purposes.

NOTE: With sets of great intervallic regularity, the ordering that begins with the lowest number should be chosen, but again this function is simplified.
"
  (let* ((aux (remove-duplicates (pitch->pc pitches)))
	 (l (length aux)))
    (if (< l 2)
	aux
	(let* (;; Compiler note : "could not stack allocate..." -- why?
	       (pcs (sort aux #'<))
	       (intervals-between (pitch->pc (tu:x->dx (append pcs (list (first pcs))))))
	       (max-interval (apply #'max intervals-between))
	       (max-interval-positions (loop for pos in (tu:positions-if (lambda (x) (= x max-interval)) intervals-between)
					  ;; Position to the right...
					  collect (mod (1+ pos) l)))
	       (candidate-forms
		(loop for start-position in max-interval-positions
		   collect (if (= start-position 0)
			       pcs 
			       (append
				(subseq pcs start-position l)
				(subseq pcs 0 start-position)))))
	       ;; Wrap in list a candidate-form and PC interval between its first and but-last PC
	       (annotated-candidate-forms
		(loop for form in candidate-forms
		   collect (list form
				 (pitch->pc (- (nth (- l 2) form)
					       (nth 0 form)))))))
	  ;; (break)
	  ;; NOTE: Simplification: take 
	  (first ;; skip annotating interval again
	   (best-if annotated-candidate-forms (lambda (x y) (< (second x) (second y)))))
	  ))))
#|
(pitches->pc-normal-form '(2 14 14))
(pitches->pc-normal-form '(64 67 72))
(pitches->pc-normal-form '(64 67 72 76))
(pitches->pc-normal-form '(67 72 75))
(pitches->pc-normal-form '(62 67 71 74))
(pitches->pc-normal-form '(60 64 67 70))
;; Example with tied surrounding most compact interval, see https://www.mta.ca/pc-set/pc-set_new/pages/page04/page04.html
(pitches->pc-normal-form '(1 4 7 8 10))
; => (4 7 8 10 1) ; but (7 8 10 1 4) would even be more compact (at interval between first and third-last, see webpage above)
(pitches->pc-normal-form '(2 4 8 10)) ; any order would do here...
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


(defun all-elements-equal? (xs &key (test #'equal))
  "Returns T if all elements in XS are equal."
  (let ((first-elt (first xs)))
    (every (lambda (x) (funcall test first-elt x)) (rest xs))))
; (all-elements-equal? '(1 1 1))


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



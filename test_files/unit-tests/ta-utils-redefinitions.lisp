;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package :ta-utilities/redefinitions)

;; Some definitions copied here from my only internally updated library ta-utilities.

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

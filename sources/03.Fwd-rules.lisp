(in-package cluster-engine)

;;;;;;;;;;;;;;;;;
;Fwd-rule1 is just to always get a solution. It does NOT balance engines.
(defun fwd-rule1 (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "Very simple: shortest index next, in order of default order"
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))
  vsolution ;this is just here to avoid error message when compiled
  (if (aref vbacktrack-history 0)
      (progn (pop (aref vbacktrack-history 3))
        (pop (aref vbacktrack-history 2))
        (pop (aref vbacktrack-history 1))
        (pop (aref vbacktrack-history 0))) ;forward to last backtracked engine
    (let ((min-index (apply 'min (loop for n in (aref vdefault-engine-order 0) collect (aref vindex n)))))
      (declare (type fixnum min-index))
      (loop for engine in (aref vdefault-engine-order 0)
            while (/= (aref vindex engine) min-index)
            finally (return engine)))))

(defun fwd-rule_left-to-right_type-tie-breaking (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "A forward rule (dynamic variable ordering) for CLUSTERENGINE. A forward rule returns the index of the engine where next variable should be visited.

Somewhat simplified, this variable ordering progresses in score time \"from left to  right\”, completing all voices in parallel. More specifically, this variable ordering always visits next the engine where the solution is most behind in score time in the search, but in case of ties, the meter engine is visited first, and all rhythm engines before any pitch engine (engine type priorities). In case of ties on engine type priority, the engine with lowest engine index is visited first.

This variable ordering is suitable for polyphonic CSPs in general, including for CSPs where the rhythm is fixed in the CSP definition (by a single rhythm motif in the rhythm domain), and only the pitches are searched for (other forward rules are less suitable for this case)."
  ;; TODO: check what of these commented declarations is useful -- and if used, then also add suitable compile declaration
  ;; (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  ;; (declare (type fixnum number-of-engines))
  (declare (ignore number-of-engines))

  (pop-backtrack-history vbacktrack-history)

  (let* ((metric-engine (get-metric-engine-index vdefault-engine-order))
	 (metric-engine-endtime (if (engine-set? metric-engine vindex) ;; (/= (aref vindex metric-engine) -1)
				    (get-current-index-endtime metric-engine vindex vsolution)
				    0))
	 ;; ? TODO: Refactoring -- can I somehow reduce the computations here (e.g., by memoization -- somehow store intermediate results perhaps)? E.g., with one of the libraries at https://www.cliki.net/memoization
	 ;;
	 ;; Each spec is a triple (<type-priority> <engine-index> <current-endtime>)
	 ;; Type priorities: meter = 3, rhythm = 2, pitch = 1.
	 ;; In case of ties, the engine with the higher type priority is visited.
	 (metric-engine-spec (list 3 metric-engine metric-engine-endtime))
	 (rhythm-engine-specs
	  (mapcar (lambda (partial-spec) (cons 2 partial-spec))
		  (matrix-trans
		   (list (get-rhythm-engine-indices vdefault-engine-order)
			 (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order)))))
	 (pitch-engine-specs
	  (mapcar (lambda (partial-spec) (cons 1 partial-spec))
		  (matrix-trans
		   (list (get-pitch-engine-indices vdefault-engine-order)
			 (get-end-times-all-pitch-engines vsolution vindex vdefault-engine-order)))))
	 (all-specs (append (list metric-engine-spec)
			    rhythm-engine-specs
			    pitch-engine-specs)))
    ;; (when (= (length (remove-if-not (lambda (x) (equal x -1)) vindex))
    ;; 	     1)
    ;;   (break))
    (second ;; return engine index
     (best-if all-specs (lambda (spec1 spec2)
			  (let ((start1 (third spec1))
				(start2 (third spec2)))
			    (cond (;; Main condition: smallest start time
				   (< start1 start2)
				   T)
				  (;; Tie break
				   (= start1 start2)
				   (cond (;; First visit variable with higher type priority
					  (> (first spec1) (first spec2))
					  T)
					 (;; Tie break again
					  ;; Prefer variables with lower engine number
					  ;; ? TODO: Make this perhaps dependent on an argument? Perhaps sometimes I want to start search with lowest pitches on highest voice index?
					  (= (first spec1) (first spec2))
					  (< (second spec1) (second spec2)))
					 (T NIL)))
				  (T NIL))))))))
  
#|  
  (let* ((metric-engine (get-metric-engine-index vdefault-engine-order))
	 (length-metric-engine (if (engine-set? metric-engine vindex) ;; (/= (aref vindex metric-engine) -1)
                                   (get-current-index-endtime metric-engine vindex vsolution)
				   0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length)))
    ;; (declare (type number max-voice-length min-voice-length)) ;  length-metric-engin
    ;; (declare (type list all-voices-total-length))
    ;; (break)
    (if (<= length-metric-engine max-voice-length)
	metric-engine
	(let ((pitch-engine (find-pitch-engine-with-earliest-missing-pitch vsolution vindex vdefault-engine-order)))
	  (if pitch-engine
	      pitch-engine
	      (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order))))
    )
|#


;;;;;;;;;;;;;;;;;
;Fwd-rule-indep does not take backtrack history into consideration.
(defun fwd-rule-indep (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "Backtrack route is poped just to keep the list short. This could be removed.

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (when (aref vbacktrack-history 0)
    (progn (pop (aref vbacktrack-history 3))
      (pop (aref vbacktrack-history 2))
      (pop (aref vbacktrack-history 1))
      (pop (aref vbacktrack-history 0)))) ;pop backtracked engine just to make the list shorter..

  (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1)
                                   (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution)
				   0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length))
         pitch-engine-with-missing-pitches)
    (declare (type number max-voice-length min-voice-length)) ;  length-metric-engin
    (declare (type list all-voices-total-length))
    (declare (type t pitch-engine-with-missing-pitches))

    (cond ((<= length-metric-engine max-voice-length)
           (car (aref vdefault-engine-order 3)))
          ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
           pitch-engine-with-missing-pitches)
          (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

    ))


;;;;;;;;;;;;;;;;;
;Fwd-rule2 expects a matching rhythm engine for every pitch engine (a pitch engine without rhythm will be skipped). 
;It also expects a metric engine (it will crash without one).

(defun fwd-rule2 (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
 "If there is a backtrack routed, forward that engine (and pop the list). 

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (if (aref vbacktrack-history 0)
      (progn (pop (aref vbacktrack-history 3))
        (pop (aref vbacktrack-history 2))
        (pop (aref vbacktrack-history 1))
        (pop (aref vbacktrack-history 0))) ;forward to last backtracked engine
    (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1) 
                                     (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution) 0))
           (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
           (max-voice-length (apply 'max all-voices-total-length))
           (min-voice-length (apply 'min all-voices-total-length))
           pitch-engine-with-missing-pitches)
      (declare (type number max-voice-length min-voice-length)) ;  length-metric-engin 
      (declare (type list all-voices-total-length))
      (declare (type t pitch-engine-with-missing-pitches))

      (cond ((<= length-metric-engine max-voice-length)
             (car (aref vdefault-engine-order 3)))
            ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
             pitch-engine-with-missing-pitches)
            (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

      )))

;;;---



(defun fwd-rule3 (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "This forward rule takes the index of the backtracked index into consideration.
If backtrack route is not at index, make a free choice.

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (loop
   while (aref vbacktrack-history 0)
   do
   (cond ((= (1+ (aref vindex (car (aref vbacktrack-history 0)))) (car (aref vbacktrack-history 1)))
             ;if proposed backtrack engine is at the same index as at the point of backtracking: forward to this engine
          (pop (aref vbacktrack-history 3))
          (pop (aref vbacktrack-history 2))
          (pop (aref vbacktrack-history 1))
          (return-from fwd-rule3 (pop (aref vbacktrack-history 0))))
         ((> (1+ (aref vindex (car (aref vbacktrack-history 0)))) (car (aref vbacktrack-history 1)))
             ;if proposed backtrack engine is at a higher index than at the point of backtracking: loop backtrack-info until
             ;an index within the range is found.
          (pop (aref vbacktrack-history 3))
          (pop (aref vbacktrack-history 2))
          (pop (aref vbacktrack-history 1))
          (pop (aref vbacktrack-history 0)))
         (t
             ;if proposed backtrack engine is at a lower index than at the point of backtracking: return from loop and let other
             ;forward rules determine (until index will catch up).
          (return nil))))

  (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1) 
                                   (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution) 0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length))
         pitch-engine-with-missing-pitches)
    (declare (type number max-voice-length min-voice-length)) ; length-metric-engin
    (declare (type list all-voices-total-length))
    (declare (type t pitch-engine-with-missing-pitches))

    (cond ((<= length-metric-engine max-voice-length)
           (car (aref vdefault-engine-order 3)))
          ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
           pitch-engine-with-missing-pitches)
          (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

    ))


;;;---

(defun fwd-rule4 (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "This forward rule takes the index of the backtracked index into consideration.
If backtrack engine is not at index, backtrack engine without poping it (i.e. step forward this
engine and assign variables until index has caught up).

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (loop
   while (aref vbacktrack-history 0)
   do
   (cond ((= (1+ (aref vindex (car (aref vbacktrack-history 0)))) (car (aref vbacktrack-history 1)))
             ;if proposed backtrack engine is at the same index as at the point of backtracking: forward to this engine
          (pop (aref vbacktrack-history 3))
          (pop (aref vbacktrack-history 2))
          (pop (aref vbacktrack-history 1))
          (return-from fwd-rule4 (pop (aref vbacktrack-history 0))))
         ((> (1+ (aref vindex (car (aref vbacktrack-history 0)))) (car (aref vbacktrack-history 1)))
             ;if proposed backtrack engine is at a higher index than at the point of backtracking: loop backtrack-info until
             ;an index within the range is found.
          (pop (aref vbacktrack-history 3))
          (pop (aref vbacktrack-history 2))
          (pop (aref vbacktrack-history 1))
          (pop (aref vbacktrack-history 0)))
         (t
             ;if proposed backtrack engine is at a lower index than at the point of backtracking: forward this engine 
             ;without poping it (until index will catch up).   
          (return-from fwd-rule4 (car (aref vbacktrack-history 0))))))

  (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1) 
                                   (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution) 0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length))
         pitch-engine-with-missing-pitches)
    (declare (type number max-voice-length min-voice-length)) ; length-metric-engin
    (declare (type list all-voices-total-length))
    (declare (type t pitch-engine-with-missing-pitches))

    (cond ((<= length-metric-engine max-voice-length)
           (car (aref vdefault-engine-order 3)))
          ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
           pitch-engine-with-missing-pitches)
          (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

    ))

;;;---


(defun fwd-rule5 (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "This forward rule takes the COUNT VALUE of the backtracked index into consideration.
If the backtracked engine is not at the count value, backtrack that engine without poping it (to catch up).
The metric layer considers index instead of count value.

Count value has the problem that rests are not counted in rhythm engines. Rests could slip trhough the system
and cause the engine to only generate rests for a backtracked engine.

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (loop
   while (aref vbacktrack-history 0)
   do
   (progn (when (= (car (aref vbacktrack-history 0)) (1- number-of-engines))
          ;if the proposed backtrack engine is the metric engine, just bactrack it.
          ;This can definitely be more elegant done (i.e. more efficient)
            (pop (aref vbacktrack-history 3))
            (pop (aref vbacktrack-history 2))
            (pop (aref vbacktrack-history 1))
            (return-from fwd-rule5 (pop (aref vbacktrack-history 0))))
     (let ((current-index-total-notecount (if (>= (aref vindex (car (aref vbacktrack-history 0))) 0)
                                              (get-current-index-total-notecount (car (aref vbacktrack-history 0)) vindex vsolution)
                                            0)))
       (declare (type fixnum current-index-total-notecount))
       (cond
        ((= current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has the same number of notes (or pitches) than at the moment of backtracking: forward to this engine
          ;and pop it
         (pop (aref vbacktrack-history 3))
         (pop (aref vbacktrack-history 2))
         (pop (aref vbacktrack-history 1))

         (return-from fwd-rule5 (pop (aref vbacktrack-history 0))))
        ((< current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has a lower number of notes (or pitches) than at the moment of backtracking: forward according to  
          ;general rules (until the number of notes will catch up).  
          ;The reason for this is that rests in rhythm engines arenot counted and can make the system only pick rests (since count value 
          ;will not change). Change this in rule6 when time pont is considered....

         (return nil))
        ((> current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has a higher number of notes (or pitches) than at the moment of backtracking: loop backtrack-info until
          ;an index within the range is found.
         (pop (aref vbacktrack-history 3))
         (pop (aref vbacktrack-history 2))
         (pop (aref vbacktrack-history 1))
         (pop (aref vbacktrack-history 0))
         )))))

  (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1) 
                                   (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution) 0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length))
         pitch-engine-with-missing-pitches)
    (declare (type number max-voice-length min-voice-length)) ; length-metric-engin
    (declare (type list all-voices-total-length))
    (declare (type t pitch-engine-with-missing-pitches))

    (cond ((<= length-metric-engine max-voice-length)
           (car (aref vdefault-engine-order 3)))
          ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
           pitch-engine-with-missing-pitches)
          (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

    ))

;;;---


(defun fwd-rule6 (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "This forward rule takes the COUNT VALUE AND TIME POINT  of the backtracked index into consideration.
If the backtracked engine is not at the count value (pitch engine) or time point (rhythm engine/metric engine), 
backtrack that engine without poping it (to catch up).

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (loop
   while (aref vbacktrack-history 0)
   do
   (if (evenp (car (aref vbacktrack-history 0)))
       ;if the proposed backtrack engine has a time line, i.e. it is a metric or rhythm engine:
       ;Compare cureent end time with start time at the previous moment of backtracking
       (let ((current-index-end-time (get-current-index-endtime (car (aref vbacktrack-history 0)) vindex vsolution)))
         (cond ((= current-index-end-time (car (aref vbacktrack-history 3)))
                ;if the proposed backtrack engine has the same end time as at the moment of backtracking: forward to this engine
                ;and pop it
                (pop (aref vbacktrack-history 3))
                (pop (aref vbacktrack-history 2))
                (pop (aref vbacktrack-history 1))
                (return-from fwd-rule6 (pop (aref vbacktrack-history 0))))
               ((< current-index-end-time (car (aref vbacktrack-history 3)))
                ;if the proposed backtrack engine has an earlier end time than at the moment of backtracking: forward this engine  
                ;without poping it (until the number of notes will catch up).  
                (return-from fwd-rule6 (car (aref vbacktrack-history 0))))
               ((> current-index-end-time (car (aref vbacktrack-history 3)))
                ;if the proposed backtrack engine has a later end time than at the moment of backtracking: loop backtrack-info until
                ;a time point within the range is found.
                (pop (aref vbacktrack-history 3))
                (pop (aref vbacktrack-history 2))
                (pop (aref vbacktrack-history 1))
                (pop (aref vbacktrack-history 0)))))

     ;else (if it is a pitch engine)
     (let ((current-index-total-notecount (if (>= (aref vindex (car (aref vbacktrack-history 0))) 0)
                                              (get-current-index-total-notecount (car (aref vbacktrack-history 0)) vindex vsolution)
                                            0)))
       (declare (type fixnum current-index-total-notecount))
       (cond
        ((= current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has the same number of notes (or pitches) than at the moment of backtracking: forward to this engine
          ;and pop it
         (pop (aref vbacktrack-history 3))
         (pop (aref vbacktrack-history 2))
         (pop (aref vbacktrack-history 1))
         (return-from fwd-rule6 (pop (aref vbacktrack-history 0))))
        ((< current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has a lower number of notes (or pitches) than at the moment of backtracking: forward this engine  
          ;without poping it (until the number of notes will catch up).  
         (return-from fwd-rule6 (car (aref vbacktrack-history 0))))
        ((> current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has a higher number of notes (or pitches) than at the moment of backtracking: loop backtrack-info until
          ;an index within the range is found.
         (pop (aref vbacktrack-history 3))
         (pop (aref vbacktrack-history 2))
         (pop (aref vbacktrack-history 1))
         (pop (aref vbacktrack-history 0))
         )))))

  (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1) 
                                   (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution) 0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length))
         pitch-engine-with-missing-pitches)
    (declare (type number max-voice-length min-voice-length)) ; length-metric-engin
    (declare (type list all-voices-total-length))
    (declare (type t pitch-engine-with-missing-pitches))

    (cond ((<= length-metric-engine max-voice-length)
           (car (aref vdefault-engine-order 3)))
          ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
           pitch-engine-with-missing-pitches)
          (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

    ))

;;;---


(defun fwd-rule6B (vsolution vindex vbacktrack-history vdefault-engine-order number-of-engines)
  "Identical to fwd-rule6 but:
If the backtracked engine is not at the count value (pitch engine) or time point (rhythm engine/metric engine), 
backtrack according to general rules.

1. Metric structure has to be longest.
2. Fill out pitches (for durations without pitches) in all voices - start with the voice with highest priority.
3. Search for rhythm in the voice that is most behind. If two or more are equal, the default search order determines 
which voice to search next."
  (declare (type array vsolution vindex vbacktrack-history vdefault-engine-order))
  (declare (type fixnum number-of-engines))

  (loop
   while (aref vbacktrack-history 0)
   do
   (if (evenp (car (aref vbacktrack-history 0)))
       ;if the proposed backtrack engine has a time line, i.e. it is a metric or rhythm engine:
       ;Compare cureent end time with start time at the previous moment of backtracking
       (let ((current-index-end-time (get-current-index-endtime (car (aref vbacktrack-history 0)) vindex vsolution)))
         (cond ((= current-index-end-time (car (aref vbacktrack-history 3)))
                ;if the proposed backtrack engine has the same end time as at the moment of backtracking: forward to this engine
                ;and pop it
                (pop (aref vbacktrack-history 3))
                (pop (aref vbacktrack-history 2))
                (pop (aref vbacktrack-history 1))
                (return-from fwd-rule6B (pop (aref vbacktrack-history 0))))
               ((< current-index-end-time (car (aref vbacktrack-history 3)))
                ;if the proposed backtrack engine has an earlier end time than at the moment of backtracking: forward this engine  
                ;without poping it (until the number of notes will catch up).  
                (return nil))
               ((> current-index-end-time (car (aref vbacktrack-history 3)))
                ;if the proposed backtrack engine has a later end time than at the moment of backtracking: loop backtrack-info until
                ;a time point within the range is found.
                (pop (aref vbacktrack-history 3))
                (pop (aref vbacktrack-history 2))
                (pop (aref vbacktrack-history 1))
                (pop (aref vbacktrack-history 0)))))

     ;else (if it is a pitch engine)
     (let ((current-index-total-notecount (if (>= (aref vindex (car (aref vbacktrack-history 0))) 0)
                                              (get-current-index-total-notecount (car (aref vbacktrack-history 0)) vindex vsolution)
                                            0)))
       (declare (type fixnum current-index-total-notecount))
       (cond
        ((= current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has the same number of notes (or pitches) than at the moment of backtracking: forward to this engine
          ;and pop it
         (pop (aref vbacktrack-history 3))
         (pop (aref vbacktrack-history 2))
         (pop (aref vbacktrack-history 1))
         (return-from fwd-rule6B (pop (aref vbacktrack-history 0))))
        ((< current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has a lower number of notes (or pitches) than at the moment of backtracking: forward this engine  
          ;without poping it (until the number of notes will catch up).  
         (return nil))
        ((> current-index-total-notecount (car (aref vbacktrack-history 2)))
          ;if the proposed backtrack engine has a higher number of notes (or pitches) than at the moment of backtracking: loop backtrack-info until
          ;an index within the range is found.
         (pop (aref vbacktrack-history 3))
         (pop (aref vbacktrack-history 2))
         (pop (aref vbacktrack-history 1))
         (pop (aref vbacktrack-history 0))
         )))))

  (let* ((length-metric-engine (if (/= (aref vindex (car (aref vdefault-engine-order 3))) -1) 
                                   (get-current-index-endtime (car (aref vdefault-engine-order 3)) vindex vsolution) 0))
         (all-voices-total-length (get-total-duration-all-rhythm-engines vsolution vindex vdefault-engine-order))
         (max-voice-length (apply 'max all-voices-total-length))
         (min-voice-length (apply 'min all-voices-total-length))
         pitch-engine-with-missing-pitches)
    (declare (type number max-voice-length min-voice-length)) ; length-metric-engin
    (declare (type list all-voices-total-length))
    (declare (type t pitch-engine-with-missing-pitches))

    (cond ((<= length-metric-engine max-voice-length)
           (car (aref vdefault-engine-order 3)))
          ((setf pitch-engine-with-missing-pitches (find-pitch-engine-with-missing-pitches vsolution vindex vdefault-engine-order))
           pitch-engine-with-missing-pitches)
          (t (find-shortest-rhythm-engine min-voice-length all-voices-total-length vdefault-engine-order)))

    ))


;;;---

(defun pop-backtrack-history (vbacktrack-history)
  "pop backtracked engine just to make the list shorter.."
  (when (aref vbacktrack-history 0)
    (progn (pop (aref vbacktrack-history 3))
	   (pop (aref vbacktrack-history 2))
	   (pop (aref vbacktrack-history 1))
	   (pop (aref vbacktrack-history 0)))))
 

(defun find-pitch-engine-with-missing-pitches (vsolution vindex vdefault-engine-order)
  (declare (type array vsolution vindex vdefault-engine-order))
;(print vsolution *cluster-engine-log-output*)
  (loop for rhythm-engine in (aref vdefault-engine-order 1)
        do (let ((pitch-engine (1+ rhythm-engine)))
             (when (member pitch-engine (aref vdefault-engine-order 2))
               (let ((nr-of-pitches (if (/= (aref vindex pitch-engine) -1)
					(get-current-index-total-pitchcount pitch-engine vindex vsolution) 0))
                     (nr-of-notes (if (/= (aref vindex rhythm-engine) -1)
				      (get-current-index-total-notecount rhythm-engine vindex vsolution) 0)))
                 (when (> nr-of-notes nr-of-pitches)
		   (return pitch-engine)))))
        finally (return nil))) ;this means that all pitch engines are OK


#| ;; TMP:
vsolution example value at index. Looks like durations (neg for rest), start times (neg for rest) and number of notes (excluding rests) so far. First start time is 1 instead of 0 for some strange reason and then durations are added accumulatively.
(((3/8 3/8 1/8 -1/4 1/8 1/8 1/8 3/8 3/8 1/8 -1/4 1/8 1/8 1/8) 
  (1 11/8 7/4 -15/8 17/8 9/4 19/8 5/2 23/8 13/4 -27/8 29/8 15/4 31/8 4) 
  (1 2 3 3 4 5 6 7 8 9 9 10 11 12)))
|#

;; ? TODO: Handle also motifs
;; TODO: Why is first endtime 1 and not 0??
(defun get-start-time-at-note-position (engine vindex vsolution note-position)
  "Return start time of note at NOTE-POSITION. Rests are skipped (e.g., if voice of ENGINE starts with rests, then the start time at NOTE-POSITION = 0 is greater than 0).

If the engine has no assigned events, the time will be 1 (i.e. the minimum time).
Engine has to be a rhythm engine."
  (declare (type array vsolution vindex))
  (declare (type fixnum engine))
  (if (engine-set? engine vindex)
      ;; TODO: Optimisation: only skip rests up to nr-of-pitches -- perhaps replace with loop that returns once index is reached.
      (let ((starts (remove-if #'minusp ;; remove rests 
				 (cadar (aref (aref vsolution engine) (aref vindex engine))))))
	(nth note-position starts))
      1))


(defun get-end-times-all-pitch-engines (vsolution vindex vdefault-engine-order)
  "For each pitch engine, return the end time of the last note to which a pitch has been assigned (list of numbers)."
  (declare (type array vsolution vindex vdefault-engine-order))
  (loop for rhythm-engine in (get-rhythm-engine-indices vdefault-engine-order)
     collect (let* ((pitch-engine (1+ rhythm-engine))
		    (nr-of-pitches (if (engine-set? pitch-engine vindex)
				       (get-current-index-total-pitchcount pitch-engine vindex vsolution)
				       0))
		    (end-time (if (engine-set? rhythm-engine vindex)
				    (get-start-time-at-note-position rhythm-engine vindex vsolution nr-of-pitches)
				    0)))
	       end-time)))


#|
;; TODO: Discuss with Orjan whether to use find-pitch-engine-with-earliest-missing-pitch in some other forward engines
;; BUG: In case nr-of-pitches and nr-of-notes are both 0, these are not taken, but some voice with higher end time for pitches but less pitches than nr-of-notes is taken, and that might then be only candidate.  
;; TODO: Can I reduce loop somehow, e.g., when nr-of-pitches is 0, this is already minimal.
(defun find-pitch-engine-with-earliest-missing-pitch (vsolution vindex vdefault-engine-order)
  "Return the pitch engine with the smallest end time of its corresponding rhythm engine out of those that is shorter than their corresponding rhythm engine. Result NIL means that no pitch engine is shorter than its corresponding rhythm engine."
  (declare (type array vsolution vindex vdefault-engine-order))
  (let (candidates)
    (loop for rhythm-engine in (get-rhythm-engine-indices vdefault-engine-order)
       do (let ((rhythm-engine-set? (engine-set? rhythm-engine vindex))
		(pitch-engine (1+ rhythm-engine)))
	    (when (member pitch-engine (get-pitch-engine-indices vdefault-engine-order))
	      (let* ((nr-of-pitches (if (engine-set? pitch-engine vindex)
					(get-current-index-total-pitchcount pitch-engine vindex vsolution)
					0))
		     (nr-of-notes (if rhythm-engine-set?
				      (get-current-index-total-notecount rhythm-engine vindex vsolution)
				      0))
		     (start-time (if rhythm-engine-set?
				     (get-start-time-at-note-position rhythm-engine vindex vsolution nr-of-pitches)
				     0)))
		;; (break)
		(when (> nr-of-notes nr-of-pitches)
		  (push (list pitch-engine start-time) candidates))
		))))
    ;; TMP: tu-dependency -- remove!
    (first (best-if candidates #'< :key #'second))
    ))
|#


(defun find-shortest-rhythm-engine (min-voice-length all-voices-total-length vdefault-engine-order)
  (declare (type number min-voice-length))

  (declare (type list all-voices-total-length))
  (declare (type array vdefault-engine-order))
  (loop for engine in (aref vdefault-engine-order 1)
        for total-length in all-voices-total-length
        do (when (= total-length min-voice-length) (return engine))))


(defun get-total-duration-all-rhythm-engines (vsolution vindex vdefault-engine-order)
  (declare (type array vsolution vindex vdefault-engine-order))
  (loop for engine in (aref vdefault-engine-order 1)
        collect (if (/= (aref vindex engine) -1) (get-current-index-endtime engine vindex vsolution) 0)))



;;;;;;;;;;;;;;;;; below are not used


(defun get-total-notecount-all-rhythm-engines (vsolution vindex vdefault-engine-order)
  (declare (type array vsolution vindex vdefault-engine-order))
  (loop for engine in (aref vdefault-engine-order 1)
        collect (if (/= (aref vindex engine) -1) (get-current-index-total-notecount engine vindex vsolution) 0)))

(defun get-total-pitchcount-all-pitch-engines (vsolution vindex vdefault-engine-order)
  (declare (type array vsolution vindex vdefault-engine-order))
  (loop for engine in (aref vdefault-engine-order 2)
        collect (if (/= (aref vindex engine) -1) (get-current-index-total-pitchcount engine vindex vsolution) 0)))



;;;below functions are not tested yet

(defun get-longest-timebased-engine (vsolution vindex vdefault-engine-order)
  (declare (type array vsolution vindex vdefault-engine-order))
  (let ((max-total-duration (apply 'max
                                   (loop for engine in (aref vdefault-engine-order 4)
                                         collect (get-current-index-endtime engine vindex vsolution)))))
    (declare (type number max-total-duration))
    (loop for engine in (aref vdefault-engine-order 4)
          while (/= (get-current-index-endtime engine vindex vsolution) max-total-duration)
          finally (return engine))))


(defun get-shortest-timebased-engine (vsolution vindex vdefault-engine-order)
  (declare (type array vsolution vindex vdefault-engine-order))
  (let ((min-total-duration (apply 'min
                                   (loop for engine in (aref vdefault-engine-order 4)
                                         collect (get-current-index-endtime engine vindex vsolution)))))
    (declare (type number min-total-duration))
    (loop for engine in (aref vdefault-engine-order 4)
          while (/= (get-current-index-endtime engine vindex vsolution) min-total-duration)
          finally (return engine))))

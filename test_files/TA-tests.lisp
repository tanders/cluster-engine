
(require :cluster-engine)

#|
(ClusterEngine  10 t nil nil '((4 4)) '((1/4) (1/8)  (1/16))  nil)

(ClusterEngine  10 t nil (R-RHYTHMS-ONE-VOICE #'(lambda (x y) (= x y)) 0 :durations) '((4 4)) '((1/4) (1/8)  (1/16))  nil)
|#


(defun preview-cluster-engine-score (score)
  "Dummy def -- in my code this function notates and plays the score in Opusmodus."
  score)


(preview-cluster-engine-score
 (ce::clusterengine 
  10 t nil 
  ;; all rhythmic values are equal
  (ce::R-rhythms-one-voice #'(lambda (x y) (= x y)) 0 :durations)
  '((3 4)) 
  '(((1/4) (1/8) (1/16))
    ((60) (61))))
 )


;; Cluster Engine tutorial 5f 
(preview-cluster-engine-score
 (ce::clusterengine 
  20 t nil
  (ce::R-rhythms-one-voice-at-timepoints 
   #'(lambda (x) (equal x '(0 1/4))) 
   0 '(2) :dur-start)
  '((4 4)) 
  '(((1/4) (1/8) (1/16) (3/8)) 
    ((60) (m 7 -3) (m -7 3)))))

;; TODO: TMP test
(let* ((seed 1)
       (*random-state* (ta-utils:read-random-state seed)))
  (preview-cluster-engine-score
   (ce::clusterengine 
    20 t nil
    (ce::R-rhythms-one-voice-at-timepoints 
     #'(lambda (x)
	 ;; (break)
	 ;; (equal x '(-1/8 1/4))
	 (equal x '(0 1/4))
	 ) 
     0 '(2) :dur-start)
    '((4 4)) 
    '(((1/4) (1/8) (1/16) (3/8)) 
      ((60) (m 7 -3) (m -7 3))))))

;; TODO: TMP test
(defun motif-ends-at-stoptime (motifs-end)
  (let ((motif-end-offset (first motifs-end))
	(motif-rhythm (second motifs-end)))
    (break)
    (= motif-end-offset 0)
    ))
(let* ((seed 2)
       (*random-state* (ta-utils:read-random-state seed)))
  (preview-cluster-engine-score
   (ce::clusterengine 
    20 t nil
    (ce::R-rhythms-one-voice-at-timepoints
     ;; #'motif-ends-at-stoptime
     #'(lambda (x)
     	 (break)
     	 ;; (equal x '(-1/8 1/4))
     	 (equal (first x) 0)
     	 ) 
     0 '(2)
     :dur-start
     ;; Perhaps only used to pitch motifs??
     ;; :motif-start
     ;; :motif-end
     )
    '((4 4)) 
    '(; ((1/4) (1/8) (1/16) (3/8))
      ((1/4) (1/8) (-1/8))
      ((60)
       ;; (m 7 -3) (m -7 3)
       )))))

;; Cluster Engine tutorial 7c
(preview-cluster-engine-score
 (ce::clusterengine 
  10 t t 
  (append 
   (ce::R-metric-hierarchy 0 :durations) 
   (ce::R-note-meter #'(lambda (x)
                         (if (= (first x) 1/4) (= (second x) 0) t))
                     0 :d_offs :beats :incl-rests :normal) )
  '((4 4)) 
  '(((1/12) (1/16) (1/8) (1/4)) 
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)))))


;; Cluster Engine tutorial 8a 
(preview-cluster-engine-score
 (ce::clusterengine 
  12 t nil 
  (append 
   (ce::R-pitches-one-voice 
    #'(lambda (x) 
        (not (member (mod (car (last x)) 12) (mapcar #'(lambda (a) (mod a 12)) (butlast x)) )))
    '(0 1) :all-pitches)
   (ce::R-pitch-pitch 
    #'(lambda (x) 
        (member (mod (ce::apply-minus x) 12) '(3 4 7 8 9))) '(0 1) '(0) :all :no_grace :pitch)
   (ce::R-pitch-pitch #'(lambda (x) (>= (first x) (second x)) ) '(0 1) '(0) :all :no_grace :pitch)) 
  '((4 4)) 
  '(((1/4)) 
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)) 
    ((1/4)) 
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))
 )


;; r-pitch-pitch 1st-beat
(preview-cluster-engine-score
 (ce::clusterengine 
  12 t nil 	
  (ce::R-pitch-pitch 
   #'(lambda (x) 
       (member (mod (- (first x) (second x)) 12) '(3 4 7 8 9)))
   '(0 1) '(0) :1st-beat :no_grace :pitch)
  '((4 4)) 
  '(((1/4)) 
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)) 
    ((1/4)) 
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)))))



(preview-cluster-engine-score
 (ce:ClusterEngine 
  21 t nil 
  (append (ce:R-metric-hierarchy 0 :durations)          
          (ce:r-note-meter #'(lambda (x) (progn (print x)
                                           (if (equal '(4 4) (third x))
                                             (if (member (mod (fourth x) 12)
                                                         '(0 2 4 5 7 9 11)) t nil)
                                             (if (member (mod (fourth x) 12)
                                                         '(1 3 5 6 8 10 11)) t nil)  )))                           
                           0 :d_offs_m_n :beats :incl-rests :normal :true/false)         
          (ce:r-pitches-one-voice #'(lambda (x y)
                                      (< (abs (- x y)) 5)                                  
                                      )
                                  0 :pitches)         
          )
  (ce:metric-domain '(4 4) '(1 2 3 4) nil '(6 8) '(1 3) 3/8)  
  '(((1/12)(1/16)(1/8)(1/4)) ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)))))



;; heuristic -- quasi profile with input-mode :all-pitches raising pitches (when max domain value is reached then start from scratch)
(preview-cluster-engine-score
 (ce::clusterengine 
  20 t nil 
  (cr::HR-pitches-one-voice 
   #'(lambda (pitches) 
       (if (and (>= (length pitches) 2)
                (< (first (last pitches 2)) 
                   (second (last pitches 2))))      
         100
         0))
   0
   :all-pitches)
  '((4 4)) 
  '(((1/4))
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))
 )

;; same as above, but this time with rests
(preview-cluster-engine-score
 (ce::clusterengine 
  20 t nil 
  (ce::HR-pitches-one-voice 
   #'(lambda (pitches) 
       (if (and (>= (length pitches) 2)
                (< (first (last pitches 2)) 
                   (second (last pitches 2))))      
         100
         0))
   0
   :all-pitches)
  '((4 4)) 
  '(((1/4 1/4 -1/4 1/4 1/4))
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)))))


;; version with input-mode :pitches
(preview-cluster-engine-score
 (ce::clusterengine 
  20 t nil 
  (ce::HR-pitches-one-voice 
   #'(lambda (pitch1 pitch2) 
       (if (< pitch1 pitch2)
         100
         0))
   0
   :pitches)
  '((4 4)) 
  '(((1/4 1/4 -1/4 1/4 1/4))
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))
 )


;;;
;;; Test ties
;;;

;;; test ties -- pitches are nil??? Number of variables not large enough
(preview-cluster-engine-score
 (ce::clusterengine  
  10 t nil
  ()
  '((2 4))
  '(((-1/2 3/2 -1/2 -1/2 -1/2 3/2))
    ((72) (73) (74)))))

;; How large does no-of-variables need to be to have exactly one rhythm motif occurance in the solution and all note pitches assigned?
(preview-cluster-engine-score
 (ce::clusterengine  
  18 t nil
  ()
  '((2 4))
  '(((1/2 3/2 -1/2 -1/2 -1/2 3/2))
    ((72) (73) (74)))))

;;; test ties -- pitches are nil???
(preview-cluster-engine-score
 (ce::clusterengine  
  10 t nil
  ()
  '((2 4))
  '(((-1/6 1/6 1/6 1/4 1/4 1/6 1/6 1/6 1/2 -1/4 1/4 1/4 -1/4 -1/10 -1/10 1/10 1/10 1/10 1/4 1/4 1/5 1/10 1/10 1/10 1/2))
    ((72) (73) (74))
    ;; ((-1/2 3/2 -1/2 -1/2 -1/2 3/2))
    ;; ((60) (61) (62))
    )))



;;;
;;; Example with a contradiction -- no solution
;;;

;;
;; Note: if there is no solution, clusterengine should perhaps better return so uniform value meaning that there is no solution
;; Instead, it returns various values that simply miss the information that could not be found
;;

;; failed rhythm rule; non-empty rhythm and empty pitch domain: no pitches and no rhythm returned
(ce::clusterengine 
  10 t nil 
  ;; all rhythmic value paris are increasing
  (ce::R-rhythms-one-voice #'(lambda (x y) (< x y)) 0 :durations)
  '((3 4)) 
  '(((1/4) (1/8))
    () 
    ))
; => :no-solution
; OLD: => (nil nil ((3 4)))

;; failed rhythm rule; non-empty rhythm and non-empty pitch domain: no pitches and no rhythm returned
(ce::clusterengine 
  10 t nil 
  ;; all rhythmic value paris are increasing
  (ce::R-rhythms-one-voice #'(lambda (x y) (< x y)) 0 :durations)
  '((3 4)) 
  '(((1/4) (1/8))
    ((60) (61))
    ))
; => :no-solution
; OLD: => (nil nil ((3 4)))

;; failed rhythm rule; single rhythm domain value, and non-empty pitch domain: one rhythmic value by no pitches returned, though pitch list is now (nil)
(ce::clusterengine 
  10 t nil 
  ;; all rhythmic value paris are increasing
  (ce::R-rhythms-one-voice #'(lambda (x y) (< x y)) 0 :durations)
  '((3 4)) 
  '(((1/4)) 
    ((60) (61))
    ))
; => :no-solution
; OLD: => ((1/4) (nil) ((3 4)))


;;; Contrast: purely rhythmic CSP (no rules here for simplicity)
;;; NOTE: The last above example result cannot be distinguished from a purely rhythmic CSP
(ce::clusterengine 
  10 t nil 
  ;; all rhythmic value paris are increasing
  () 
  '((3 4)) 
  '(((1/4) (1/8)) 
    ()
    ))
; => ((1/8 1/4 1/8 1/8 1/4 1/4 1/8 1/8 1/8 1/4) (nil nil nil nil nil nil nil nil nil nil) ((3 4) (3 4) (3 4)))


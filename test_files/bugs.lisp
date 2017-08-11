
;;; Examples that seem to show bugs

;;; BUG: metric domain of multiple values seemingly always results in static meter
(preview-cluster-engine-score
 (ce::clusterengine 
  20 t nil 
  ()
  '((3 4) (2 4)) 
  '(((1/4) (1/8))
    ((60))))
 )

;; OK: heuristic -- raising pitches (when max domain value is reached then start from scratch)
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

;;; BUG: "profile rule" always starts from scatch after a rest. Also, why always same profile followed?
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
  '(((1/4 1/4 -1/4))
    ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))
 )



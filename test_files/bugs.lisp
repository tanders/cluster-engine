
;;; Examples that seem to show bugs

;;; BUG: metric domain of multiple values seemingly always results in static meter
(preview-score
 (cluster-engine-score
   (ce::clusterengine 
    20 t nil 
    ()
    '((3 4) (2 4)) 
    '(((1/4) (1/8))
      ((60))))
   ))


(in-package cluster-engine) 

<<<<<<< HEAD
(print
 (clusterengine 
=======
(in-package cluster-engine) 

(preview-cluster-engine-score
 (ce:clusterengine 
>>>>>>> 11b6783f7b1bb55ce3040263404ccb22b4e71dcd
  16 t nil 
  '()  ; no rules
  '((4 4)) 
  '(((1/4))
    ;; chord domain
    (((60 64 67)) ((65 69 72)) ((67 71 74)(69 72 76)))
    )))

(print
 (clusterengine 
  16 t nil 
  '()  ; no rules
  '((4 4)) 
  '(((1/4) (1/8))
    ;; chord sequence motif
    ((60)(m 7 -3)(m -7 3))
    )))


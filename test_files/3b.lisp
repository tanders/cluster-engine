
(in-package cluster-engine) 

(preview-cluster-engine-score
 (ce:clusterengine 
  16 t nil 
  '()  ; no rules
  '((4 4)) 
  '(((1/4))
    ;; chord domain
    (((67 71 74)) ((69 72 76)))
    )))

(preview-cluster-engine-score
 (ce:clusterengine 
  16 t nil 
  '()  ; no rules
  '((4 4)) 
  '(((1/4) (1/8))
    ;; chord sequence motif
    (((67 71 74) (69 72 76)))
    )))


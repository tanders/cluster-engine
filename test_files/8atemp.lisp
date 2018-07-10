(in-package cluster-engine) 
(setf *random-state* (make-random-state t)) 

	(print (cluster-engine::ClusterEngine 12 t nil 
		(cluster-engine::R-pitch-pitch #'(lambda (x) (>= (first x) (second x)) ) '(0 1) '(0) :all :no_grace :pitch)                              
                                         	
		
		'((4 4)) 
		'(((1/4)) ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)) 
			((1/4)) ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79)))) )

		
	
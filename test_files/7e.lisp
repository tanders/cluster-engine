(in-package cluster-engine) 
(setf *random-state* (make-random-state t)) 

	(print (cluster-engine::ClusterEngine 21 t nil 
		(append (cluster-engine::R-metric-hierarchy 0 :durations) 
			
                        (r-note-meter #'(lambda (x)
                                          (if (equal '(4 4) (third x))
                                              (member (pw::g-mod (fourth x) 12)
                                                      '(0 2 4 5 7 9 11))
                                              (member (pw::g-mod (fourth x) 12)
                                                      '(1 3 5 6 8 10 11))))
                                              
                                0 :d_offs_m_n :beats :incl-rests :normal)
                        
                        (r-pitches-one-voice #'(lambda (x y)
                                                 (< (pw::g-abs (pw::g- x y)) 5)
                                                 
                                                 )
                                0 :pitches)

		)

		(cluster-engine::metric-domain '(4 4) '(1 2 3 4) nil '(6 8) '(1 3) 3/8)  

                '(((1/12)(1/16)(1/8)(1/4)) ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))
	)
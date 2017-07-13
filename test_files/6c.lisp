(in-package cluster-engine) (setf *random-state* (make-random-state t)) 


(print

	(ClusterEngine 10 t nil 
		(r-rhythm-pitch-one-voice 
                         #'(lambda (x y) 

                             (if (second y)
                                 (if (minusp (first x))
                                     (= (second y) 60)
                                     (> (second y) (second x)))
                                 t)

                             ) 

                         0 :include-rests :exclude-gracenotes) 
			
		'((4 4)) 

		'(((1/16) (1/8) (1/4) (-1/8)) 
                  
                  ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))


                  )
                )
        
	)


#| `(((1/16) (1/8) (1/4) (-1/8)) 
                  
                  ,(mapcar #'list (pw::arithm-ser 60 1 79))


                  )

|#
(in-package cluster-engine) 
(setf *random-state* (make-random-state t)) 

(print
 (ClusterEngine 35 t nil 
		(append (R-pitches-one-voice (lambda (x y) (member (abs (- x y)) '(1 2 3 4)))
					     '(0 1) :pitches) 
			(R-chords '(0 1 2) '((4 7) (3 7)) '(0) :all :exclude-gracenotes )
			(R-pitch-pitch (lambda (x) (>= (first x) (second x)))
				       '((0 1) (1 2)) '(0) :beat :no_grace :pitch)
			(R-pitch-pitch (lambda (x) (= (length (remove-duplicates (pw::g-mod x 12))) 3))
				       '(0 1 2) '(0) :beat :no_grace :pitch) 
			) 
		'((4 4)) 
		'(((1/4)) ((60) (62) (64) (65) (67) (69) (71) (72) (74) (76) (77) (79) (81) (83) (84))
		  ((1/4)) ((60) (62) (64) (65) (67) (69) (71) (72) (74) (76) (77) (79) (81) (83) (84))
		  ((1/4)) ((60) (62) (64) (65) (67) (69) (71) (72) (74) (76) (77) (79) (81) (83) (84))
		  ))
 )

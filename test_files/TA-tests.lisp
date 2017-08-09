
(require :cluster-engine)

#|
(ClusterEngine  10 t nil nil '((4 4)) '((1/4) (1/8)  (1/16))  nil)

(ClusterEngine  10 t nil (R-RHYTHMS-ONE-VOICE #'(lambda (x y) (= x y)) 0 :durations) '((4 4)) '((1/4) (1/8)  (1/16))  nil)
|#


(defun preview-cluster-engine-score (score)
  "Dummy def -- in my code this function notates and plays the score in Opusmodus."
  score)


(preview-cluster-engine-score
 (ce::ClusterEngine 
  10 t nil 
  ;; all rhythmic values are equal
  (ce::R-rhythms-one-voice #'(lambda (x y) (= x y)) 0 :durations)
  '((3 4)) 
  '(((1/4) (1/8))
    ((60) (61))))
 )


;; Cluster Engine tutorial 5f 
(preview-cluster-engine-score
 (ce::ClusterEngine 
  20 t nil
  (ce::R-rhythms-one-voice-at-timepoints 
   #'(lambda (x) (equal x '(0 1/4))) 
   0 '(2) :dur-start) 
  '((4 4)) 
  '(((1/4) (1/8) (1/16) (3/8)) 
    ((60) (m 7 -3) (m -7 3)))))


;; Cluster Engine tutorial 7c
(preview-cluster-engine-score
 (ce::ClusterEngine 
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
 (ce::ClusterEngine 
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
 (ce::ClusterEngine 
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



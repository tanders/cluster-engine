(in-package cluster-engine) 
(setf *random-state* (make-random-state t)) 




(defun rule-3-engines-notes-and-their-offset-to-metric-structure-ignor-rests-and-gracenotes-indicate-timesignature (simple-rule rhythm-engine1 pitch-engine3 flag-for-metric-access)
  "Formats a rule for offsets for notes (ignoring rests), i.e. how notes are positioned in relation to the pulse.
The rule will also access the duration ratio, the timesignature for the measure where the events onset exist and pitches. Grace notes and rests are ignored 
(they will be skipped: the rule will not know that they exist).
"
  (let ((no-of-args (length (function-lambda-list simple-rule)))
        (metric-timepoints (cond ((= flag-for-metric-access 1) ; all beats
                                  (list 'mapcar '(quote abs) (list 'the 'list (list 'aref 'vlinear-solution 'metric-engine2 1))))
                                 ((= flag-for-metric-access 2) ; only first beat in measures
                                  (list 'remove-if '(quote minusp) (list 'the 'list (list 'aref 'vlinear-solution 'metric-engine2 1)))))))
(print 'test-confirmed)
    (list 'lambda '(vsolution vlinear-solution vindex vsolution-for-backjump vbackjump-indexes engine)
          '(declare (type array vsolution vlinear-solution vindex vsolution-for-backjump vbackjump-indexes))
          '(declare (type fixnum engine))

        ;find the information to check
          (list 'block 'this-rule
                (list 'let '((metric-engine2 (1- (the fixnum (array-dimension vindex 0))))
                             events-plus-preceding-for-extra-args-engine1
                             pitches-plus-preceding-for-extra-args-engine3
                             list-of-offsets
                             list-of-timesigns
                             timepoints-for-backjump)
                      '(declare (type list list-of-offsets timepoints-for-backjump events-plus-preceding-for-extra-args-engine1 pitches-plus-preceding-for-extra-args-engine3 list-of-timesigns))
                      '(declare (type fixnum metric-engine2))
                
                      (list 'cond (list (list '= 'engine rhythm-engine1)
                                        (list 'when (list 'not (list 'aref 'vlinear-solution 'metric-engine2 1)) '(return-from this-rule t))
                                        (list 'let* (list (list 'this-cell-onsets-plus-preceding-for-extra-args 
                                                                (list 'butlast ;remove last events end-time (this might be the start of a rest)
                                                                      (list 'the 'list (list 'get-timepoints-from-start-last-rhythmcell-minus-nsteps-ignor-rests-and-gracenotes ;keep grace notes
                                                                                             rhythm-engine1 'vindex 'vsolution 'vlinear-solution (1- no-of-args)))))
                                                          (list 'this-cell-events-plus-preceding-for-extra-args 
                                                                (list 'the 'list (list 'get-events-from-start-last-rhythmcell-minus-nsteps-ignor-rests-and-gracenotes 
                                                                                       rhythm-engine1 'vindex 'vsolution 'vlinear-solution (1- no-of-args))))
                                                          (list 'this-cell-notecounts-plus-preceding-for-extra-args 
                                                                (list 'the 'list (list 'get-notecounts-from-start-last-rhythmcell-minus-nsteps-ignor-rests-and-gracenotes rhythm-engine1 'vindex 'vsolution 'vlinear-solution (1- no-of-args))))

                                                          (list 'beats-metric-engine2 metric-timepoints) ;this is set as an input option (see above) 
                                                                                                       ;keep the endtime since it is a grid (no rests possible)
                                                          (list 'end-time-metric-engine2 (list 'car (list 'last (list 'the 'list (list 'aref 'vlinear-solution 'metric-engine2 1)))))
                                                          (list 'engine1-timepoints-to-check '(filter-timepoints-keep-before-endtime ;this is specific for when timesign is indicated - timesign is not knowsn at endpoint
                                                                                               this-cell-onsets-plus-preceding-for-extra-args end-time-metric-engine2))
                                                          (list 'matching-or-following-timepoints '(find-all-timepoints2-convert-rests ; the convert rest is unnecessary, but does not use up much power
                                                                                                    engine1-timepoints-to-check beats-metric-engine2))
                                                          (list 'total-pitchcount (list 'get-total-pitchcount pitch-engine3 'vlinear-solution)))
                                              '(declare (type list this-cell-onsets-plus-preceding-for-extra-args this-cell-events-plus-preceding-for-extra-args this-cell-notecounts-plus-preceding-for-extra-args
                                                              beats-metric-engine2 engine1-timepoints-to-check matching-or-following-timepoints))
                                              '(declare (type number end-time-metric-engine2))
                                              '(declare (type fixnum total-pitchcount))

                                              '(when (not matching-or-following-timepoints) (return-from this-rule t))

                                              (list 'setf 'events-plus-preceding-for-extra-args-engine1
                                                    (list 'filter-durations-keep-upto-endtime 'this-cell-events-plus-preceding-for-extra-args 'this-cell-onsets-plus-preceding-for-extra-args 'end-time-metric-engine2))
                                              (list 'setf 'list-of-offsets '(the list (mapcar '- engine1-timepoints-to-check matching-or-following-timepoints)))
                                              (list 'setf 'list-of-timesigns '(the list (get-time-signature-at-timepoints metric-engine2 vsolution vlinear-solution engine1-timepoints-to-check)))

                                              ;;added here
                                              (list 'setf 'pitches-plus-preceding-for-extra-args-engine3 
                                                    (list 'the 'list (list 'get-pitches-at-notecounts pitch-engine3 'vlinear-solution 'this-cell-notecounts-plus-preceding-for-extra-args 'total-pitchcount)))
                                              ;;

                                              '(setf timepoints-for-backjump engine1-timepoints-to-check)
                                              ))

                            (list (list '= 'engine 'metric-engine2)
                                  (list 'when (list 'not (list 'aref 'vlinear-solution rhythm-engine1 1)) '(return-from this-rule t))
                                  (list 'let* (list (list 'this-cell-starttime (list 'get-current-index-starttime 'metric-engine2 'vindex 'vsolution))
                                                    (list 'onsets-plus-preceding-for-extra-args-engine1
                                                          (list 'butlast ;remove last events end-time (this might be the start of a rest)
                                                                (list 'the 'list (list 'get-timepoints-from-any-timepoint-minus-nsteps-ignor-rests-and-gracenotes 
                                                                                       rhythm-engine1 'vindex 'vlinear-solution 'this-cell-starttime (1- no-of-args)))))
                                                    (list 'events-plus-preceding-for-extra-args-matching-onsets 
                                                          (list 'the 'list (list 'get-events-from-any-timepoint-minus-nsteps-ignor-rests-and-gracenotes 
                                                                                 rhythm-engine1 'vindex 'vlinear-solution 'this-cell-starttime (1- no-of-args))))
                                                    (list 'notecounts-plus-preceding-for-extra-args-matching-onsets 
                                                          (list 'the 'list (list 'get-notecounts-from-any-timepoint-minus-nsteps-ignor-rests-and-gracenotes 
                                                                                 rhythm-engine1 'vindex 'vlinear-solution 'this-cell-starttime (1- no-of-args))))
                                                    (list 'beats-metric-engine2 metric-timepoints) ;keep the endtime since it is a grid (no rests possible)
                                                    (list 'end-time-metric-engine2 (list 'car (list 'last (list 'the 'list (list 'aref 'vlinear-solution 'metric-engine2 2)))))
                                                    (list 'engine1-timepoints-to-check '(filter-timepoints-keep-before-endtime ;this is specific for when timesign is indicated
                                                                                         onsets-plus-preceding-for-extra-args-engine1 end-time-metric-engine2))
                                                    (list 'matching-or-following-timepoints '(find-all-timepoints2-convert-rests ; the convert rest is unnecessary, but does not use up much power
                                                                                              engine1-timepoints-to-check beats-metric-engine2))
                                                    (list 'total-pitchcount (list 'get-total-pitchcount pitch-engine3 'vlinear-solution)))

                                        '(declare (type list onsets-plus-preceding-for-extra-args-engine1 events-plus-preceding-for-extra-args-matching-onsets notecounts-plus-preceding-for-extra-args-matching-onsets
                                                        beats-metric-engine2 engine1-timepoints-to-check matching-or-following-timepoints))
                                        '(declare (type number this-cell-starttime))
                                        '(declare (type fixnum total-pitchcount))

                                        '(when (not beats-metric-engine2) (return-from this-rule t))
                                        ;nothing to check - exist to avoid errors
                                        '(when (not (car matching-or-following-timepoints)) (return-from this-rule t))
                                        ;this meanes that engine2 starts with rests - this is NOT CHECKED (use the rule that includes rests to check this...)

                                        (list 'setf 'events-plus-preceding-for-extra-args-engine1
                                              (list 'filter-durations-keep-upto-endtime 'events-plus-preceding-for-extra-args-matching-onsets 'onsets-plus-preceding-for-extra-args-engine1 'end-time-metric-engine2))
                                        (list 'setf 'list-of-offsets '(the list (mapcar '- engine1-timepoints-to-check matching-or-following-timepoints)))
                                        (list 'setf 'list-of-timesigns '(the list (get-time-signature-at-timepoints metric-engine2 vsolution vlinear-solution engine1-timepoints-to-check)))
                                      
                                        ;;added here
                                        (list 'setf 'pitches-plus-preceding-for-extra-args-engine3 
                                              (list 'the 'list (list 'get-pitches-at-notecounts pitch-engine3 'vlinear-solution 'notecounts-plus-preceding-for-extra-args-matching-onsets 'total-pitchcount)))
                                        ;;   
                                        '(setf timepoints-for-backjump engine1-timepoints-to-check)
                                        ))
                            (list (list '= 'engine pitch-engine3) 
                                  (list 'when (list 'not (list 'aref 'vlinear-solution 'metric-engine2 1)) '(return-from this-rule t))
                                  (list 'when (list 'not (list 'aref 'vlinear-solution rhythm-engine1 1)) '(return-from this-rule t))  
                                  (list 'let* (list (list 'first-pitchcount-this-cell (list 'get-current-index-first-pitchcount pitch-engine3 'vindex 'vsolution))
                                                    (list 'first-timepoint-at-first-pitchcount (list 'get-timepoint-at-notecount rhythm-engine1 'vlinear-solution 'first-pitchcount-this-cell)))
                                        '(declare (type fixnum first-pitchcount-this-cell))
                                        '(declare (type t first-timepoint-at-first-pitchcount))

                                        ;check if there are any new timepoints that can be checked
                                        '(when (not first-timepoint-at-first-pitchcount) (return-from this-rule t))

                                        (list 'let* (list (list 'total-pitchcount (list 'get-total-pitchcount pitch-engine3 'vlinear-solution))
                                                          (list 'end-time-pitchcount (list 'get-offset-timepoint-at-notecount-include-final-rest rhythm-engine1 'vlinear-solution 'total-pitchcount))
                                                          (list 'end-time-rhythm-with-pitch (list 'if 'end-time-pitchcount 'end-time-pitchcount
                                                                                                  (list 'get-current-index-endtime rhythm-engine1 'vindex 'vsolution)))
                                                          (list 'onsets-plus-preceding-for-extra-args-engine1
                                                                (list 'butlast ;remove last events end-time (duration is onknown on teh last endtime)
                                                                      (list 'the 'list (list 'get-timepoints-from-any-timepoint-minus-nsteps-ignor-rests-and-gracenotes 
                                                                                             rhythm-engine1 'vindex 'vlinear-solution 'first-timepoint-at-first-pitchcount (1- no-of-args)))))
                                                          (list 'events-plus-preceding-for-extra-args-matching-onsets
                                                                (list 'the 'list (list 'get-events-from-any-timepoint-minus-nsteps-ignor-rests-and-gracenotes 
                                                                                       rhythm-engine1 'vindex 'vlinear-solution 'first-timepoint-at-first-pitchcount (1- no-of-args))))
                                                          (list 'notecounts-plus-preceding-for-extra-args-matching-onsets
                                                                (list 'the 'list (list 'get-notecounts-from-any-timepoint-minus-nsteps-ignor-rests-and-gracenotes
                                                                                       rhythm-engine1 'vindex 'vlinear-solution 'first-timepoint-at-first-pitchcount (1- no-of-args))))
                                                          (list 'beats-metric-engine2 metric-timepoints) ;this is set as an input option (see above) 
                                                                                                       ;keep the endtime since it is a grid (no rests possible)
                                                          (list 'end-time-metric-engine2 (list 'car (list 'last (list 'the 'list (list 'aref 'vlinear-solution 'metric-engine2 1)))))
                                                          (list 'engine1-timepoints-to-check '(filter-timepoints-keep-before-endtime ;this is specific for when timesign is indicated 
                                                                                               onsets-plus-preceding-for-extra-args-engine1 end-time-metric-engine2))
                                                          (list 'matching-or-following-timepoints '(find-all-timepoints2-convert-rests ; the convert rest is unnecessary, but does not use up much power. Thiese are metric points.
                                                                                                    engine1-timepoints-to-check beats-metric-engine2)))
                                              '(declare (type fixnum total-pitchcount))
                                              '(declare (type t end-time-pitchcount))
                                              '(declare (type number end-time-rhythm-with-pitch end-time-metric-engine2))
                                              '(declare (type list onsets-plus-preceding-for-extra-args-engine1 events-plus-preceding-for-extra-args-matching-onsets notecounts-plus-preceding-for-extra-args-matching-onsets
                                                              beats-metric-engine2 engine1-timepoints-to-check matching-or-following-timepoints))
                                              '(when (not (car matching-or-following-timepoints)) (return-from this-rule t))
                                                  ;this meanes that engine2 starts with rests - this is NOT CHECKED (use the rule that includes rests to check this...)

                                              (list 'setf 'events-plus-preceding-for-extra-args-engine1
                                                    (list 'filter-durations-keep-upto-endtime 'events-plus-preceding-for-extra-args-matching-onsets 'onsets-plus-preceding-for-extra-args-engine1 'end-time-metric-engine2))

                                              (list 'setf 'list-of-offsets '(the list (mapcar '- engine1-timepoints-to-check matching-or-following-timepoints)))
                                              (list 'setf 'list-of-timesigns '(the list (get-time-signature-at-timepoints metric-engine2 vsolution vlinear-solution engine1-timepoints-to-check)))
                            
                                              ;;added here
                                              (list 'setf 'pitches-plus-preceding-for-extra-args-engine3 
                                                    (list 'the 'list (list 'get-pitches-at-notecounts pitch-engine3 'vlinear-solution 'notecounts-plus-preceding-for-extra-args-matching-onsets 'total-pitchcount)))
                                              ;;                                                  
                                      
                                              '(setf timepoints-for-backjump engine1-timepoints-to-check)
                                              ))))
                                                  

                      ;here is the rule test
                      (list 'loop 'for 'nth-variable 'from 0 
                            'to (list 'the 'fixnum (list '- '(min (1- (length list-of-offsets)) ;this includes timepoint in both rhythm and meter
                                                                  (1- (length pitches-plus-preceding-for-extra-args-engine3)))
                                                         (1- no-of-args)))
                            'do (list 'when (list 'not (list 'apply (compile-if-not-compiled nil simple-rule) 
                                                             (list 'loop 'for 'n 'from 0 'to (1- no-of-args)
                                                                   'collect (list 'the 'list (list 'list (list 'nth '(+ n nth-variable) 'events-plus-preceding-for-extra-args-engine1)
                                                                                                   (list 'nth '(+ n nth-variable) 'list-of-offsets)
                                                                                                   (list 'nth '(+ n nth-variable) 'list-of-timesigns)
                                                                                                   (list 'nth '(+ n nth-variable) 'pitches-plus-preceding-for-extra-args-engine3))))))
                                      (list 'progn 
                                            ;backjump
                                            (list 'set-vbackjump-indexes-from-failed-timepoint-meter-duration-pitch 
                                                  (list 'the 'number (list 'nth (list '+ 'nth-variable (1- no-of-args)) 'timepoints-for-backjump))
                                                  'engine rhythm-engine1 'metric-engine2 pitch-engine3 'vbackjump-indexes 'vsolution-for-backjump 'vlinear-solution)
                                            '(return nil)))
                            'finally '(return t)
                            )
                      )))
    ))







	(print (cluster-engine::ClusterEngine 21 t nil 
		(append (cluster-engine::R-metric-hierarchy 0 :durations) 
			
                        (r-note-meter #'(lambda (x) (progn (print x)
                                          (if (equal '(4 4) (third x))
                                              (if (member (mod (fourth x) 12)
                                                      '(0 2 4 5 7 9 11)) t nil)
                                              (if (member (mod (fourth x) 12)
                                                      '(1 3 5 6 8 10 11)) t nil)  )))
                                              
                                0 :d_offs_m_n :beats :incl-rests :normal :true/false)
                        
                        (r-pitches-one-voice #'(lambda (x y)
                                                 (< (pw::g-abs (pw::g- x y)) 5)
                                                 
                                                 )
                                0 :pitches)

		)

		(cluster-engine::metric-domain '(4 4) '(1 2 3 4) nil '(6 8) '(1 3) 3/8)  

                '(((1/12)(1/16)(1/8)(1/4)) ((60) (61) (62) (63) (64) (65) (66) (67) (68) (69) (70) (71) (72) (73) (74) (75) (76) (77) (78) (79))))
	)
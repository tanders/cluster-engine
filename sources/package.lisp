(defpackage cluster-engine
  #+SBCL (:use #:common-lisp #:sb-ext)
  #+PWGL (:use #:common-lisp #:lw)
  #-(or SBCL PWGL) (:use #:common-lisp) 
  (:nicknames :ce)
  (:export #:ClusterEngine #:Rules->Cluster  ; Cluster Engine
	   ;; domain, debug and more
	   #:metric-domain #:R-predefine-meter #:Stop-rule-time #:Stop-rule-index #:CLUSTERdebug #:preferences
	   ;; rules one engine
	   #:R-rhythms-one-voice #:R-index-rhythms-one-voice #:R-pitches-one-voice #:R-index-pitches-one-voice #:R-time-signatures #:R-index-time-signatures #:R-only-m-motifs #:R-rhythms-one-voice-at-timepoints #:HR-rhythms-one-voice #:HR-index-rhythms-one-voice #:HR-pitches-one-voice #:HR-index-pitches-one-voice #:HR-time-signatures #:HR-index-time-signatures #:R-pmc-one-voice #:R-jbs-one-voice #:set-end
	   ;; rules one voice
	   #:R-rhythm-pitch-one-voice #:R-index-rhythm-pitch-one-voice #:R-metric-hierarchy #:R-note-meter #:R-meter-note #:R-mel-interval-one-voice #:HR-rhythm-pitch-one-voice #:HR-index-rhythm-pitch-one-voice #:HR-duration-meter #:HR-meter-duration
	   ;; rules two voices
	   #:R-rhythm-rhythm #:R-rhythm-hierarchy #:R-canon #:HR-rhythm-rhythm
	   ;; rules two or more voices
	   #:R-pitch-pitch #:R-chords #:R-list-all-events #:HR-pitch-pitch #:HR-list-all-events
	   ;; utilities
	   #:apply-and #:apply-minus #:first-n #:test-seq-follows-markov-chain? #:test-seq-follows-energy-profile?
	   #:*verbose-i/o?* #:*cluster-engine-log-output*
	   ;; forward rules
	   #:fwd-rule_left-to-right_type-tie-breaking #:fwd-rule6B
	   )
  )


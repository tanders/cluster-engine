;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ClusterEngine Library version 0.1542
;;; Copyright (c) 2013, �rjan Sandred.  All rights reserved.
;;;
;;; This is an experimental library, and a work in progress
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; version 0.1 (Uppsala April 14, 2010)
;;; version 0.12 (Winnipeg April 3, 2011)
;;; version 0.13 (Winnipeg May 30, 2011)
;;; version 0.14 
;;;    bugfix: r-duration-meter and  hr-duration-meter (the incl-rests, normal setting)
;;;    changed behaviour of r-rhythm-rhythm
;;;    new box: r-meter-note (replaces r-meter-duration)
;;;    r-duration-meter now also can check the time signature
;;; version 0.141 (Paris July 4, 2011)
;;;    bugfix: get-rhythm-motifs-from-index-to-current-index-nth (affected r-rhythms-one-voice)
;;;    added: test-seq-follows-markov-chain?, test-seq-follows-energy-profile?
;;;     r-time-signatures, r-index-time-signatures, hr-time-signatures, hr-index-time-signatures, 
;;;     r-only-m-motifs
;;;    new box: r-note-meter (replaces r-duration-meter)
;;; version 0.142 (Uppsala July 16, 2011)
;;;    r-meter-note has two new input-modes: offset-motif and offset-motif-meter (also works for heuristic rule-types, but not in the list input mode) 
;;; version 0.143 
;;;    r-meter-note has two new input-modes: offset-dur-meter and offset-dur-pitch-meter (also works for heuristic rule-types, but not in the list input mode) 
;;; version 0.15 (Uppsala July 7, 2012)
;;;    added: r-list-all-events, hr-list-all-events, r-pitch-pitch, hr-pitch-pitch, r-chords, r-mel-interval-one-voice
;;;    removed r-pitch-pitch-all, r-pitch-pitch-on-beats, r-pitch-pitch-on-1st-beats
;;;    bugfix: global variable *bktr-rr2v* not correctly initiated, renamed *bktr-dm2v* and *bktr-md2v* to *bktr-dm1v* and *bktr-md1v* to better reflect what they are
;;; version 0.151 (Saline Royale July 12, 2012)
;;;    added: r-rhythms-one-voice-at-timepoints, first-n
;;; version 0.152 (Saline Royale July 14, 2012)
;;;    added: stop-rule-time, r-predefine-meter
;;;    improved r-rhythms-one-voice-at-timepoints
;;; version 0.153 (Uppsala Aug 2012)
;;;    bugfix: count-notes-last-cell-at-current-index-nth (did not work if notes are chords in a voice) - this affected r-rhythm-pitch-one-voice in the heuristic-switch settmode
;;;            first-n (n can now be longer than the list length)
;;; version 0.154 (Paris July 2013)
;;;    added: r-pitch-pitch can now access the duration of a pitch and offsets for events' onsets. r-rhythm-pitch-one-voice can now access absolute onset time.
;;;           metric-domain can now handle a list of lternative beat lengths (i.e. it allows any beat distribution).
;;;    minor fix: removed unnecessary error message at r-pitches-one-voice in the :all setting.
;;; version 0.1541 (Saline Royal July 2013)       
;;;    added: hr-rhythm-pitch-one-voice can now access absolute onset time.  
;;; version 0.1542 (Saline Royal July 2013)       
;;;    added: r-pitch-pitch can now access can now access absolute onset time. The global variable *vindex* was added to mirror the index in the engine.
;;;    improved the stop-time-rule - it can now handle AND and OR.
;;; version 0.1543 (Paris/Uppsala July 2013)     
;;;    added: stop-rule-index, get-index-during-search (not in menu - use with care!!)
;;;    improved: r-rhythms-one-voice and hr-rhythms-one-voice (it can now access the start time of durations or motifs)
;;;              r-pitches-one-voice and r-pitches-one-voice (it can now access the position of pitches or motifs, or the index of motifs)
;;; version 0.155 (St Sauveur December 31 2014)
;;;    The domain can now include chords defined as interval relations. The FIRST pitch in teh definition will be considered having an intervalic relation to preceedingand following pitch.
;;;    Ex. The chords '(0 3 4) and '(2 3 4) are both minor triads, but the first starts at the same pitch as the previous event, while the second example starts a major second higher.


(in-package :asdf)

;; you might want to put this somewhere else:
;; (:optimize ((debug 3) (speed 3) (safety 1)))

;; e.g. 
;; (declaim (optimize ((debug 3) (speed 3) (safety 1))))
;; at the top of each file after in-package

(defsystem cluster-engine
  :version "non-pwgl 0.157"
  :author "Orjan Sandred"
  :description "This is not a public release - use this at your own risk.

This library provides a constraint system that uses several search engines that run in parallel to solve a musical constraint problem. Any number of engines can run in parallel (however the interface limits the user to maximum 21 engines). Variables in different engines can be constrained, and the engines can trigger backtracking in dependant engines to find a solution. 

This system is experimental: Any found solution is always valid, however in some cases the system might not be able to find a solution even if there is one. Typically it is  enough to try again to find a valid solution. 

THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"
  ;; :default-component-class ccl::pwgl-source-file
  :serial t
  :components
  ((:file "package")
   (:file "from-studio-flat")
   ;; (:file "000.box-ptr-for-orjan") ; likely obsolete 
   (:file "01.domain")
   (:file "02.engine")
   (:file "03.Fwd-rules")
   (:file "04.Backtrack-rules")
   (:file "05.rules-interface")
   (:file "05a.rules-interface-1engine")
   (:file "05b.rules-interface-2engines") ;pitch and rhythm
   (:file "05c.rules-interface-2engines") ;rhythm and rhythm
   (:file "05d.rules-interface-2engines") ;hierarchy and metric rules
   (:file "05e.rules-interface-2engines") ;metric rules and duration
   (:file "05f.rules-interface-3engines") ;meter note
   (:file "05g.rules-interface-any-n-engines") ;flexible number of layers, r-pitch-pitch
   (:file "05h.rules-higher-level") ;"ready made" rules
   (:file "05i.rules-stop-search")
   (:file "05n.rules-interface-nn-engines")
   (:file "06.heuristic-rules-interface")
   (:file "06a.heuristic-rules-interface-1engine")
   (:file "06b.heuristic-rules-interface-2engines")
   (:file "06c.heuristic-rules-interface-2engines")
   (:file "06d.heuristic-rules-interface-2engines")
   (:file "06e.heuristic-rules-interface-2engines")
   (:file "06f.heuristic-rules-interface-3engines")
   (:file "06g.heuristic-rules-interface-any-n-engines")
   (:file "07.backjumping")
   (:file "08.decode")
   (:file "09.utilities")
   (:file "09b.markov-tools")
   (:file "09c.cluster-energy-profile")
   ;; (:file "10.PWGL-interface")
   ;; (:file "11.simple-tree")
   (:file "12.debug-tools")
   (:file "13.convert-pmc-rules")
   ;; (:file "14.back-compability")
   ;; (:file "00.menu")
   ;; Files added after moving away from PWGL
   (:file "_000.main-interface")
   ;;; TMP comment -- dependency on two PW functions
   ;; (:file "_001.gen_domains")
))

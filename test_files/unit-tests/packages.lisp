;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
(defpackage #:cluster-engine/tests
  (:use #:cl
        #:cluster-engine
        #:rove))
|#

(defpackage #:cluster-engine/test-utils
  (:use #:cl #:cluster-engine
	;; For defining tests for test utils
	#:FiveAM)
  (:export #:gen-select-one #:gen-selection #:gen-ratio
	   #:*rhythm-domain-template* #:*even-rhythm-domain-template*
	   #:*1/4-rhythm-domain-template* #:*simple-rhythm-domain-template*
	   #:*pitch-domain-template* #:*pitch-pairs* #:*metric-domain-template*
	   #:cluster-shorthand
	   #:get-rhythms #:get-pitches #:get-time-signatures #:get-voices #:get-starts #:get-keyword-voices
	   #:is-note? #:is-rest? #:is-chord?
	   #:get-start #:get-duration #:get-pitch
	   #:get-events-time-points
	   ;; FiveAM test short-hands
	   #:test-harmonic-constraint))


(defpackage #:ta-utilities/redefinitions
  (:documentation "For avoiding dependencies to the latest unpublished version of the ta-utilities library, some of its definitions are copied into the present library, but to clearly mark the code repetition they are put into an extra package here.")
  (:nicknames :tu-redef)
  (:use #:cl)
  (:export #:best-if
	   #:pitch->pc
	   #:pc-transpose-to-0
	   #:pitches->pc-normal-form))


(defpackage #:cluster-engine/tests
  (:use #:cl
        #:cluster-engine
        #:cluster-engine/test-utils
        #:FiveAM)
  (:export #:run!
	   #:all-tests))


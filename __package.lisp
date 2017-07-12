;;;;; BEFORE : RUN SBCL WITH THE CORE OF MOZ LIB

(defpackage cluster-engine (:use :common-lisp :sb-ext))

(in-package :cluster-engine)

;;;; from Studio FLAT ;;;;;;

(defun all-diff? (list)
    "Test that all elements in a list are unique."
  (cond ((null list) t)
        ((member (car list) (cdr list) :test #'equal) nil)
        (t (all-diff? (cdr list)))))


(defun within-deviation? (value tolerance x) ; (value 0)(tolerance 0)(x 0)
    "Test that a value doesn't exceed maximum deviation from x."
    (:groupings '(3)  :x-proportions '((0.3 0.15 0.3)))
  (cond ((> value (+ x tolerance)) nil)
        ((< value (- x tolerance)) nil)
        (t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  :pw  ;;;;;
(load "~/Documents/Dev-GitHub/cluster-engine/01.domain.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/02.engine.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/03.Fwd-rules.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/04.Backtrack-rules.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05.rules-interface.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05a.rules-interface-1engine.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05b.rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05c.rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05d.rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05e.rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05f.rules-interface-3engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05g.rules-interface-any-n-engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05h.rules-higher-level.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05i.rules-stop-search.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/05n.rules-interface-nn-engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06.heuristic-rules-interface.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06a.heuristic-rules-interface-1engine.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06b.heuristic-rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06c.heuristic-rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06d.heuristic-rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06e.heuristic-rules-interface-2engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06f.heuristic-rules-interface-3engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/06g.heuristic-rules-interface-any-n-engines.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/07.backjumping.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/08.decode.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/09.utilities.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/09b.markov-tools.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/09c.cluster-energy-profile.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/12.debug-tools.lisp")
(load "~/Documents/Dev-GitHub/cluster-engine/13.convert-pmc-rules.lisp")

(load "~/Documents/Dev-GitHub/cluster-engine/_000.main-interface.lisp")

;;;;;; PW and Co.
(load "~/Dropbox/cluster-to-Max-Orjan/other-libraries/iter.lisp")
(load "~/Dropbox/cluster-to-Max-Orjan/other-libraries/pw-common-language.lisp")
(load "~/Dropbox/cluster-to-Max-Orjan/other-libraries/pw_profile_functions.lisp")

;;;;;; gen-domains
(load "~/Documents/Dev-GitHub/cluster-engine/_001.gen_domains.lisp")

(in-package :cluster-engine)

(defun cluster-convert-one-rtm-pitch-pair (list)
  (let* ((rtm (car list))
         (pitch (second list))
         (res-pitch ()))
    (loop for i in rtm
      if (< i 0) do (push nil res-pitch) else do (push (pop pitch) res-pitch))
    (list rtm (nreverse res-pitch))))

(defun cluster-conv-nil-rests (list)
  (let* ((list-pairs (butlast list))
         (time-sigs (car (last list)))
         (grouped-pairs (pw::group-list list-pairs 2 'linear)))
    (append (loop for i in grouped-pairs
              append (cluster-convert-one-rtm-pitch-pair i)) (list time-sigs))))



;;;;;;;;;;; PLEASE un-comment the proper one and comment the other 
;;;;;;;;;;; (for some reason save-lisp-and-die doesn't understand the ~/ address...)
; 
;(save-lisp-and-die "/Users/orjansandred/Dropbox/cluster-to-Max-Orjan/cluster19.core")
;(save-lisp-and-die "/Users/julienvincenot/Dropbox/cluster-to-Max-Orjan/new-approach/cluster.core")
 (save-lisp-and-die "/Users/julienvincenot/Desktop/cluster-test.core")

;;;;;;;;;;;

; (print (ClusterEngine  10 t nil nil '((4 4)) '((1/4)(1/8)) '((50) (51)) '((1/4)(1/8)) '((50) (51))))
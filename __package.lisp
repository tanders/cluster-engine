;;;;; BEFORE : RUN SBCL WITH THE CORE OF MOZ LIB

(defpackage cluster-engine (:use :common-lisp :sb-ext))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  :pw  ;;;;;
(load "from-studio-flat")
(load "01.domain.lisp")
(load "02.engine.lisp")
(load "03.Fwd-rules.lisp")
(load "04.Backtrack-rules.lisp")
(load "05.rules-interface.lisp")
(load "05a.rules-interface-1engine.lisp")
(load "05b.rules-interface-2engines.lisp")
(load "05c.rules-interface-2engines.lisp")
(load "05d.rules-interface-2engines.lisp")
(load "05e.rules-interface-2engines.lisp")
(load "05f.rules-interface-3engines.lisp")
(load "05g.rules-interface-any-n-engines.lisp")
(load "05h.rules-higher-level.lisp")
(load "05i.rules-stop-search.lisp")
(load "05n.rules-interface-nn-engines.lisp")
(load "06.heuristic-rules-interface.lisp")
(load "06a.heuristic-rules-interface-1engine.lisp")
(load "06b.heuristic-rules-interface-2engines.lisp")
(load "06c.heuristic-rules-interface-2engines.lisp")
(load "06d.heuristic-rules-interface-2engines.lisp")
(load "06e.heuristic-rules-interface-2engines.lisp")
(load "06f.heuristic-rules-interface-3engines.lisp")
(load "06g.heuristic-rules-interface-any-n-engines.lisp")
(load "07.backjumping.lisp")
(load "08.decode.lisp")
(load "09.utilities.lisp")
(load "09b.markov-tools.lisp")
(load "09c.cluster-energy-profile.lisp")
(load "12.debug-tools.lisp")
(load "13.convert-pmc-rules.lisp")

(load "_000.main-interface.lisp")

;;;;;; PW and Co.
;(load "~/Dropbox/cluster-to-Max-Orjan/other-libraries/iter.lisp")
;(load "~/Dropbox/cluster-to-Max-Orjan/other-libraries/pw-common-language.lisp")
;(load "~/Dropbox/cluster-to-Max-Orjan/other-libraries/pw_profile_functions.lisp")

;;;;;; gen-domains
(load "_001.gen_domains.lisp")


;;;;;;;;;;; PLEASE un-comment the proper one and comment the other 
;;;;;;;;;;; (for some reason save-lisp-and-die doesn't understand the ~/ address...)
; 
;(save-lisp-and-die "/Users/orjansandred/Dropbox/cluster-to-Max-Orjan/cluster19.core")
(save-lisp-and-die "/Users/julienvincenot/Dropbox/cluster-to-Max-Orjan/new-approach/cluster.core")

;;;;;;;;;;;

; (print (ClusterEngine  10 t nil nil '((4 4)) '((1/4)(1/8)) '((50) (51)) '((1/4)(1/8)) '((50) (51))))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
(asdf:load-system :cluster-engine/tests)
(asdf:load-system :ta-utilities)

(asdf:test-system :cluster-engine)


;; (run :cluster-engine/tests :style :spec)

|#


(in-package #:cluster-engine/tests)

#|
(setf a 10)
(ok (= a 10))
|#


(deftest testing-demo
  (testing "array"
    ;; fail test
    (ok (< (length #(1 2 3)) 3)))

  (testing "list"
    (ok (= (length (list 1 2 3)) 3))))



;; Cluster engine is silent
(setf *verbose-i/o?* nil)


;; running multiple tests in a loop works, though the test report gets rather long
(deftest looping-test
  (loop repeat 10
     for x = (random 10)
     for y = 1
     do (ok (= x y))))
  


#|

(ce::clusterengine 
 10 t nil 
 ;; all rhythmic values are equal
 (ce::R-rhythms-one-voice #'(lambda (x y) (= x y)) 0 :durations)
 '((3 4)) 
 '(((1/4) (1/8))
   ((60) (61))))

|#

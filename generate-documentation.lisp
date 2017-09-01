;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#|
;;; Using cldoc (https://gitlab.common-lisp.net/cldoc/cldoc/) 
;;; Just works.  Minor doc string formatting supported.
;;; Perhaps only downside: internal and exported symbols are not distinguished. Workaround: mark auxiliary function names etc. with a leading underscore (e.g., _aux-function). 
;;; BUG: Nested functions (e.g.g, global functions within flet) not documented. Could this be fixed with cldoc::define-descriptor-handler?

(require :cldoc)

(defparameter *doc-dir* (make-pathname :directory (append (pathname-directory *load-truename*) '("doc"))))

(cldoc:extract-documentation 'cludg:html (format nil "~A" *doc-dir*)
    (asdf:find-system :cluster-engine)
    :table-of-contents-title 
    "Cluster Engine")
|#

;;; using the API generator cl-gendoc

(ql:quickload :cl-gendoc)

(gendoc:gendoc (:output-filename "/Users/torsten/common-lisp/cluster-engine/doc/index.html"
                :css "simple.css")
  (:mdf "/Users/torsten/common-lisp/cluster-engine/doc/README.md")
  (:apiref :cluster-engine)
  )



;;; using the more flexible API generator Codex (https://github.com/CommonDoc/codex)

;; (ql:quickload :codex)
(require :codex)

;; (in-package :codex.tmpl)

;; Edited template directly defined in Codex code for now in the following file and directory
;; [file:///Users/torsten/quicklisp/dists/quicklisp/software/codex-20160929-git/templates/templates.lisp]
; [file:///Users/torsten/quicklisp/dists/quicklisp/software/codex-20160929-git/templates/minima-edit/] 

;; generate documentation
(codex:document :cluster-engine)

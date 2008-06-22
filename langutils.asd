;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Language utilities asd files

(defpackage #:langutils.system
  (:use #:cl #:asdf))

(in-package #:langutils.system)

(defsystem #:langutils
    :description "Language utilities"
    :version "1.0"
    :author "Ian Eslick"
    :licence "LLGPL (Franz Inc's modification to LGPL)"
    :depends-on (:meta :s-xml-rpc :stdutils)
    :components ((:module "src"
			  :components ((:file "package")
				       (:file "config")
				       (:file "tokens")
				       (:file "reference")
				       (:file "stopwords")
				       (:file "tokenize")
				       (:file "lexicon")
				       (:file "lemma")
				       (:file "porter")
				       (:file "contextual-rule-parser")
				       (:file "tagger-data")
				       (:file "tagger")
				       (:file "chunker-constants")
				       (:file "chunker")
				       (:file "concept")
				       (:file "init"))
			  :serial t))
    :in-order-to ((load-op (compile-op :langutils))))



;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Language utilities asd files

(defpackage #:langutils.system
  (:use #:cl #:asdf #:asdf-config))

(in-package #:langutils.system)

(defsystem #:langutils
    :description "Language utilities"
    :version "1.0"
    :author "Ian Eslick"
    :licence "LLGPL (Franz Inc's modification to LGPL)"
    :depends-on #+think(:meta :s-xml-rpc :port :stdutils)
                #-think(:meta :s-xml-rpc :port :stdutils)
    :components ((:file "package")
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
    :serial t
    :in-order-to ((load-op (compile-op :langutils))))
;;     :parameters ((:external-token-map "langutils::*external-token-map*")
;; 		 (:token-map "langutils::*default-token-map-file*")
;; 		 (:lexicon "langutils::*default-lexicon-file*")
;; 		 (:stems "langutils::*default-stems-file*")
;; 		 (:lexical-rules "langutils::*default-lexical-rule-file*")
;; 		 (:contextual-rules "langutils::*default-contextual-rule-file*")
;; 		 (:stopwords "langutils::*default-stopwords-file*")
;; 		 (:concise-stopwords "langutils::*default-concise-stopwords-file*"))
;;     :initialization "langutils::init-langutils")



;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          init
;;;; Purpose:       Initialiation of the language utilities package
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;
;;;; Notes:  Implements the subsystem convention: <op>-<package> 
;;;;   init-langutils - initialize the system, called from asdf-config startup script
;;;;   clean-langutils - Wipe all static datafiles, set system to it's clean state
;;;;   reset-langutils - Call clean & init to reset system to initial loaded state
;;;;

(in-package :langutils)

;; NOTES ON USAGE:
;; 
;; When the system is initialized, the lexicon should be loaded first, followed by any
;; lemma files.  That way any token IDs used in the stored SQL arrays stays coherent
;; to the database.  To ensure a standard token map, you can set the variable
;; *default-token-map-file* to define a file to load from.  You must explicitely save
;; your own token map at some point.
;; 

(defun init-langutils ()
  (when (if *external-token-map*
	    (and (not *lexicon*) *token-counter-hook*)
	    (not *lexicon*))
    (format t "Initializing langutils...~%")

    ;; Token maps
    (initialize-tokens)
    (unless *external-token-map*
      (reset-token-counts)
      (aif *default-token-map-file* (load-token-map it)))

    ;; Lexicon (virtualize init?)
    (unless *lexicon*
      (format t "Loading lexicon...~%")
      (time
       (init-lexicon *default-lexicon-file* *default-stems-file*)))

    ;; Tagger
    (unless (and *tagger-lexical-rules* *tagger-contextual-rules*)
      (format t "Loading tagger rule sets...~%")
      (init-tagger *default-lexical-rule-file* *default-contextual-rule-file*))

    ;; Stopword db
    (format t "Finishing miscellaneous langutils setup.~%")
    (init-stopwords *default-stopwords-file*)
    (init-concise-stopwords *default-concise-stopwords-file*)

    ;; Concepts
    (clear-concept-cache)

    (format t "Done initializing langutils.~%")
    t))

(defun clean-langutils ()
  (reset-token-counts)
;;  (reset-token-tables)
  (initialize-tokens)
  (clean-lexicon)
  (clean-tagger)
  (clean-stopwords))

(defun reset-langutils ()
  (clean-langutils)
  (init-langutils))

(eval-when (:load-toplevel)
  ;; Read config
  (when *auto-init*
    (init-langutils)))

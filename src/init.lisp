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

(defvar *default-token-map-file* nil
  "Path to the token map file")
(defvar *default-lexicon-file* nil
  "Path to the lexicon file")
(defvar *default-stems-file* nil
  "Path to the word stems file")
(defvar *default-lexical-rule-file* nil
  "Path to the brill lexical rule file")
(defvar *default-contextual-rule-file* nil
  "Path to the brill contextual rule file")
(defvar *default-stopwords-file* nil
  "Path to a stopwords file")
(defvar *default-concise-stopwords-file* nil
  "Path to a *very* small list of words. Mainly pronouns and determiners")

(defun init-langutils ()
  (when (if *external-token-map*
	    (and (not *lexicon*) *token-counter-hook*)
	    (not *lexicon*))
    (format t "Initializing langutils...~%")
;;    (start-logging lexicon-init)
    
    ;; Token maps
    (initialize-tokens)
    (unless *external-token-map*
      (reset-token-counts)
      (aif *default-token-map-file* (load-token-map it))
      (port:gc))

    ;; Lexicon (virtualize init?)
    (format t "Loading lexicon...~%")
    (time
     (init-lexicon *default-lexicon-file* *default-stems-file*))
    (port:gc)

  ;; Tagger
  (format t "Loading tagger rule sets...~%")
  (init-tagger *default-lexical-rule-file* *default-contextual-rule-file*)
  (port:gc)

  ;; Stopword db
  (format t "Finishing miscellaneous langutils setup.~%")
  (init-stopwords *default-stopwords-file*)
  (init-concise-stopwords *default-concise-stopwords-file*)
  (port:gc)

  ;; Concepts
  (clear-concept-cache)
  (port:gc)

;;    (stop-logging lexicon-init)
  (format t "Done initializing langutils.~%")
  t))

(defun clean-langutils ()
  (reset-token-counts)
  (reset-token-tables)
  (initialize-tokens)
  (clean-lexicon)
  (clean-tagger)
  (clean-stopwords))

(defun reset-langutils ()
  (clean-langutils)
  (port:gc)
  (init-langutils))
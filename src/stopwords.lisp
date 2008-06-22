;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stopwords
;;;; Purpose:       Simple db for stopword identity, all tokenized
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

;;; stopwords vs concise-stopwords: stopwords is a wide list of words.
;;;                                 concise-stopwords are a *very* small list of words. Mainly pronounds and determiners
(in-package :langutils)

;; =================================
;; Stopword Database and Processing
;; =================================

(defvar *stopwords* nil)
(defvar *concise-stopwords* nil)

(defun init-stopwords (&optional path)
  (when (null path) (setf path (translate-logical-pathname "think:data;lang;en;langutils;stopwords.txt")))
  (setf *stopwords* (make-hash-table :test #'equal :size 1000))
  (init-word-test)
  (with-open-file (f path )
    (do-contentful-lines (line f)
      (hash-put *stopwords* (id-for-token (string-trim " " line)) t))))

(defun init-concise-stopwords (&optional path)
  (when (null path) (setf path  (translate-logical-pathname "think:data;lang;en;langutils;concise-stopwords.txt")))
  (setf *concise-stopwords* (make-hash-table :test #'equal :size 10))
  (with-open-file (f path )
    (do-contentful-lines (line f)
      (hash-put *concise-stopwords* (id-for-token (string-trim " " line)) t))))


(defun clean-stopwords ()
  (setf *stopwords* nil)
  (setf *concise-stopwords* nil))

(defun stopword? (id)
  "Identifies id as a 'stopword'"
  (hash-get *stopwords* id))

(defun concise-stopword? (id)
  "Identifies id as a 'concise-stopword' word.
   concise-stopwords are a *very* small list of words. Mainly pronouns and determiners"
  (hash-get *concise-stopwords* id))


(defvar *is-token* nil)
(defvar *s-token* nil)

(defun init-word-test ()
  (declare (special *is-token* *s-token*))
  (setf *is-token* (id-for-token "is"))
  (setf *s-token* (id-for-token "'s")))

(defun contains-is? (ids)
  "Tests list of ids for 'is' words"
  (declare (special *is-token* *s-token*))
  (find-if (lambda (id) (or (eq *is-token* id)
			    (eq *s-token* id)))
	   ids))

;; --------------------------

(defun string-stopword? (word)
  (stopword? (id-for-token word)))

(defun string-concise-stopword? (word)
  "Check the word if it is a 'concise-stopword' word.
   concise-stopwords are a *very* small list of words. Mainly pronouns and determiners"
  (concise-stopword? (id-for-token word)))

(defun string-contains-is? (words)
  "Checks the list for a string containing 'is'"
  (contains-is? (mapcar #'id-for-token words)))






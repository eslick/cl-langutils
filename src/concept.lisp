;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          concept
;;;; Purpose:       Abstraction for concepts.  A "concept" contains an array of
;;;;                tokens representing the words of that concept, ie "walk dog" or "brush teeth"
;;;;
;;;; Programmer:    Aaron M. Sokoloski (and Ian Eslick)
;;;; Date Started:  January 2005


(in-package :langutils)

;;
;; Concept class definition
;;

(defclass concept ()
  ((token-vector ;; vector of token ids for words
    :reader token-vector
    :initarg :token-vector
    #-mcl :type #-mcl (array fixnum)
    :documentation "Stores the representation of the concept as an array of token ids")))

(eval-when (:compile-toplevel :load-toplevel)
  (export 'token-vector))

(defmethod print-object ((cn concept) stream)
  (print-unreadable-object (cn stream :type t :identity nil)
    (prin1 (concept->string cn) stream)))

;;
;; Concept library so all instances of the same 'concept'
;; are the same object (cheap 'eq comparisons)
;;

(defvar *concept-vhash* nil)

(defmethod lookup-canonical-concept-instance ((ta array))
  "Take a token array and see if there is already a 
   concept instance"
;;  (vechash-get ta *concept-vhash*))
  (stdutils.gds:get-value *concept-vhash* ta))

(defmethod clear-concept-cache ()
  (setf *concept-vhash* (make-instance 'stdutils.gds:vector-keyed-table)))

(defparameter *concept-store-scratch-array* (make-array 20 :element-type 'fixnum :adjustable t)
  "Allows us to lookup concepts from arrays without allocating lots of unnecessary data")
  
(defmethod lookup-canonical-concept-instance ((lf list))
  "List of fixnums to lookup a concept instance"
;;  (vechash-get lf *concept-vhash*))
  (stdutils.gds:get-value *concept-vhash* (list->array lf)))

(defmethod register-new-concept-instance ((c concept))
;;  (vechash-put (token-vector c) c *concept-vhash*)
  (setf (stdutils.gds:get-value *concept-vhash* (token-vector c)) c)
  c)

;;
;; Concept is basically a low-overhead phrase, so allow comparisons between them
;;

(defmethod-exported conceptually-equal ((ph1 phrase) (ph2 phrase))
  (every #'eql (phrase-lemmas ph1) (phrase-lemmas ph2)))

(defmethod-exported conceptually-equal ((ph phrase) (cn concept))
  (every #'eql (phrase-lemmas ph) (concept->token-array cn)))

(defmethod-exported conceptually-equal ((cn concept) (ph phrase))
  (conceptually-equal ph cn))

(defmethod-exported conceptually-equal ((cn1 concept) (cn2 concept))
  (let ((ta1 (concept->token-array cn1))
	(ta2 (concept->token-array cn2)))
    (and (eql (length ta1) (length ta2))
	 (every #'eql ta1 ta2))))

(defmethod-exported concept-contains ((csuper concept) (csub concept))
  (search (concept->token-array csub) (concept->token-array csuper) :test #'eql))

(defmethod-exported concat-concepts (&rest concepts)
  (token-array->concept (apply #'concatenate 'array (mapcar #'concept->token-array concepts)) :lemmatized t))

;;
;; Getting at text of Concepts
;;

(defmethod-exported concept->words ((cname concept))
  (loop for id across (token-vector cname)
    collecting
    (token-for-id id)
    into strings
    finally (return strings)))

(defmethod-exported concept->string ((cname concept))
  (list-to-delimited-string (concept->words cname)))

(defmethod-exported concept->token-array ((cname concept))
  "Concepts are immutable, don't change them!"
  (let ((copy (make-array (length (token-vector cname)) :element-type 'fixnum)))
    (map-into copy #'identity (token-vector cname))))


;;
;; Concept creation and uniqification (concepts are non-volatile)
;; 

(defun-exported force-concept (c)
  (etypecase c
    (string (string->concept c))
    (concept c)
    (array (token-array->concept c))
    (phrase (phrase->concept c))))

(defun-exported string->concept (s &key (lemmatized nil))
  (words->concept (lex-string (string-downcase s)) :lemmatized lemmatized))

(defun-exported phrase->concept (p &key lemmatized)
  "Create a canonical concept from an arbitrary phrase
   by removing determiners and lemmatizing verbs."
  ;; NOTE: Could do this faster, but what the heck...
  (words->concept (phrase-words p) :lemmatized lemmatized))

(defun-exported words->concept (slist &key (lemmatized nil))
  (labels ((ensure-tokens (list)
			     (if (not (integerp (car list)))
				 (mapcar #'id-for-token list)
			       list))
	   (ensure-lemmatized (list)
			      (if (not lemmatized)
				  (lemmatize list :strip-det t :noun t :last-only t)
				list)))
    (let ((tlist (ensure-lemmatized (ensure-tokens slist))))
      (when tlist
        (aif-ret (lookup-canonical-concept-instance tlist)
          (let ((token-vector (make-array (length tlist) :element-type 'fixnum :adjustable nil)))
  	    (map-into token-vector #'identity tlist)
 	    (token-array->concept token-vector :lemmatized t)))))))

(defun-exported token-array->concept (tokens &key (lemmatized nil))
  (if lemmatized 
      (ensure-concept tokens) 
    (ensure-concept (lemmatize tokens :strip-det t :noun t :last-only t))))

(defun ensure-concept (tokens)
  (aif-ret (lookup-canonical-concept-instance tokens)
	   (make-concept tokens)))

(defun-exported make-concept (ta)
  (register-new-concept-instance (make-instance 'concept :token-vector ta)))

;;
;; Random concept related utilities
;;

(defun-exported associate-concepts (phrases)
  "Return the list of phrase/list/token-arrays as pairs with the 
   first element being the original and the second being a
   canonicalized concept instance"
  (mapcar (lambda (phrase)
	    (cons phrase (phrase->concept phrase)))
	  phrases))

;;
;; Some unit tests
;;

(defun test-concept-equality ()
  (assert (eq (string->concept "a fast dog") (string->concept "fast dogs"))))

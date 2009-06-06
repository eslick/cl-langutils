;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lemma
;;;; Purpose:       Identify the lemma of a given word or token in the lexicon
;;;;                or guess the root using the porter algorithm
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;;;; find root

(defun get-lemma (word &key pos (noun t) porter)
  "Provides the root word string for the provided word string"
  (token-for-id (get-lemma-for-id (id-for-token word) :pos pos :noun noun :porter porter)))

(defun get-lemma-for-id (id &key pos (noun t) (porter nil))
  "Returns a lemma id for the provided word id.  pos only
   returns the root for the provided pos type.  noun will
   stem nouns to the singular form by default and porter
   determines whether the porter algorithm is used for 
   unknown terms.  pos type causes the noun argument to be
   ignored"
  (aif (get-lexicon-entry id)
       ;; Use lexicon if we can, should have all root forms
       (let ((roots (lexicon-entry-roots it)))
	(if roots
	    (cond ((and pos noun) ;; POS given and lemmatizing nouns
		   (aif (assoc pos roots)
			(values (cdr it) (car it))
			(values id nil)))
		  ((and pos (not noun)) ;; POS speech given, don't lemmatize nouns
		   (aif (and (not (find pos '(:NN :NNS)))
			     (assoc pos roots))
			(values (cdr it) (car it))
			(values id nil)))
		  ((and (not pos) noun) ;; lemmatize everything, no pos so just return default root
		   (values (cdar roots) ;; only return pos if id is different so can test root-found on 2nd value
			   (when (not (eq (cdar roots) id))
			     (caar roots))))
		  ((and (not pos) (not noun)) ;; don't lemmatize nouns so return default unless word has a noun form
		   (if (or (eq (caar roots) :NN)
			   (eq (caar roots) :NNS)
			   (eq (caar roots) :NNP))
		       (values id nil)
		     (values (cdar roots) (caar roots)))))
	  ;; else assume I am the root
	  (values id nil)))
       ;; Use porter algorithm if we have no lexicon entry
       (if porter
	   (let ((str (token-for-id id)))
	     (values (id-for-token (stem str)) :NN)) ;; guess noun
	 (values id nil))))

;; ========================
;; General interface
;; ========================

(defun *get-determiners* ()
  (mapcar #'id-for-token '("the" "a" "an")))

(defun select-token (token &key strip-det noun pos porter (lemma t))
  "Internal per-token function"
  (if (and strip-det (find token (*get-determiners*)))
      nil
    (if lemma 
	(get-lemma-for-id token :noun noun :pos pos :porter porter)
      token)))

(defmethod-exported lemmatize ((sequence list) &key strip-det pos (noun t) porter last-only)
  "Non-destructive lemmatization of provided sequence"
  (labels ((select (token orig)
		   (select-token token 
				 :lemma (if (and last-only (not (last-elt? orig))) nil t)
				 :strip-det strip-det 
				 :pos pos 
				 :noun noun 
				 :porter porter))
	   (last-elt? (list)
		      (null (cdr list)))
	   (descend (orig new)
		    (if (null orig) 
			(nreverse new)
		      (descend (cdr orig) 
			       (aif (select (car orig) orig) 
				    (cons it new) 
				    new)))))
    (when sequence
      (assert (integerp (car sequence)))
      (descend sequence nil))))
      

(defmethod-exported lemmatize ((sequence array) &key strip-det pos (noun t) porter last-only)
  (let ((new (make-array (length sequence) :element-type 'fixnum :adjustable t))
	(last (1- (length sequence)))
	(index 0))
    (loop for token across sequence do
	  (awhen (select-token token 
			       :lemma (if (and last-only (not last)) nil t)
			       :strip-det strip-det 
			       :pos pos 
			       :noun noun 
			       :porter porter)
		 (setf (aref new index) it)
		 (incf index)))
    new))

;; ------------------------------
;; Find surface forms from lemma
;; ------------------------------

(defvar *pos-class-map*
  '((:VB :V)
    (:VBD :V)
    (:VBN :V)
    (:VBG :V)
    (:VBZ :V)
    (:JJ :A)
    (:NN :N)
    (:NNS :N)
    (:NNP :N)))

(defun morph-surface-forms-text (root &optional pos-class)
  (mapcar #'token-for-id (morph-surface-forms root pos-class)))

(defun morph-case-surface-forms (root &optional (pos-class nil))
  "All cases of morphological surface forms of the provided root"
  (let ((forms (morph-surface-forms root pos-class)))
    (append forms
	    (nflatten 
	     (mapcar #'get-lexicon-case-forms forms)))))
	     

(defun morph-surface-forms (root &optional (pos-class nil))
  "Takes a word or id and returns all surface form ids or all forms of
   class 'pos-class' where pos-class is a symbol of langutils::V,A,N"
  (labels ((surface-forms (root-pairs)
	  (remove-duplicates
	   (flatten
	    (mapcar #'(lambda (root-pair)
			(lexicon-entry-surface-forms
			 (get-lexicon-entry (cdr root-pair))))
		    root-pairs))))
	   (filter-by-pos (surface-forms)
	     (select-if #'(lambda (word)
			    (let ((entry (get-lexicon-entry word)))
			      (some #'(lambda (tag)
					(in-pos-class? tag pos-class)) 
				    (lexicon-entry-tags entry))))
			surface-forms)))
  (let* ((all-root-pairs (lexicon-entry-roots (get-lexicon-entry root)))
         (all-surface (surface-forms all-root-pairs)))
    (when all-surface
      (if pos-class
	  (filter-by-pos all-surface)
	all-surface)))))


(defun-exported in-pos-class? (element class)
  (aif (and (or (eq class :V)
	      (eq class :N)
	      (eq class :A))
	  (assoc element *pos-class-map*))
       (eq (cadr it) class)))
  
  



  

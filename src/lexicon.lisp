;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lexicon
;;;; Purpose:       The BROWN/WSJ lexicon, extendable, supports all langutils
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; -----------------------------
;; LEXICON

;; The Lexicon
(defvar *lexicon* nil)

(defmacro with-static-memory-allocation (() &rest body)
  `(progn ,@body))

;; Loading the lexicon
(defun init-lexicon (&optional lexicon-file lemma-file)
  "Populates the lexicon with 'word tag1 tag2' structured
   lines from lexicon-file"
  (unless *lexicon* 
    (write-log lexicon-init "Initializing the lexicon")
    (setf *lexicon* (make-hash-table :size 100000 :rehash-size 1.3 :rehash-threshold 0.8))
    (let ((lexicon-file (aif lexicon-file it 
			     (translate-logical-pathname "think:data;lang;en;langutils;LEXICON-BROWN-AND-WSJ.txt")))
	  (lemma-file (aif lemma-file it
			   (translate-logical-pathname "think:data;lang;en;langutils;stem-dict.txt"))))
      (with-static-memory-allocation ()
	(write-log lexicon-init "Reading lexicon from ~A" lexicon-file)
	;; Parse the lines into a predicate ID and two node structures
	(with-open-file ( s lexicon-file :external-format :ascii)
	  (do-count-contentful-lines (l count s)
	    (when (= (mod count 10000) 0) (write-log lexicon-init "Processed ~A lines" count))
	    (let ((lexicon-entry (extract-words l))) ;; (pregex:split "\\s+" l)))
	      (add-basic-entry (car lexicon-entry) (mapcar #'mkkeysym (cdr lexicon-entry))))))
	(write-log lexicon-init "Reading word->lemma data from ~A" lemma-file)
	;; Parse the lines into a predicate ID and possible roots
	(with-open-file ( s lemma-file :external-format :ascii)
	  (do-count-contentful-lines (l count s)
	    (when (= (mod count 10000) 0) (write-log lexicon-init "Processed ~A lines" count))
	    (let ((roots-entry (extract-words l)))
	      (add-roots (first roots-entry) 
			 (mapcar #'(lambda (root+pos) 
				     (cons (mkkeysym (cdr root+pos)) 
					   (id-for-token (car root+pos))))
				 (pairs (cdr roots-entry)))))))))))

(defun clean-lexicon ()
  (setf *lexicon* nil))

;; 
;; Internals and accessors
;; 

(defstruct (lexicon-entry 
	    (:conc-name "LEXICON-ENTRY-"))
  ;; Base lexicon (human entered)
  tags
  id 
  roots
  surface-forms
  case-forms)

(defun lexicon-entry-tag (entry)
  (aif entry (car (lexicon-entry-tags entry)) nil))

(defun get-lexicon-entry (word)
  (etypecase word
    (string (hash-get *lexicon* (id-for-token word)))
    (integer (hash-get *lexicon* word))))

(defun set-lexicon-entry (word entry)
  (ecase word
    (string (hash-put *lexicon* (id-for-token word) entry))
    (integer (hash-put *lexicon* word entry))))

(defsetf get-lexicon-entry set-lexicon-entry)

(defun get-lexicon-default-pos (word)
  (awhen (get-lexicon-entry word)
	 (lexicon-entry-tag it)))

(defun get-lexicon-case-forms (word)
  (awhen (get-lexicon-entry word)
	 (lexicon-entry-case-forms it)))


;;
;; Modifying the lexicon
;;

(defun ensure-lexicon-entry (word &key roots surface)
  (aif (get-lexicon-entry word)
       it
       (add-basic-entry word 
		    (mapcar #'car roots)
		    :roots roots :surface surface)))

(defun add-basic-entry ( word tags &key roots surface)
  "Add a word and it's probability ordered tags to the lexicon"
  (unless (and (stringp word) (string= word ""))
    (let* ((id (etypecase word
		 (string (id-for-token word))
		 (integer word)))
	   (cases (make-cases 
		   (etypecase word
		     (string word)
		     (integer (token-for-id word)))))
	   (entry (make-lexicon-entry 
		   :id id
		   :tags tags 
		   :roots roots
		   :surface-forms surface
		   :case-forms cases)))
      (hash-put *lexicon* id entry)
      entry)))

(defun make-cases ( word )
;;  (declare (type (string word)))
  (let ((all-cases 
	 (mapcar #'id-for-token
		 (list (string-downcase word)
		       (string-upcase word)
		       (concatenate 'string 
				    (string-upcase (subseq word 0 1))
				    (string-downcase (subseq word 1)))))))
    (remove (id-for-token word) all-cases)))

(defun add-unknown-lexicon-entry (word guessed-tag)
  (let ((id (etypecase word
	      (string (id-for-token word))
	      (integer word))))
    (hash-put *lexicon* id
	    (make-lexicon-entry
	     :id id
	     :tags (list guessed-tag)
	     :surface-forms nil
	     :case-forms nil
	     :surface-forms nil))))

(defun add-roots ( word root-pairs )
  "Set the root list (pairs of pos_type/root) for the entry for 'word'"
  (let ((entry (ensure-lexicon-entry word :roots root-pairs)))
    ;; Add roots and surface forms
    (add-root-forms word root-pairs)
    ;; Ensure default tags get set
    (when (null (lexicon-entry-tags entry))
      (setf (lexicon-entry-tags entry)
	    (mapcar #'car (lexicon-entry-roots entry))))))

(defun add-root-forms ( word pos-root-pairs )
  (let ((id (etypecase word
	      (string (id-for-token word))
	      (integer word)))
	(pr-pairs (reverse pos-root-pairs))) ;; get ordering right in object
    ;; Add roots to entry
    (mapc #'(lambda (x) (add-root id x))
	  pr-pairs)
    ;; Add surface form to each root
    (mapc #'(lambda (x) (add-surface-form (cdr x) id))
	  pr-pairs)))
	
(defun add-root ( word pos-root-pair)
  "Add a root form to word if not exists"
  (let ((entry (ensure-lexicon-entry word :roots (list pos-root-pair))))
    (pushnew pos-root-pair (lexicon-entry-roots entry) :test #'equal)))
  
(defun add-surface-form ( root surface-form )
  "Add a surface form to a root word"
  ;; (assert (and (numberp surface-form) (numberp root)))
  (let ((entry (ensure-lexicon-entry root :surface (list surface-form))))
    (pushnew surface-form (lexicon-entry-surface-forms entry) :test #'equal)))

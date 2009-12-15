;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          chunker
;;;; Purpose:       A regex verb and noun phrase chunker using the 
;;;;                array matching utility infrastructure
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; ===============================
;; STRING CHUNKING TOP LEVEL
;; Speed: slow (extra conversion)
;; Input size: < 100k strings

(defun chunk (text)
  "Returns a phrase-list for the provided text"
  (get-basic-chunks (vector-tag text)))

(defun chunk-tokenized (text)
  "Returns a phrase-list for the provided tokenized string"
  (get-basic-chunks (vector-tag-tokenized text)))

;; ===============================
;; VECTOR-DOCUMENT INTERFACE
;; Speed: optimal
;; Input size: unlimited

(defmethod get-basic-chunks ((doc vector-document) &optional interval)
  "Returns a list of PHRASEs referencing 'doc' for
   all supported primitive phrase types"
  (let ((nxs (get-nx-chunks doc interval))
	(vxs (get-vx-chunks doc interval))
	(axs (get-adverb-chunks doc interval))
        (ps (get-pp-chunks doc interval)))
    (sort (append nxs vxs axs ps)
	  #'< :key #'phrase-start)))

(defmethod get-imperative-chunks ((doc vector-document) &optional interval)
  (do-collect-vector-matches (start end #.(localize-expression
					   (list :AND 
						 verb-pattern
						 (list :? :AND noun-pattern)
						 p-pattern
						 noun-pattern) :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
       (make-instance 'phrase
		      :type :imperative
		      :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defmethod get-event-chunks ((doc vector-document) &optional interval)
  "Return vx+nx (simple verb arg) phrase objects"
  (do-collect-vector-matches (start end #.(localize-expression (list :AND verb-pattern noun-pattern) :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
       (write-log chunker "Found event: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :event
		      :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))
  
(defmethod get-extended-event-chunks1 ((doc vector-document) &optional interval)
  "Return vx+nx+pp... objects"
  (do-collect-vector-matches (start end #.(localize-expression `(and ,verb-pattern ,noun-pattern ,p-pattern ,noun-pattern) :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
       (write-log chunker "Found extended event: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :event
		      :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defmethod get-extended-event-chunks2 ((doc vector-document) &optional interval)
  "Return vx+nx+pp... objects"
  (do-collect-vector-matches (start end #.(localize-expression `(and ,verb-pattern ,noun-pattern ,p-pattern ,noun-pattern ,p-pattern ,noun-pattern) :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
       (write-log chunker "Found extended event: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :event
		      :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))


(defmethod get-nx-chunks ((doc vector-document) &optional interval)
  "Return a list of all nx phrases"
  (do-collect-vector-matches (start end #.(localize-expression noun-pattern :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
       (write-log chunker "Found np: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
       (make-instance 'phrase
		      :type :nx
		      :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defmethod get-vx-chunks ((doc vector-document) &optional interval)
  "Return a list of all primitive vx phrases - no arguments"
  (do-collect-vector-matches (start end #.(localize-expression verb-pattern :package 'keyword)) 
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
      (write-log chunker "Found vp: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :vx
		     :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defmethod get-adverb-chunks ((doc vector-document) &optional interval)
  "Return a list of all adverbial phrases"
  (do-collect-vector-matches (start end #.(localize-expression adv-pattern :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
      (write-log chunker "Found ap: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :advp
		     :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defmethod get-p-chunks ((doc vector-document) &optional interval)
  "Return a list of all prepositions as phrases"
  (do-collect-vector-matches (start end #.(localize-expression p-pattern :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
      (write-log chunker "Found prep: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :prep
		     :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defmethod get-pp-chunks ((doc vector-document) &optional interval)
  "Return a list of all prepositions as phrases"
  (do-collect-vector-matches (start end #.(localize-expression (list :AND p-pattern noun-pattern) :package 'keyword))
			     ((if interval
				  (subseq (document-tags doc) (car interval) (cdr interval))
				  (document-tags doc)))
      (write-log chunker "Found prep: ~A (~A : ~A)" (subseq (document-tags doc) start (1+ end)) start end)
      (make-instance 'phrase
		     :type :pp
		     :document doc
		      :start (if interval (+ start (car interval)) start)
		      :end (if interval (+ end (car interval)) end))))

(defun head-verbs (phrases &key (filter-common t))
  (collect (lambda (p) (head-verb p :filter-common filter-common))
	   phrases))
  
(defparameter *common-verbs* nil)

(defun ensure-common-verbs ()
  (setf *common-verbs* 
	(mapcar #'id-for-token
		'("be" "have" "say"  "see" "ask" "tell" "reply"
		  "do" "let"  "find" "answer" "take"))))

(defun head-verb (phrase &key (filter-common t))
  (ensure-common-verbs)
  (unless (and filter-common
	       (member (get-lemma-for-id (get-token-id phrase 0)) *common-verbs*))
    (make-phrase (make-array 1 :initial-element (get-token-id phrase 0))
		 (make-array 1 :initial-element (get-tag phrase 0))
		 :verb)))

(defun root-nouns (phrases)
  (append (collect #'root-noun phrases)
	  phrases))

(defun root-noun (phrase)  
  (when (> (phrase-length phrase) 1)
    (let ((last (1- (phrase-length phrase))))
      (make-phrase (make-array 1 :initial-element (get-token-id phrase last))
		   (make-array 1 :initial-element (get-tag phrase last))
		   :noun))))
		 

;; ====================================
;; Simple tagging interactive function

(defun test-phrase (text)
  "Prints all the phrases found in the text for simple
   experimenting"
  (let ((doc (vector-tag text)))
    (format t "Tagged: ~A~%" (print-vector-document doc))
    (mapcar #'print-phrase (get-basic-chunks doc))))
    
;; =======================================
;; Experiment with higher order structure 

(defun all-vx+nx-phrases (phrases)
  "Overly hairy function for finding all vx phrases that
   are followed by nx.  Get event chunks is a better way 
   to do this."
  (declare (optimize speed (safety 1))
           (type list phrases))
  (let ((pairs nil))
    (declare (type list pairs))
    (labels ((following-noun (start phrases count)
			     (cond ((or (null phrases) (= count 2))
				    nil)
				   ((= start (phrase-start (car phrases)))
				    (car phrases))
				   (t (following-noun start (cdr phrases) (1+ count)))))
	     (rec (cp phrases)
		  (cond ((null phrases)
			 (nreverse pairs))
			((eq (phrase-type cp) :verb)
			 (aif (following-noun (1+ (phrase-end cp)) phrases 0)
			      (push (make-instance 'phrase
						   :type :event
						   :document (phrase-document cp)
						   :start (phrase-start cp)
						   :end (phrase-end it))
				    pairs))
			 (rec (car phrases) (cdr phrases)))
			(t (rec (car phrases) (cdr phrases))))))
      (rec (car phrases) (cdr phrases)))))



;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reference
;;;; Purpose:       A wrapper around vector representations of text
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; ---------------------------
;; Document wrapper

(defclass vector-document ()
  ((text ;; text vector
    :accessor document-text
    :initarg :text
    :type (array fixnum))
   (tags  ;; tag vector
    :accessor document-tags
    :initarg :tags
    :type (array symbol))
   (annotations
    :accessor document-annotations
    :initarg :annotations
    :initform nil
    :type list)))

(defmethod length-of ((doc vector-document))
  (length (document-text doc)))

(defun make-vector-document (text &optional tags)
  (make-instance 'vector-document
		 :text text
		 :tags tags))

;; Document accessors

(defmethod get-token-id ((doc vector-document) offset)
  (aref (document-text doc) offset))
(defmethod get-tag ((doc vector-document) offset)
  (aref (document-tags doc) offset))

(defmethod get-annotation ((doc vector-document) key)
  "First returned value is the association value
   or null if none.  The second is true if the
   key exists, nil otherwise"
  (aif (assoc key (document-annotations doc))
       (values (cdr it) t)
       (values nil nil)))

(defmethod set-annotation ((doc vector-document) key value &key (method :override))
  "Add an annotation to object using method :override, :push, :duplicate-key"
  (flet ((set-on-empty () (assoc-setf (document-annotations doc) key value 'eq)))
    (case method
      (:override 
	(aif (assoc key (document-annotations doc))
	     (rplacd it value)
	     (set-on-empty)))
      (:push
	(aif (assoc key (document-annotations doc))
	     (push value (cdr it))
	     (set-on-empty)))
      (:duplicate-key
	(setf (document-annotations doc) (acons key value (document-annotations doc))))
      (t (error "Invalid method ~A for add-annotation" method)))
    t))

(defmethod unset-annotation ((doc vector-document) key)
  (remove key (document-annotations doc) :key #'car))

;; Create a vector document from a file or text

(defun vector-document (input)
  (typecase input 
    (string (vector-tag input))
    (pathname (read-vector-document input))
    (vector-document input)
    (t (error "No handler for input type: ~A" (type-of input)))))

;; Documents to/from strings

(defun string-tag ( string &optional (stream t))
  "Tokenizes and tags the string returning
   a standard tagged string using '/' as a separator"
  (string-tag-tokenized
   (mvretn 3 (tokenize-string string))
   stream))

(defun string-tag-tokenized ( string &optional (stream t))
  (print-vector-document (vector-tag string) :stream stream))

(defmethod vector-document-string ( (doc vector-document) &key (with-tags nil) (with-newline nil) )
  (with-output-to-string (stream)
    (print-vector-document doc :stream stream :with-tags with-tags :with-newline with-newline)))

(defmethod print-vector-document ( (doc vector-document) &key (stream t) (with-tags t) (with-newline t) )
  (with-slots (text tags) doc
    (loop for token fixnum across (document-text doc) and
	      tag   symbol across (document-tags doc) do
	(if with-tags
	    (format stream "~A/~A " (token-for-id token) tag)
	  (format stream "~A " (token-for-id token))))
    (when with-newline
      (format stream "~%"))))

(defmethod vector-document-words ( (doc vector-document) )
  (token-array->words (document-text doc)))

;; Documents to/from files in native form

(defmethod vector-doc-as-ids ( (doc vector-document) )
  "Converts the word array to ids with shared structure
   for the other elements; keeps the data 'in the family'
   so the source or destination documents should be short lived"
  (let* ((word-array (document-text doc))
	 (id-array (make-array (length word-array) :element-type 'fixnum)))
    (make-instance 'vector-document
		   :text (map-into id-array #'id-for-token word-array)
		   :tags (document-tags doc)
		   :annotations (document-annotations doc))))

(defmethod vector-doc-as-words ( (doc vector-document) )
  (let* ((id-array (document-text doc))
	 (word-array (make-array (length id-array) :element-type 'string)))
    (make-instance 'vector-document
		   :text (map-into word-array #'token-for-id id-array)
		   :tags (document-tags doc)
		   :annotations (document-annotations doc))))

(defmethod write-vector-document ((doc vector-document) filename &key (with-tags t) (if-exists :supersede))
  (with-open-file (s filename :direction :output :if-exists if-exists)
    (print-vector-document doc :stream s :with-tags with-tags)))

(defmethod read-vector-document (filename)
  (vector-tag (read-file-to-string filename)))

(defmethod read-vector-document-to-string ((doc vector-document) &key (with-tags t))
  (with-output-to-string (s)
    (print-vector-document doc :with-tags with-tags :stream s)))

(defmethod document-window-as-string (document start end)
  (apply #'concatenate 'string 
	 (shuffle 
	  (mapcar #'token-for-id 
		  (subseq (document-text document) start end))
	  (repeat " " (- end start)))))

;; ========================================================
;; Phrase wrapper
;; ========================================================

(defclass+ phrase ()
  ((type nil)     ;; phrase type
   (document nil) ;; pointer or id if in registry
   (start nil)    ;; offset in doc
   (end nil)      ;; end in doc
   (annotations nil))
  (:prefix "phrase-"))

(defmethod print-object ((p phrase) stream)
  (let ((pstr (make-string-output-stream)))
    (print-phrase p :stream pstr :with-tags nil :newline nil)
    (format stream "#<~A:~A \"~A\">"
	    (class-name (class-of p))
	    (phrase-type p)
	    (get-output-stream-string pstr))))

(defun make-phrase-from-sentence (tok-string &optional tag-array)
  (let ((words (extract-words tok-string)))
    (make-phrase (map-into (make-array (length words)) #'id-for-token words)
		 tag-array)))

(defun make-phrase (text-array tag-array &optional type)
  "Take two arrays of test and tags and create a phrase 
   that points at a vdoc created from the two arrays"
  (make-phrase-from-vdoc
   (vector-doc-as-ids 
    (make-vector-document text-array tag-array))
   0
   (length text-array)
   type))

(defparameter *temp-phrase* nil)

(defun temp-phrase ()
  (unless *temp-phrase*
    (setf *temp-phrase* (make-phrase nil nil nil)))
  *temp-phrase*)

(defmethod find-phrase ((p phrase) (doc vector-document) &key (match :all) (start 0) (ignore-start nil) (ignore-end nil) (lemma nil) (concept-terms nil))
  "Find the specified phrase starting at start, matching text and/or tags according to match.
   The lemma parameter indicates whether the phrases match under the lemma operator and
   ignore-start and ignore-end causes the search to not match a region within the document"
  (declare (optimize (speed 3) (safety 1) (space 0))
	   (type fixnum start))
  (if (eq (phrase-document p) doc)
      (values (phrase-start p) (phrase-end p))
    (let ((ptext (document-text (phrase-document p)))
	  (dtext (document-text doc))
	  (ptags (document-tags (phrase-document p)))
	  (dtags (document-tags doc)))
      (declare (type (array fixnum *) dtext)
	       (type (array symbol *) ptext))
      (labels ((match-tokens (doc-offset phrase-offset)
		 (declare (type fixnum doc-offset phrase-offset))
		 (if (and ignore-start ignore-end
			  (>= doc-offset ignore-start)
			  (<= doc-offset ignore-end))
		     nil
		     (case match
		       (:all (and (match-text doc-offset phrase-offset)
				  (match-tags doc-offset phrase-offset)))
		       (:words (match-text doc-offset phrase-offset))
		       (:pos (match-tags doc-offset phrase-offset))
		       (t nil))))
	       (match-text (d p) 
		 (declare (type fixnum d p))
		 (cond ((and concept-terms 
			     (eq (aref ptext p) (id-for-token "person")))
			(member (aref dtext d)
				(mapcar #'id-for-token
					'("I" "him" "her" "they" "them" "me" "you" "us"))))
		       ((and concept-terms
			     (eq (aref ptext p) (id-for-token "something")))
			(eq (aref dtext d) (id-for-token "it")))
		       (lemma 
			(eq (get-lemma (aref ptext p)) (get-lemma (aref dtext d))))
		       (t (eq (aref ptext p) (aref dtext d)))))
	       (match-tags (d p) 
		 (declare (type fixnum d p))
		 (eq (aref ptags p) (aref dtags d))))
	(loop for offset fixnum from start upto (1- (length dtext))
	   finally (return nil) do
	     (when (match-tokens offset 0)
	       (when (loop for pindex from 0 
			for dindex from offset 
			while (and (< pindex (length ptext))
				   (< dindex (length dtext))) 
			finally (return t) do
			  (unless (match-tokens dindex pindex) (return nil)))
		 (return (values offset (+ offset (1- (length ptext))))))))))))
	       
(defmethod find-phrase-intervals ((p phrase) (doc vector-document) 
				  &key (match :all) (start 0) (lemma nil) (concept-terms nil)
				  &aux results)
  "Find all phrase intervals in the vector document"
  (loop while (< start (1- (length-of doc))) do
       (mvbind (front back) (find-phrase p doc 
					 :match match :start start 
					 :lemma lemma :concept-terms concept-terms)
	       (if (and front back)
		   (progn
		     (push (cons front back) results)
		     (setf start (1+ back)))
		   (return))))
  (nreverse results))

(defmethod find-phrase-intervals ((p array) (doc vector-document) 
				  &key (match :words) (start 0) (lemma nil) (concept-terms nil)
				  &aux results)
  "Find all phrase intervals in the vector document"
  (declare (ignore match)
	   (optimize (speed 3) (space 0) (safety 1))
	   (type fixnum start)
	   (type list results))
;;  (labels ((expand-if-person ()
;;	     (let ((offset (person-token-offset p)))
;;	       (when (> offset -1)
;;		 (setf results 
;		       (mapcan #'(lambda (
  (loop while (< start (1- (length-of doc))) do
       (mvbind (front back) (find-phrase (make-phrase p nil) doc 
					 :match :words :start start :lemma lemma 
					 :concept-terms concept-terms)
	       (declare (type (or fixnum nil) front back))
	       (if (and front back)
		   (progn
		     (push (cons front back) results)
		     (setf start (1+ back)))
		   (return))))
  (nreverse results))

(defun person-token-offset (array)
  (let ((person (id-for-token "person")))
    (loop for token across array 
	  for offset from 0 do
	 (when (eq token person)
	   (return-from person-token-offset t)))))

(defmethod get-annotation ((p phrase) key)
  "First returned value is the association value
   or null if none.  The second is true if the
   key exists, nil otherwise"
  (aif (assoc key (phrase-annotations p))
       (values (cdr it) t)
       (values nil nil)))

(defmethod set-annotation ((p phrase) key value &key (method :override))
  "Add an annotation to object using method :override, :push, :duplicate-key"
  (flet ((set-on-empty () (setf (phrase-annotations p) (acons key value nil))))
    (case method
      (:override 
	(aif (assoc key (phrase-annotations p))
	     (rplacd it value)
	     (set-on-empty)))
      (:push
	(aif (assoc key (phrase-annotations p))
	     (push value (cdr it))
	     (set-on-empty)))
      (:duplicate-key
	(setf (phrase-annotations p) (acons key value (phrase-annotations p))))
      (t (error "Invalid method ~A for add-annotation" method)))
    t))

(defmethod unset-annotation ((p phrase) key)
  (remove key (phrase-annotations p) :key #'car))

(defun make-phrase-from-vdoc (doc start len &optional (type nil))
  (make-instance 'phrase 
		 :type type
		 :document doc
		 :start start
		 :end (+ start len (- 1))))

(defmethod get-token-id ((p phrase) offset)
  (get-token-id (phrase-document p)
	     (+ (phrase-start p) offset)))

(defmethod get-tag ((p phrase) offset)
  (get-tag (phrase-document p)
	      (+ (phrase-start p) offset)))

(defmethod phrase-length ((p phrase))
  (with-slots (end start) p
    (- end start -1)))

(defun print-token-array (tokens start stop &key pos pos-start stream with-tags newline)
  (let ((offset (- pos-start start)))
    (loop for index from start upto stop do
      (if with-tags
	  (format stream "~A/~A " (string-downcase (token-for-id (aref tokens index))) (aref pos (+ index offset)))
	(format stream "~A " (string-downcase (token-for-id (aref tokens index))))))
    (when newline (format stream "~%"))))

(defmethod print-phrase ((p phrase) &key (stream t) (with-tags t) (with-info nil) (newline t))
  (with-slots (text tags) (phrase-document p)
    (when with-info (format stream "~A phrase: " (phrase-type p)))
    (print-token-array text (phrase-start p) (phrase-end p)
		       :pos (if with-tags tags nil)
		       :pos-start (if with-tags (phrase-start p) 0)
		       :with-tags with-tags
		       :stream stream
		       :newline newline)))

(defmethod phrase->string ((p phrase) &key (with-tags nil) (with-info nil) (newline nil))
  (with-output-to-string (stream)
    (print-phrase p :stream stream :with-tags with-tags :with-info with-info :newline newline)))

(defmethod phrase->token-array ((p phrase))
  "Used in conceptnet to index into a node data structure
   NOTE: could be faster with direct, declared array copy"
  (make-array (phrase-length p) :element-type 'fixnum :initial-contents (phrase-words p) :adjustable nil))

(defmethod print-window ((p phrase) wsize &key (stream t) (with-tags t) (with-info nil) (newline t))
  (with-slots (text tags) (phrase-document p)
    (let ((start (limit-min 0 (- (phrase-start p) wsize)))
	  (end (limit-max (1- (length-of (phrase-document p)))
			  (+ (phrase-end p) wsize))))
      (loop for index from start upto end do
	(when with-info (format stream "~A phrase: " (phrase-type p)))
	(if with-tags
	    (format stream "~A/~A " (token-for-id (aref text index)) (aref tags index))
	  (format stream "~A " (token-for-id (aref text index)))))
      (when newline (format stream "~%")))))

(defun token-array->words (tokens)
  (let ((words nil))
    (map-across (lambda (word)
		  (push word words))
		tokens )
    (nreverse words)))
;;  (on-array (cons it rec) nil tokens))

(defun phrase-words (phrase &optional index)
  (assert phrase)
  (cond ((null index)
         (phrase-words phrase (phrase-start phrase)))
        ((>= index (1+ (phrase-end phrase)))
         nil)
        (t (cons (aref (document-text (phrase-document phrase)) index)
		 (phrase-words phrase (1+ index))))))

;; Phrase operations

(defmethod copy-phrase ((p phrase) &optional (annotations t))
  (make-instance 'phrase
		 :type (phrase-type p)
		 :document (phrase-document p)
		 :start (phrase-start p)
		 :end (phrase-end p)
		 :annotations (when annotations (copy-list (phrase-annotations p)))))

(defmethod phrase-distance ((p1 phrase) (p2 phrase))
  "Distance between the nearest end of two phrases"
  (let* ((p1-start (slot-value p1 'start))
         (p1-end (slot-value p1 'end))
         (p2-start (slot-value p2 'start))
         (p2-end (slot-value p2 'end)))
    (cond ((> p2-start p1-end)
	   (- p2-start p1-end))
	  ((> p1-start p2-end)
           (- p1-start p2-end))
	  ((or (> p2-end p1-start) 
	       (> p1-end p2-start))
	   0) 
	  (t (error "I didn't consider a case in phrase-distance")))))

(defmethod phrase-overlap ((ph1 phrase) (ph2 phrase))
  (not (or (< (slot-value ph1 'end) (slot-value ph2 'start))
	   (< (slot-value ph2 'end) (slot-value ph2 'start)))))

(defmethod phrase-equal ((ph1 phrase) (ph2 phrase))
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (and (eq (phrase-length ph1) (phrase-length ph2))
       (loop 
	 with text1 = (document-text (phrase-document ph1)) and
	      text2 = (document-text (phrase-document ph2))
	 for i from (phrase-start ph1) upto (phrase-end ph1)
	 for j from (phrase-start ph2) upto (phrase-end ph2)
	 finally (return t)
	 do
	 (when (neq (aref text1 i) (aref text2 j))
	   (return nil)))))

(defmethod phrase-lemmas ((ph phrase))
  "Returns the lemmatized phrase represented by the underlying phrase"
  (mapcar #'get-lemma-for-id (phrase-words ph)))

(defmethod print-phrase-lemmas ((ph phrase))
  (apply #'concatenate (cons 'string
   (shuffle (mapcar #'token-for-id (phrase-lemmas ph))
	    (repeat " " (1- (phrase-length ph)))))))


;; =========================================================
;; Altered phrase - keep doc refs, but change active content
;; =========================================================

;; Allows us to lemma the original phrase but still
;; perform ops on it as phrases; means lots of generic
;; functions though...
(defclass+ altered-phrase (phrase)
  ((custom-document nil))
  (:prefix "altered-phrase-"))

(defmethod get-token-id ((phrase altered-phrase) index)
  (get-token-id (altered-phrase-custom-document phrase) index))

(defmethod get-tag ((phrase altered-phrase) index)
  (get-tag (altered-phrase-custom-document phrase) index))

(defmethod make-document-from-phrase ((p phrase))
  "Copy referenced phrase data into it's own document"
  (let ((start (phrase-start p))
	(end (phrase-end p))
	(text (document-text (phrase-document p)))
	(tags (document-tags (phrase-document p))))
    (make-instance 'vector-document
		   :text (subseq text start (1+ end))
		   :tags (subseq tags start (1+ end))
		   :annotations nil)))

(defmethod make-alterable-phrase ((p phrase))
  (change-class (copy-phrase p) 'altered-phrase
		:custom-document (make-document-from-phrase p)))

;; Spoof the altered document as the original for most calls
(defmethod phrase-document ((p altered-phrase)) (altered-phrase-custom-document p))
(defmethod phrase-start ((p altered-phrase)) 0)
(defmethod phrase-end ((p altered-phrase)) (1- (length-of (altered-phrase-custom-document p))))
(defmethod phrase-length ((p altered-phrase)) (length-of (altered-phrase-custom-document p)))

;; Mutate the 'phrase' data
(defmethod change-word ((p phrase) index new-token &optional new-pos)
  (change-word (change-class p 'altered-phrase
			     :custom-document (make-document-from-phrase p))
	       index new-token new-pos))

(defmethod remove-word ((p phrase) index)
  (remove-word (change-class p 'altered-phrase
			     :custom-document (make-document-from-phrase p))
	       index))

(defmethod change-word ((p altered-phrase) index new-token &optional new-pos)
  (let ((text (document-text (altered-phrase-custom-document p)))
	(tags (document-tags (altered-phrase-custom-document p))))
    (setf (aref text index) new-token)
    (when new-pos
      (setf (aref tags index) new-pos))
    p))

(defmethod remove-word ((p altered-phrase) index)
  (setf (document-text (phrase-document p)) 
	(vector-1d-lshift (document-text (phrase-document p)) index 1))
  (setf (document-tags (phrase-document p)) 
	(vector-1d-lshift (document-tags (phrase-document p)) index 1)))

(defmethod add-word ((p altered-phrase) index word tag)
  (setf (document-text (phrase-document p)) 
	(vector-1d-rshift (document-text (phrase-document p)) index 1 :filler word))
  (setf (document-tags (phrase-document p)) 
	(vector-1d-rshift (document-tags (phrase-document p)) index 1 :filler tag)))
  


;; =================================
;; Handle destructive lemmatization
;; =================================

(defmethod lemmatize-phrase ((p phrase) &optional (offset 0))
  "Destructive lemmatization of a phrase"
  (let ((doc (phrase-document p))
	(start (phrase-start p)))
    (when (<= offset (phrase-end p))
      (loop for idx from (+ offset start) upto (phrase-end p) do
	(let ((token (get-token-id doc idx)))
	  (awhen2 (get-lemma-for-id token 
				    :pos (get-tag doc idx) 
				    :noun nil)
		  (when (not (eq it token))
		    (return (lemmatize-phrase (change-word p (- idx start) it) (1+ (- idx start))))))))))
  p)

(defparameter *test* nil)
(defmethod lemmatize-phrase ((p altered-phrase) &optional (offset 0))
  "Destructive lemmatization of a phrase"
  (let ((doc (phrase-document p)))
    (setf *test* p)
    (when (<= offset (phrase-end p))
      (loop for idx from (+ offset (phrase-start p)) upto (phrase-end p) do
	(let ((token (get-token-id doc idx)))
	  (awhen2 (get-lemma-for-id token 
				    :pos (get-tag doc idx) 
				    :noun nil)
		  (when (not (eq it token))
		    (change-word p idx it)))))))
  p)
  
  
  
		       
      


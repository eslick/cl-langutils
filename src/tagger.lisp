;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tagger
;;;; Purpose:       Lisp version of the Brill tagger w/ WSJ/BROWN data
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;; ==========================================
;; TAGGER STATE

;; Variables to hold the datafiles
(defvar *tagger-lexical-rules* nil
  "Table to hold the lexical rule closures")
(defvar *tagger-contextual-rules* nil
  "Table to hold the contextual rule closures")
(defvar *tagger-bigrams* nil
  "Bigram hash (not implemented yet)")
(defvar *tagger-wordlist* nil
  "Wordlist hash (not implemented yet)")

;; ==========================================
;; STRING TO STRING TAGGING 
;; Speed: moderately fast
;; Size: Designed for strings on order of Dcache size
 
(defun tag ( string )
  (with-output-to-string (stream)
    (print-vector-document (vector-tag string) :stream stream)))

(defun tag-tokenized ( string )
  (with-output-to-string (stream)
    (print-vector-document (vector-tag-tokenized string) :stream stream)))

;; ===============================
;; FILE TAGGING - slow, small files
;; Speed: slow
;; Input size: Works on small files

(defun read-file-to-string (file)
  (with-output-to-string (stream)
    (with-open-file (file file)
      (do-stream-lines (line file)
	(format stream "~A~%" line)))))

(defun-exported read-file-as-tagged-document (file)
  (vector-tag (read-file-to-string file)))

(defun-exported read-and-tag-file (file)
  (tag (read-file-to-string file)))

;; =================================
;; STRING TO VECTOR-DOCUMENT TAGGING
;; Speed: optimal
;; Input size: < 100k strings

(defun test-vector-tag-tokenized ( string )
  (time (vector-tag-tokenized string)))

(defun vector-tag ( string )
  "Returns a 'document' which is a class containing a pair of vectors 
   representing the string in the internal token format.  Handles arbitrary data."
  (vector-tag-tokenized (mvbind (succ consumed data remainder) (tokenize-string string)
				(declare (ignore succ consumed remainder))
				data)))
(let ((temp-tokens (make-array 100000 :element-type 'fixnum :initial-element 0 :adjustable t))
      (temp-tags (make-array 100000 :element-type 'symbol :initial-element :NN :adjustable t))
      (temp-string (make-array 10000 :initial-element #\a :element-type 'character :fill-pointer t :adjustable nil)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type (array fixnum (*)) temp-tokens)
	   (type (array symbol (*)) temp-tags)
	   (type (array character (*)) temp-string))
  (defun write-temp ( token tag pos )
    (declare (type fixnum token pos)
	     (type symbol tag)
	     (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (setf (aref temp-tokens pos) token)
    (setf (aref temp-tags pos) tag)
    nil)
  
  (defun duplicate-from ( source start end )
    (declare ;;(inline aref)
	     (type fixnum source start end)
	     (optimize (speed 3) (safety 0) (debug 0) (space 0)))
    (loop for pos fixnum from start to (1- end) do
	  (progn 
	    (setf (aref temp-tokens pos) (aref temp-tokens source))
	    (setf (aref temp-tags pos) (aref temp-tags source))
	    nil)))

  (defun vector-tag-tokenized (string &key (end-tokens nil))
    "Returns a document representing the string using the
     internal token dictionary; requires the string to be tokenized.
     Parses the string into tokens (whitespace separators) then populates 
     the two temp arrays above with token id's and initial tags.  Contextual
     rules are applied and a new vector document is produced which
     is a copy of the enclosed data.  This is all done at once so good
     compilers can open-code the array refs and simplify the calling 
     of the labels functions.
    "
    (declare (optimize (speed 3) (safety 1) (space 0) (debug 0)))
    (labels ((initial-tag-all ()
			      (let ((array-offset 3)
				    (temp-index 0))
				(declare (type fixnum temp-index array-offset))
				(loop for char character across string do
				      (if (not (constituent char))
					  (if (= temp-index 0)
					      (continue)
					      (progn (setf (fill-pointer temp-string) temp-index)
						     (mvbind (tokid tagid) (initial-tag temp-string)
							     (write-temp tokid tagid array-offset))
						     (incf array-offset)
						     (assert (< array-offset 99990))
						     (setf temp-index 0)))
					(progn (setf (char temp-string temp-index) char)
					       (incf temp-index))))
				(unless (= temp-index 0)
				  (setf (fill-pointer temp-string) temp-index)
				  (mvbind (tokid tagid) (initial-tag temp-string)
					  (write-temp tokid tagid array-offset))
				  (incf array-offset))
				;; Append end tokens
				(loop for token in end-tokens do
				     (mvbind (tokid tagid) (initial-tag token)
					     (write-temp tokid tagid array-offset))
				     (incf array-offset))
				;; Put valid tokens in beginning and end of pattern-match array
				(duplicate-from 3 0 3)
				(duplicate-from (- array-offset 1) (+ 3 array-offset) (+ 6 array-offset))
				(- array-offset 3))))
      ;; Setup arrays and populate initial tags
      (let ((elements (initial-tag-all)))
	(declare (type fixnum elements))
	;; Run contextual fixup rules
	(apply-contextual-rules elements)
	;; Fresh copy of the resulting data
	(return-vector-doc elements))))

  (defun apply-contextual-rules (elements)
    (declare (optimize (speed 3) (safety 1) (space 0) (debug 0))
	     (type fixnum elements))
;;    (with-print-clock-cycles (1.67 :unit-name "cycles per rule app" :norm-f 
;;			  #'(lambda (cycles) (/ cycles (* elements (length *tagger-contextual-rules*) 1.0))))
      (loop for pos fixnum from 3 upto (+ 3 elements) do
	    (loop for rule function in *tagger-contextual-rules* do
		  (funcall rule temp-tokens temp-tags pos))))

  (defun return-vector-doc (elements)
    (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (make-instance 'vector-document 
		   :text (subseq temp-tokens 3 (+ elements 3))
		   :tags (subseq temp-tags 3 (+ elements 3)))))


(defun initial-tag ( token )
  "Return an initial tag for a given token string using the langutils 
   lexicon and the tagger lexical rules (via guess-tag)"
  (declare (optimize (speed 3) (safety 1) (debug 0) (space 0)))
  (let ((id (id-for-token token nil)))
    (declare (type fixnum id))
    (aif (hash-get *lexicon* id)
	 (values (lexicon-entry-id it) (lexicon-entry-tag it)) ;; token id, best tag
	 (let ((tag (guess-tag token (default-tag token) *tagger-lexical-rules*)))
	   (add-unknown-lexicon-entry id tag)
	   (values id tag)))))

(defun default-tag ( token )
  "Simple default tagging based on capitalization of token string"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (if (and (> (char-code (char token 0)) (char-code #\A))
	   (< (char-code (char token 0)) (char-code #\Z)))
      :NNP
    :NN))

;; ==========================================
;; TAGGER INITIALIZATION

(defun-exported init-tagger (&optional lexical-rule-file contextual-rule-file)
  (write-log tagger-init "Initializing the tagger")
  ;; Handle vector tags and tokens
  (unless (and *tagger-lexical-rules* *tagger-contextual-rules*)
    ;; Load the files
    (load-tagger-files lexical-rule-file contextual-rule-file)))

(defun load-tagger-files ( lexical-rules contextual-rules &key bigrams wordlist )
  (declare (ignore bigrams wordlist))
  (setf *tagger-lexical-rules* (load-lexical-rules lexical-rules *tagger-bigrams* *tagger-wordlist*))
  (setf *tagger-contextual-rules* (load-contextual-rules contextual-rules))
  nil)

(defun-exported clean-tagger ()
  (clean-lexicon)
  (setf *tagger-lexical-rules* nil)
  (setf *tagger-contextual-rules* nil)
  (setf *tagger-bigrams* nil)
  (setf *tagger-wordlist* nil))




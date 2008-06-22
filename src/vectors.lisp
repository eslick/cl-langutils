;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vectors
;;;; Purpose:       Macro system for compiling efficient vector match & side effect rules
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)
#|
- Match pattern to vector(s)
  - integer or byte-level pattern matching
  - Can figure which rule to apply in parallel
  - Can operate on more than one array at a time
- A matching rule results in a side effect
  - a procedure call with
    - source data & focal point
    - current focus only
    - all element matching pattern
  - a direct side effect on the vector

Dictionary for resolving symbolic labels to integers
|#

;; ------------------------------------
;;          VECTOR GROUPS
;; ------------------------------------

(defvar *vector-match-groups* (hash :test #'equal))

(defstruct vector-group
  "A vector group is a set of applicable rules
   to match against a source vector.  Groups can
   be compiled to produce efficient match procedures."
  (name :type string)
  vectors               ;; alist of alists; vector names & attributes
  (rules :type hash)    ;; rule name to rule mapping
  (stats :type hash)    ;; keeps stats on rule use frequency
  (dirty :type boolean) ;; whether new rules have been added
  match-proc)           ;; the match procedure for this group

(defun get-group (name)
  (hash-get *vector-match-groups* name))

(defun vector-group-vector-names (group)
  (mapcar #'car
	  (vector-group-vectors group)))

(defun vector-group-vec-type (gname vname)
  (cdr (assoc :type (cdr (assoc vname (vector-group-vectors (get-group gname)))))))

(defun vector-group-vec-dict (gname vname)
  (cdr (assoc :dict (cdr (assoc vname (vector-group-vectors (get-group gname)))))))

(defmacro-exported undef-vector-group (name)
  `(hash-rem *vector-match-groups* ',name))

(defmacro-exported def-vector-group (name &rest vector-descriptions)
  (let ((group (gensym)))
  `(eval-when (compile eval load)
     (let ((,group (make-vector-group-name :name ',(localize-symbol name) :rules (hash) :stats (hash))))
       (hash-put *vector-match-groups* ',(localize-symbol name) ,group)
       (setf (vector-group-vectors ,group)
	     ',(mapcar #'(lambda (vector) 
			   (if (listp vector)
			       (destructuring-bind (name &key type dict) vector
				 `(,name . ((:type . ,type) (:dict . ,dict))))
			     `(,vector nil)))
		       vector-descriptions))
       ',name))))

(def-vector-group tagged-text
  (tokens :type (simple-vector fixnum)
	  :dict #'id-for-token)
  (tags :type (simple-vector unsigned-byte)
	:dict #'if-for-tag))

;; ------------------------------------
;;       VECTOR MATCH PATTERNS
;; ------------------------------------

(def-vector-pattern assoc-left contextual-rules
  "Name/purpose of rule"
  (tags   (PP JJ nil *NN nil nil NN))
  (tokens (          *foo))
  -> 
  '(setf (svref tags focus) (svref tags (- focus 1))))
;;  (match #'capture-matches)
;;  (pattern #'store-pattern)
;;  (focal #'capture-target))

(defstruct vector-pattern filter-proc result-proc patterns)
  

(defmacro-exported def-vector-pattern (vname gname &rest productions)
  (with-gensyms (gsym vsym)
    (unless (get-group gname) (error "Group ~A is not defined.~%" gname))
    (mvbind (patterns consequent) (split-list '-> productions)
      `(let ((,gsym (get-group ,gname)))
	 (when (null patterns) (error "No patterns found.~%"))
	 (hash-put (vector-group-rules ,gsym)
		   ,name
		   ,(construct-make-vp (get-group gname) patterns consequent)
		   
(defun construct-make-vp (group patterns consequent)
  (make-vector-pattern 
   :filter-proc #'(lambda ,(vector-group-vector-names (get-group group))
		    (declare (inline match-vectors))
		    ,@(get-vector-declarations group
					       (and ,@(mapcar #'(lambda (vsym)
								  `(match-vectors ,vname (cdr (assoc ,vname ,patterns)))))))))
   :result-proc #'(lambda ,(vector-group-vector-names (get-group group))
		    ,@consequent)
   :patterns ,patterns))

(make-vector-pattern
 :filter-proc #'(lambda (

(vector-match group (tokens tags) [(start end)|pos]
	      :mode [:compiled|:iterate])

;; Compiler

Can apply rules one at a time (debug), default is to compile if not compiled

Want to minimize the # of rules applicable after every decision point
Build a tree of decision points, weighed by any frequency data
emit a decision tree
bottom runs code or calls function

(defun find-best-strategy ()
  "Returns an tree of positions to check that minimizes total, weighted rule cost"
  )

(defun build-decision-tree (decision-tree)
  "Constructs a code sequence to implement the decision tree"
  )

(defun compile-group-rules (group)
  "Returns a procedure that applies all rules to the
   structures defined in group"
  (compile (build-decision-tree

(defun generate-production-call (expr)
  "Generates the code for a call
  



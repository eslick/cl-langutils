;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vector-keyed-table.lisp
;;;; Purpose:       Fast vector element hash implementation
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  August 2004
;;;;

(in-package :langutils)

;; ----------------------------------------------------------
;; VECTOR-HASH -- Vector Keyed Hash Trees
;;
;; Tree of hashes for fast lookup of integer simple-vectors keys
;; over very large vector spaces
;;
;; Assumptions:
;; * Vector elements are positive integers 1...n
;; * You are loading thousands of elements (expensive otherwise)
;; * Sparsely populated tree 
;; ----------------------------------------------------------

(defparameter *vector-keyed-threshold* 5
  "When to switch internally from assoc list to hash table")

(defparameter *vechash-threshold* 20)

(defclass-exported vector-keyed-table (table)
  ((root :accessor table-root 
	 :initarg :root)
   (threshold :reader vhash-threshold
	      :initarg :threshold
	      :initform *vechash-threshold*)
   (count :accessor table-count
	  :initform 0)))

(defmethod-exported initialize-instance :after ((table vector-keyed-table) &rest initargs &key &allow-other-keys)
  (clear table))

;;
;; ----------------------
;;

(defparameter *count-updated* nil)
	   
(defun make-vknode (&optional value table)
  (cons value table))

(defun vknode-value (node) 
  (car node))

(defsetf vknode-value rplaca)

(defun vknode-table (node) 
  (cdr node))

(defsetf vknode-table rplacd)

(defun end-of-vkey (vkey index) 
  (= (length vkey) (1+ index)))

(defun extend-vktable (table key)
  "Add a vk node to the table for key"
  (add table key (make-vknode)))

(defun upgrade-vknode (node)
  "Upgrade vktable entry from assoc to hashed when the size
   exceeds the vk threshold.  Do this to the table in the
   provided vknode. Also downgrade (is this a good idea as
   it may cause thrashing?)"
  (when node
    (let ((ctable (vknode-table node)))
      (when ctable
	(cond ((and (subtypep (type-of ctable) 'assoc-table)
		    (>= (size-of ctable) *vector-keyed-threshold*))
	       (setf (vknode-table node) (convert ctable 'hashed-table))))))))
;;	      ((and (subtypep (type-of ctable) 'hashed-table)
;;		    (< (size-of ctable) *vector-keyed-threshold*))
;;	       (setf (vknode-table node) (convert ctable 'assoc-table))))))))

(defun ensure-vktable (node)
  "Ensure that the provided node has a proper table for 
   the next recusion of vector-keyed-put"
  (aif-ret (vknode-table node)
	   (setf (vknode-table node) (make-instance 'assoc-table))
	   (vknode-table node)))

(defun vktable-get-extend (vktable node key &aux (table (ensure-vktable node)))
  "Get node from table.  If new node, update vktable item count
   add a new node to the table for key.  If table has exceeded
   size, upgrade it to a hashed table and return the new node"
  (aif-ret (get-value table key)
	   (unless *count-updated* 
	     (incf (table-count vktable))
	     (setq *count-updated* t))
	   (extend-vktable table key)
	   (upgrade-vknode node)
	   (get-value (vknode-table node) key)))

(defun vector-keyed-put (vktable vkey value)
  "Internal recursion to walk tree and add or modify a value for
   vector key: vkey"
  (setf *count-updated* nil)
  (labels ((rec (node index)
	     (let* ((key (aref vkey index))
		    (nextn (vktable-get-extend vktable node key)))
	       (if (end-of-vkey vkey index)
		   (progn (setf (vknode-value nextn) value) value)
		   (rec nextn (1+ index))))))
    (rec (make-vknode nil (table-root vktable)) 0)))

(defun drop-vknode-value (tnode stack)
  "Clear value in target node (tnode) and if 
   alist is size zero or nil, delete entry in 
   parent table (snode) and, if zero, also delete"
   (if (or (null (vknode-table tnode))
	   (empty (vknode-table tnode)))
       (dbind (key . nextn) (car stack)
	      (drop-vktable-entry key nextn (cdr stack)))
       (setf (vknode-value tnode) nil)))
   
(defun drop-vktable-entry (key node stack)
  (drop (vknode-table node) key)
  (when (and stack 
	     (empty (vknode-table node))
	     (null (vknode-value node)))
    (dbind (key . nextn) (car stack)
	   (drop-vktable-entry key nextn (cdr stack)))))

(defun vector-keyed-rem (vktable vkey)
  "Remove a vector keyed value from the vktable
   and clean up any empty nodes or tables created
   thereby.  Also decrement the count"
  (let ((stack nil))
    (labels ((rec (node index)
	       (when node
		 (let* ((key (aref vkey index))
			(table (vknode-table node))
			(nextn (when table (get-value table key))))
		   (push (cons key node) stack)
		   (when nextn
		     (if (end-of-vkey vkey index)
			 (progn
			   (drop-vknode-value nextn stack)
			   (decf (table-count vktable))
			   (when (empty vktable)
			     (clear vktable))
			   t)
			 (rec nextn (1+ index))))))))
      (rec (make-vknode nil (table-root vktable)) 0))))

(defun vector-keyed-get (vktable vkey)
  "Internal recursion to walk tree and return value for vector
   key: vkey"
  (labels ((rec (table index)
	     (when table
	       (awhen (get-value table (aref vkey index))
		 (if (end-of-vkey vkey index)
		     (vknode-value it)
		     (rec (vknode-table it) (1+ index)))))))
    (rec (table-root vktable) 0)))

;;
;; ----------------------
;;
	   
(defmethod get-value ((table vector-keyed-table) key)
  (assert (subtypep (type-of key) 'array))
  (vector-keyed-get table key))

(defmethod (setf get-value) (value (table vector-keyed-table) key)
  (assert (subtypep (type-of key) 'array))
  (vector-keyed-put table key value))

(defmethod drop ((table vector-keyed-table) key)
  (assert (subtypep (type-of key) 'array))
  (vector-keyed-rem table key))

(defmethod clear ((table vector-keyed-table))
  (setf (table-root table) 
	(make-instance 'hashed-table
		       :hash (make-hash-table :test #'eq :size 1000 :rehash-size 1.5 :rehash-threshold 0.7)))
  (setf (table-count table) 0)
  t)

(defmethod size-of ((table vector-keyed-table))
  (table-count table))

(defmethod storage-allocated ((table vector-keyed-table))
  ;; NOTE: TODO
  )

(defclass vector-keyed-table-iterator (iterator)
  ((reference :accessor reference :initarg :reference)
   (type :accessor iter-type :initarg :type)
   (last :accessor last-key :initform nil)
   (stack :accessor vkti-stack :initform nil)))

(defmethod get-iterator ((vktable vector-keyed-table) &key (type :pair))
  (make-instance 'vector-keyed-table-iterator
		 :reference vktable
		 :type type))

(defmethod initialize-instance :after ((iter vector-keyed-table-iterator) &rest initargs &key &allow-other-keys)
  (reset iter))

(defmacro mvpass2 (form)
  `(aif2 ,form
	 (values it t)
	 (values nil nil)))

(defmethod next-value ((iter vector-keyed-table-iterator))
  "Invariant: stack always contains a null, exhausted or intermediate table iterator"
  (with-slots (stack) iter
    (cond ((null stack)
	   (values nil nil))
	  ((or (null (car stack))
	       (not (next-p (car stack))))
	   (pop stack)
	   (pop (last-key iter))
	   (mvpass2 (next-value iter)))
	  (t 
	   (mvpass2 (vkti-next-value iter))))))

(defun vkti-next-value (iter)
  (with-slots (stack) iter
    (let* ((iterator (car stack))
	   (kvpair (next-value iterator))
	   (key (car kvpair))
	   (node (cdr kvpair)))
      (push key (last-key iter))
      (aif (vknode-table node)
	   (push (get-iterator it) stack)
	   (push nil stack))
      (aif (vknode-value node)
	   (values (extract-assoc-type (cons (list->array (reverse (last-key iter))) it) 
				       (iter-type iter))
		   t)
	   (mvpass2 (next-value iter))))))

(defmethod-exported next-p ((iter vector-keyed-table-iterator))
  (with-slots (stack) iter
    (and stack
	 (not (every (lambda (iter)
		       (or (null iter) (not (next-p iter))))
		     stack)))))

(defmethod-exported drop-last ((iter vector-keyed-table-iterator))
  (awhen (last-key iter)
    (drop (reference iter) (list->array (reverse it)))
    (setf (last-key iter) nil)))

(defmethod-exported reset ((iter vector-keyed-table-iterator))
  (setf (vkti-stack iter)
	(list (get-iterator (table-root (reference iter))))))
 
(defmethod-exported clear ((iter vector-keyed-table-iterator))
  (setf (vkti-stack iter) nil))


;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          contextual-rule-parser
;;;; Purpose:       Macro for generating the brill rule parser, used only in tagger-data.lisp
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  October 2004
;;;;

(in-package :langutils)

;;;
;;; Macro system to build parser/generator (make-contextual-rule)
;;; 

(defmacro def-contextual-rule-parser (name &body template-list)
  "Given a list of structures, defines a generator named 'name' that takes 
   a Brill contextual rule list (list of strings) and generates an applicable 
   closure. The closure accepts an argument list of (tokens tags offset) and will 
   apply the rule and related side effect to the two arrays at the provided
   offset.  Patterns are to be given in the form:
   (\"SURROUNDTAG\" (match (0 oldtag) (-1 tag1) (+1 tag2)) => 
                    (setf oldtag newtag))"
  `(defun ,name (pattern)
     (declare (optimize (speed 3) (safety 0) (debug 0)))
;;	      (inline svref aref))
     (let ((name (string-upcase (third pattern))))
       (cond ,@(mapcar #'gen-rule-closure template-list)
	     (t (write-log tagger-contextual "Unrecognized rule: ~A" (first pattern)))))))

(defun gen-rule-closure (template)
  "Generate the code for the rule closure as one of the cond 
   forms matching the name of the closure pattern to the 
   rule pattern"
  (let ((rule-name (first template))
	(match-pattern (second template))
	(newtok-name (fourth template)))
    `((string= name ,rule-name)
      (let (,@(gen-rule-arg-bindings match-pattern)
	    ,(get-bind-entry newtok-name))
	,(gen-rule-arg-decls template)
	(lambda (tokens tags pos)
	  ,(gen-rule-closure-decl)
	  (if ,(gen-rule-match template)
	      (progn
;;		(write-log tagger-contextual ,(format nil "~A: ~~A @ ~~A" (first template)) pattern pos)
		(setf (svref tags pos) ,newtok-name))))))))

(defun gen-rule-closure-decl ()
  "Optimize the compiled closure through
   type and optimization declarations"
  '(declare (ignorable tokens)
	    (type (simple-array fixnum (*)) tokens)
	    (type (simple-array symbol (*)) tags)
	    (type fixnum pos)
	    (optimize (speed 3) (safety 0) (debug 0) (space 0))))

(defparameter *contextual-rule-args*
  (list (list 'tag1 'tags 'symbol '(mkkeysym (fourth pattern)))
	(list 'tag2 'tags 'symbol '(mkkeysym (fifth pattern)))
	(list 'word1 'tokens 'fixnum '(id-for-token (fourth pattern)))
	(list 'word2 'tokens 'fixnum '(id-for-token (fifth pattern)))
	(list 'oldtag 'tags 'symbol '(mkkeysym (first pattern)))
	(list 'newtag 'tags 'symbol '(mkkeysym (second pattern))))
  "The templates for parsing contextual rules and constructing 
   matching templates over word/pos arrays")

(defun get-bind-entry (var)
  "Given a canonical variable name, create its let binding
   and extraction expression from the rule file entry"
  (aif (find var *contextual-rule-args* :key #'car)
       (list (first it) (fourth it))
       (error "Invalid variable in template pattern: ~A" var)))

(defun gen-rule-arg-bindings (pattern)
  "Generate let bindings for the args referenced in the match pattern"
  (labels ((get-arg (match-rec)
		    (when (consp match-rec)
		      (second match-rec))))
    (loop for exp in pattern nconcing 
	  (cond ((atom exp) nil)
		((not (consp exp))
		 (format t "Parser error in macro, '(+1 tag1) form expected got ~A" exp))
		((eq (car exp) 'or)
		 (gen-rule-arg-bindings (list (cadr exp))))
		(t (list
		    (get-bind-entry
		     (get-arg exp))))))))

(defun gen-rule-arg-decls (pattern)
  "Generate type declarations for canonical variables from table entry"
  (labels ((get-arg-type (var)
			 (awhen (find var *contextual-rule-args* :key #'car)
				(third it)))
	   (make-arg-decl (match-rec)
			  (when (consp match-rec)
			    `(type ,(get-arg-type (second match-rec)) ,(second match-rec))))
	   (find-args (exps)
		      (loop for exp in exps nconcing
			    (cond ((atom exp) nil)
				  ((not (consp exp))
				   (format t "Parser error in macro, '(-1 tag1) form expected got: ~A" exp))
				  ((eq (car exp) 'or)
				   (find-args (cdr exp)))
				  (t (list 
				      (make-arg-decl exp)))))))
    `(declare ,@(find-args (second pattern)))))

(defun gen-rule-match (pattern)
  "Generate the conditional code to match this rule"
  (labels ((get-array-name (var) (second (find var *contextual-rule-args* :key #'car)))
	   (get-pos-stmt (offset) (case offset
				    (0 'pos)
				    (+1 '(+ pos 1))
				    (+2 '(+ pos 2))
				    (+3 '(+ pos 3))
				    (-1 '(- pos 1))
				    (-2 '(- pos 2))
				    (-3 '(- pos 3))))
	   (parse-match-stmt (stmt)
			     (cond ((eq (first stmt) 'or)
				    `(or ,@(mapcar #'parse-match-stmt (cdr stmt))))
				   (t (let ((offset (first stmt))
					    (var (second stmt)))
					`(eq ,(case (get-array-name var)
						(tags `(aref tags ,(get-pos-stmt offset)))
						(tokens `(aref tokens ,(get-pos-stmt offset))))
					     ,var))))))
    `(and ,@(mapcar #'parse-match-stmt (cdr (second pattern))))))

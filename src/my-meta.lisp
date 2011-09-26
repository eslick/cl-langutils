;;;;										;
;;;; (c) 2001 by Jochen Schmidt.
;;;;
;;;; File:            meta.lisp
;;;; Revision:        1.0.0
;;;; Description:     A simple parsing technique
;;;; Date:            01.07.2001
;;;; Authors:         Jochen Schmidt
;;;; Tel:             (+49 9 11) 47 20 603
;;;; Email:           jsc@dataheaven.de
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED "AS IS" AND THERE ARE NEITHER 
;;;; EXPRESSED NOR IMPLIED WARRANTIES -  THIS INCLUDES, BUT 
;;;; IS NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;;;; AND FITNESS FOR A PARTICULAR PURPOSE.IN NO WAY ARE THE
;;;; AUTHORS LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ;
;;;; LOSS OF USE, DATA, OR PROFITS			; OR BUSINESS INTERRUPTION)
;;;; 
;;;; For further details contact the authors of this software.
;;;;
;;;;  Jochen Schmidt        
;;;;  Zuckmantelstr. 11     
;;;;  91616 Neusitz         
;;;;  GERMANY               
;;;;
;;;;
;;;; NOTE:
;;;; This code is based on the well known paper "Pragmatic Parsing in Common Lisp"
;;;; of Henry G. Baker. You can find it at:
;;;;
;;;;    http://linux.rice.edu/~rahul/hbaker/Prag-Parse.html
;;;;
;;;; The parsing technique Baker describes in his paper goes back to:
;;;;
;;;;     Schorre, D.V.  "META II: A Syntax-Oriented Compiler Writing Language".
;;;;       Proc. 19'th Nat'l. Conf. of the ACM (Aug. 1964),D1.3-1-D1.3-11.
;;;;
;;;;
;;;; Nurnberg, 01.Jul.2001 Jochen Schmidt

;;;; Extensions by Ian Eslick, MIT Media Laboratory 2005

(in-package :my-meta)

;;; String matching
(defmacro string-match (x &key source-symbol)
  (etypecase x
    (character
     `(when (and (< index end) (eql (char ,source-symbol index)  ,x))
        (incf index)))
    (string
     (let ((old-index-symbol (gensym "OLD-INDEX-")))
       `(let ((,old-index-symbol index))
          (or (and ,@(map 'list #'(lambda (c) `(string-match ,c
                                                             :source-symbol ,source-symbol)) x))
              (progn (setq index ,old-index-symbol) nil)))))))

(defmacro string-match-type (x v &key source-symbol)
  (let ((char-sym (gensym)))
    `(when (< index end)
       (let ((,char-sym (char ,source-symbol index)))
         (declare (base-char ,char-sym))
         (when (typep ,char-sym ',x)
           (setq ,v ,char-sym) (incf index))))))


;;; List matching
(defmacro list-match (x &key source-symbol); sublist uses new lexical index
 `(when (and (consp ,source-symbol)
             ,(if (atom x) `(eql (car ,source-symbol) ',x)
                `(let ((,source-symbol (car ,source-symbol))) ,(compile-list x :source-symbol source-symbol))))
    (pop ,source-symbol) t))

(defmacro list-match-type (x v &key source-symbol)
  `(when (and (consp ,source-symbol) (typep (car ,source-symbol) ',x))
     (setq ,v (car ,source-symbol)) (pop ,source-symbol) t))

(defun compile-list (l &key source-symbol)
  (if (atom l) `(eql ,source-symbol ',l)
      `(and ,(compileit (car l) :meta-parser-type :list :source-symbol source-symbol)
            ,(compile-list (cdr l) :source-symbol source-symbol))))


;;; Stream matching
(defmacro stream-match (x &key source-symbol)
  `(when (eql (peek-char nil ,source-symbol) ',x) (read-char ,source-symbol)))

(defmacro stream-match-type (x v &key source-symbol)
  `(when (typep (peek-char nil ,source-symbol) ',x) (setq ,v (read-char ,source-symbol))))

(defstruct (meta
            (:print-function
             (lambda (m s d &aux (char (meta-char m)) (form (meta-form m)))
               (declare (ignore d))
               (ecase char
                 ((#\@ #\! #\$) (format s "~A~A" char form))
                 (#\[ (format s "[~{~A~^ ~}]" form))
                 (#\{ (format s "{~{~A~^ ~}}" form))))))
    char
    form)

(defun symbol-name-equal (src target &key key (test #'equal))
  (funcall test (symbol-name src) (symbol-name (if key (funcall key target) target))))

(defun compileit (x &key meta-parser-type source-symbol)
	(typecase x
		(meta
			(ecase (meta-char x)
				(#\! (cond ((symbol-name-equal 'meta-dict (meta-form x) :key #'first)
					    (let ((dict (eval (second (meta-form x)))))
					      (unless (consp dict)
						(error "Meta Dictionaries are lists of strings, 
                                                        provided input is not list"))
					      `(or ,@(mapcar #'(lambda (f) (compileit f
										      :meta-parser-type meta-parser-type
										      :source-symbol source-symbol))
							     dict))))
					   (t (meta-form x))))
				(#\[ `(and ,@(mapcar #'(lambda (f) (compileit f
                                                              :meta-parser-type meta-parser-type
                                                              :source-symbol source-symbol))
                                                     (meta-form x))))
				(#\{ `(or ,@(mapcar #'(lambda (f) (compileit f
                                                             :meta-parser-type meta-parser-type
                                                             :source-symbol source-symbol))
                                                    (meta-form x))))
				(#\$ `(not (do () ((not ,(compileit (meta-form x)
                                                    :meta-parser-type meta-parser-type
                                                    :source-symbol source-symbol))))))
				(#\@ (let ((f (meta-form x))) (list (ecase meta-parser-type
                                                                      (:list 'list-match-type)
                                                                      (:string 'string-match-type)
                                                                      (:stream 'stream-match-type))
                                                                    (car f) (cadr f)
                                                                    :source-symbol source-symbol
                                                                    )))))
		(t (list (ecase meta-parser-type
                           (:list 'list-match)
                           (:string 'string-match)
                           (:stream 'stream-match))
                         x
                         :source-symbol source-symbol
                         ))))

(defparameter *saved-readtable* (copy-readtable))
(defparameter *meta-readtable* (copy-readtable))

(defun meta-reader (s c) (make-meta :char c :form (read s)))


(mapc #'(lambda (c) (set-macro-character c #'meta-reader nil *meta-readtable*)) '(#\@ #\$ #\!))

(set-macro-character #\{
	#'(lambda (s c) (make-meta :char c :form (read-delimited-list #\} s t))) nil *meta-readtable*)

(set-macro-character #\[
	#'(lambda (s c) (make-meta :char c :form (read-delimited-list #\] s t))) nil *meta-readtable*)

(mapc #'(lambda (c) (set-macro-character c (get-macro-character #\))  nil *meta-readtable*))
	'(#\] #\}))


(defmacro with-stream-meta ((source-symbol stream) &body body)
  `(let ((,source-symbol ,stream))
     (macrolet ((meta-match (x)
                       (compileit x
                                  :meta-parser-type :stream
                                  :source-symbol ',source-symbol)))
       ,@body)))

(defmacro with-string-meta ((source-symbol string-buffer &key (start 0) end) &body body)
  `(let* ((,source-symbol ,string-buffer)
          (index ,start)
          (end ,(or end `(length ,source-symbol))))
     (declare (fixnum index end)
              (type simple-base-string ,source-symbol))
;;	      (type base-string ,source-symbol))
     (macrolet ((meta-match (x)
                       (compileit x
                                  :meta-parser-type :string
                                  :source-symbol ',source-symbol)))
             ,@body)))


(defmacro with-list-meta ((source-symbol list) &body body)
  `(let ((,source-symbol ,list))
     (macrolet ((meta-match (x)
                       (compileit x
                                  :meta-parser-type :list
                                  :source-symbol ',source-symbol)))
       ,@body)))

(defun enable-meta-syntax ()
	(copy-readtable *meta-readtable* *readtable*))

(defun disable-meta-syntax()
	(copy-readtable *saved-readtable* *readtable*))

(provide 'meta)

#|

(eval-when (compile load eval)
  (deftype digit () '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (deftype non-digit () '(not (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

  (defun ctoi (d) (- (char-code d) #.(char-code #\0)))
)

(eval-when (compile load eval)
  (enable-meta-syntax)
)


(defun parse-int (string &aux (s +1) d (n 0))
  (with-string-meta (buffer string)
                    (and
                     (match
                      [{#\+ [#\- !(setq s -1)] []}
                            @(digit d) !(setq n (ctoi d))
                            $[@(digit d) !(setq n (+ (* n 10) (ctoi d)))]])
                     (* s n))))

(eval-when (compile load eval)
(disable-meta-syntax)
)

|#

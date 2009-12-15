;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tokenize
;;;; Purpose:       Fast recursive descent stream and string tokenizer based on meta
;;;;                
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  September 2004
;;;;

(in-package :langutils)

(eval-when (compile eval load)
  (defvar known-abbreviations 
    '("Apr" "Assn" "Aug" "Av" "Ave" "Bldg" "Cf" "Co"
      "Corp" "Ct" "Dec" "Dept" "Dist" "Dr" "Eq" "Feb" "Fig" "Figs"
      "Gov" "Inc" "Jan" "Jas" "Jr" "Jul" "Jun" "Lt" "Ltd" "MM"
      "Mar" "May" "Mfg" "Mme" "Mr" "Mrs" "Ms" "Msec" "Mt" "Mts"
      "No" "Nov" "Oct" "Op" "Rd" "Rep" "Rte" "Sen" "Sep" "Sr"
      "St" "Stat" "Tech" "USN" "Vol" "Vs" "Yo" "a" "al" "apr"
      "aug" "bur" "ca" "cc" "cf" "cf" "cm" "cu" "dec" "dia" "ed"
      "eds" "eg" "eqn" "eqns" "est" "etc" "ex" "feb" "fig" "figs"
      "ft" "gm" "hp" "hr" "jan" "jul" "jun" "kc" "l" "lb" "lbs"
      "m" "mEq" "mar" "may" "mc" "mg" "mil" "min" "ml" "mm" "mos"
      "nov" "nw" "oct" "oz" "p" "pl" "pp" "prop" "r" "repr" "rev"
      "sec" "sep" "sq" "trans" "v" "var" "viz" "vol" "vols" "vs"
      "yd" "yrs")))

;;(defun ctoi (d) (- (char-code d) #.(char-code #\0)))
(deftype digit () '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(deftype non-digit () '(not (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
(deftype non-digit-or-ws () '(not (or whitespace non-digit)))

(deftype whitespace () '(member #\Tab #\Space #\Newline #\Return))
(deftype non-whitespace () '(not (member #\Tab #\Space #\Newline #\Return)))

(defun alpha-lowercase (ch)
  "Return T if the given character is an alpha character"
;;  (and (>= (char-code ch) #\a) (<= (char-code ch) #\z)))
  (member ch
	  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
(defun alpha-uppercase (ch)
;;  (and (>= (char-code ch) #\A) (<= (char-code ch) #\Z)))
  (member ch
	  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))
(defun alpha-misc (ch)
  (member ch '(#\_ #\-)))

(deftype alpha () '(or (satisfies alpha-lowercase)
			 (satisfies alpha-uppercase)
			 (satisfies alpha-misc)))

(deftype alpha-upper () '(satisfies alpha-uppercase))
(deftype alpha-lower () '(satisfies alpha-lowercase))
  
(deftype punctuation () '(member #\] #\[ #\? #\! #\( #\) #\\ #\" #\; #\{ #\} #\: #\, #\/ #\'))

(deftype alphanum () '(or digit alpha punctuation))
(deftype non-punc-or-white () '(not (or whitespace punctuation)))

(define-condition end-of-sentence (condition) ())

;;
;; NOTE: make result fixed in size, run tokenizer in passes and
;; stitch results together
;;

;; Reads a stream into a string
#.(enable-meta-syntax)
(let* ((length 1024)
       (result (make-array length :element-type 'character :adjustable t)))
  (declare (type fixnum length))
(defun tokenize-stream (stream &key (by-sentence nil) (fragment "")
			       &aux (index 0) (start 0) (ch #\Space) (ws #\Space)
			            (status :running) (sentence? nil))
  "Converts a stream into a string and tokenizes, optionally, one sentence 
   at a time which is nice for large files.  Pretty hairy code: a token 
   processor inside a stream scanner.  The stream scanner walks the input stream
   and tokenizes all punctuation (except periods).  After a sequences of 
   non-whitespace has been read, the inline tokenizer looks at the end of the 
   string for mis-tokenized words (can ' t -> ca n't)"
  (declare (type fixnum index start)
	   (type character ch ws)
	   (type boolean sentence?)
;;	   (inline peek-char read-char char)
	   (optimize (speed 3) (safety 0) (debug 1)))
  (with-stream-meta (str stream)
      (macrolet ((copy-fragment ()
				`(progn 
				   (loop for i fixnum from 0 to (1- (length fragment)) do
				     (setf (char result i) (char fragment i)))))
		 (make-result-buffer ()
				     `(progn
					(setq index (length fragment))
					(setq result (make-array index
								 :element-type 'character
								 :adjustable t
								 :initial-contents fragment))
					(when (> length index)
					  (setq result (adjust-array result length)))))
		 (extend-result-buffer ()
				       `(when (> index (/ length 2))
					  (setq length (* length 2))
					  (setq result (adjust-array result length))))
		 (array-to-string ()
				  `(progn
				     (subseq result 0 index)))
		 (write-ws () `(progn 
				 (setf (char result index) #\Space)
				 (incf index)
				 t))
		 (write-ch () `(progn
;;				 (format t "writing ~A at ~A~%" ch index)
				 (setf (char result index) ch)
				 (incf index)
				 t))
		 ;; This split of sentence detection is because cap letters in the next sentence 
		 ;; are the only exception to single token analysis.  I'll have to refactor this if
		 ;; we need next token lookahead in the future for other punctuation fixup.  
		 ;; Potential sentence just indicates that the period was not otherwise classified
		 (potential-sentence () 
				     `(progn 
;;					(format t "Potential!~%") 
					(setq sentence? t)))
		 ;; If the first letter of the next token is a capital, insert newline 
		 ;; to indicate sentence boundary
		 (check-sentence ()
				 '(progn
				    (when sentence? 
;;   				      (format t "Checking sentence: ~A~%" ch)
				      (if (typep ch 'alpha-upper)
					  (make-sentence)
					(setq sentence? nil)))
				    t)))
	;; BACKUP AND CLEANUP MISPARSED TOKEN SEQUENCES & IDENTIFY POTENTIAL SENTENCE BOUNDARIES
	(labels ((process-token-inline (&aux (write-index index))
                   (declare ;;(inline peek-char)
			    (type fixnum write-index start index)
			    (optimize speed (safety 0)))
		   (when (= start index) (return-from process-token-inline t))
		   (let ((new-index
			  (with-string-meta (string result :start (if (= start 0) 0 (1- start)) :end write-index)
;;			    (format t "~A ~A \"~A\"~%" index end (subseq result index end))
			    (labels ((test-alpha ()
						 (let* ((ch (char string index))
							(code (char-code ch)))
						   (or (and (>= code (char-code #\a))
							    (<= code (char-code #\z)))
						       (and (<= code (char-code #\A))
							    (>= code (char-code #\Z)))
						       (or (eq code (char-code #\_))
							   (eq code (char-code #\-))))))
				     (swap (a b &aux temp)
					   (declare (optimize speed (safety 0)))
;;						    (inline char))
					   (setf temp (char result a))
					   (setf (char result a) (char string b))
					   (setf (char result b) temp))
				     (write-newline (pos)
						    (declare (type fixnum pos)
							     (optimize speed (safety 0)))
;;							     (inline char))
						    (setf (char result pos) #\Newline))
				     ;; Go back to provided write ptr and walk forward to the
				     ;; original 'end' of valid data removing any spaces and
				     ;; decrementing the end and index to keep track of 
				     ;; the actual end of the string.
				     (delete-spaces (write spaces)
						    (delete-chars #\Space write spaces))
				     (delete-chars (char write count &aux (read write) (orig write))
						    (declare (type fixnum write read count)
;;							     (type character char)
							     (type (vector character) result)
							     (optimize speed (safety 0)))
						    (loop while (< write end) do
						      (if (and (< (- read write) count)
							       (eq (char result read) char))
							  (progn
							    (incf read)
							    (decf index)
							    (decf end))
							(progn
;;							  (format t "w~A:'~A' <- r~A:'~A'~%"
;;								  write (char string write)
;;								  read (char string read))
							  (setf (char result write)
								(char result read))
							  (incf write)
							  (incf read))))
;;						    (format t "after delete: ~A~%" (subseq string orig (1- end)))
						    t)
				     ;; Fix contractions of the type I'll (I ' ll -> I 'll)
				     (fix-will (&aux (old-index index)) 
					       (or
						(meta-match
						 [#\Space #\' #\Space #\l #\l !(delete-spaces (- index 3) 1)])
						 (progn (setq index old-index) nil)))
				     ;; Fix possessives such as Fred (Fred ' s -> Fred's)
				     (fix-poss (&aux (old-index index) a d) 
					       (declare (type fixnum old-index index)
;;							(type character a d)
							(optimize speed (safety 0)))
					       (or
						(meta-match
						 [{!(test-alpha) @(digit d)} #\Space #\' #\Space #\s 
						 !(delete-spaces (- index 4) 2)])
						(progn (setq index old-index) nil)))
				     ;; Fix contractions of the type can't.  (can ' t -> ca n't)
				     (fix-cant (&aux (old-index index)) 
					       (or
						(meta-match
						 [#\n #\Space #\' #\Space #\t 
						 !(swap (- index 4) (- index 5))
						 !(delete-spaces (- index 2) 1)])
						(progn (setq index old-index) nil)))
				     ;; Repair tokens within numbers so they're recognizable later
				     (fix-numerics (&aux (old-index index) d);; 2 : 00 -> 2:00 
						   (or
						    (meta-match
						    [@(digit d) {#\Space} { #\: #\, #\/ } { #\Space } @(digit d)
						    !(delete-spaces (- index 4) 2)])
						    (progn (setq index old-index) nil)))
				     ;; Ignore abbreviations from the list and keep period marker
				     (fix-abbreviations (&aux (old-index index) ws)
							(or 
							 (meta-match 
							  [ @(whitespace ws) !(meta-dict known-abbreviations) 
							    #\. @(whitespace ws)])
							 (progn (setq index old-index) nil)))
				     ;; Keep tokens shortened using periods U.S.A, Ian S. Eslick, and so on
				     (fix-periods (&aux (old-index index) a)
						  (declare (optimize speed (safety 0))
;;							   (inline aref char)
							   (type string result)
							   (type fixnum old-index index))
;;							   (type character a ws))
;;						  (format t "~A~%" (subseq string index end))
						  (or 
						   (meta-match  ;; Assume sentence if not pass these filters
						    {;; Ian S. Eslick
;;						     [@(whitespace ws) @(alpha a) #\. @(whitespace ws) 
;;						      !(delete-chars #\. (- index 2) 1)] 
						          ;; Prop. , but -> Prop , but
						     [ #\. { [ ${ @(whitespace ws) } { #\, #\; } ]
						           ;; U.S. & m.p.h. -> U.S m.p.h
						           [ !(test-alpha) #\. @(whitespace ws) ] } ]
						     ;; Sentence end?: "word word. {W/w}"
						     [@(non-punc-or-white ws) #\. @(whitespace ws) !(potential-sentence)]
						     ;; Definitely a sentence!
						     [{ #\! #\? } @(whitespace ws) !(write-newline  (1- index))]
						    })
						   (progn (setq index old-index) nil))))
			      ;; NOTE: insert period handling and sentence detection
			      (meta-match
			       ${ {!(fix-numerics)
			           !(fix-will) 
				 !(fix-cant) 
				 !(fix-poss)
;;				 !(fix-abbreviations)
				 !(fix-periods)}
				 ;; gather ws
				 [@(whitespace ch) $[@(whitespace ch) !(delete-spaces (1- index) 1)]] 
				 ;; ignore, all good
				 [@(non-whitespace ch)] 
				})
			      index))))
		     (setq index new-index start new-index)
		     t))
		 ;; If we detect an end of sentence
		 (make-sentence ()  (setf (char result (- index 3)) #\Space)
				(setf (char result (- index 2)) #\.)
				(let ((ch (char result (- index 1))))
				  (setf (char result (- index 1)) #\Newline)
				  (write-ch))
				(when by-sentence
				  (setq fragment (subseq result (1- index) index))
				  (decf index 1)
				  (signal 'end-of-sentence))
				(setq sentence? nil)
				t))
	  ;; PROCESS THE STREAM INPUT
	  (declare (dynamic-extent #'process-token-inline))
	  (when fragment (copy-fragment))
	  (handler-case
	      (meta-match 
	       [${;; collapse whitespace to next token
	          [@(whitespace ws) $[@(whitespace ws)] !(write-ws) !(process-token-inline) !(extend-result-buffer)]
		  [@(punctuation ch) !(write-ws) !(write-ch) !(write-ws)] ;; expand syms [][?!()\";{}:,/
		  [@(non-punc-or-white ch) !(write-ch) !(check-sentence)] ;; accept everything else
		 }])
	    (end-of-file () (process-token-inline) (setq status :done))
	    (end-of-sentence () (setq status :done))
	    (condition (c) (pprint c) (break))))
	  (cond ((eq status :done)
		 (values t index (array-to-string) fragment))
		(t (values nil index (array-to-string)))) ;; characters read to error
	  ))))
;;end

#.(disable-meta-syntax)

(defun tokenize-string (string)
  "Returns a fresh, linguistically tokenized string"
  (with-input-from-string (s string)
    (tokenize-stream s)))

(defun tokenize-file (source target &key (if-exists :supersede))
  (write-to-file (mvretn 3 (tokenize-string (read-file-to-string source)))
		 target))
    

;; NOTE: Broken?  9/22/2004
(defun tokenize-file2 (source-file target-file &key (if-exists :supersede) &aux (total 0) (remainder ""))
  "Tokenizes a pure text file a sentence at a time"
  (declare (type string remainder)
	   (type fixnum total))
  (with-open-file (src source-file )
    (with-open-file (targ target-file :direction :output :if-exists if-exists)
      (loop while t do
	(multiple-value-bind (success count string rem)
	    (tokenize-stream src :by-sentence t :fragment remainder)
	  (format t "~A ~A ~A~%~A~%" success count string rem)
	  (incf total count)
	  (unless success
	    (return total))
	  (pprint string)
	  (setf remainder (copy-seq rem)))))))

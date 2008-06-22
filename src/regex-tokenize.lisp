;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          regex-tokenize
;;;; Purpose:       Simple regex based tokenizer for natural language pre-tagging
;;;;                
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  September 2004
;;;;

(in-package :langutils)

(defun tokenize-file (filename))

(defparameter known-abbreviations 
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
    "yd" "yrs"))

(defparameter compiled-abbrevs
  (loop for abbrev in known-abbreviations
    nconcing (list (pregex:create-scanner 
		    (concatenate 'string " +(" abbrev ")\\. +")))))

(defparameter multi-word-exp 
  '("a bit" "according to" "all of a sudden" "at large" "at last"
    "from time to time" "given that" "in addition to" "in addition"
    "in back of" "in between" "in brief" "inside out" "kung fu"
    "let 's" "no doubt" "no longer" "no matter" "none the less"
    "okey dokey" "old fashioned" "one another" "per cent" "per diem"
    "provided that" "providing that" "spot on" "time and again"
    "up to" "up to date" "upside down" "whether or not"))

(defparameter filters
  (list (make-regex-replace-filter "\\n" " " "Erase line boundaries")
	(make-regex-replace-filter "([][?!()\";{}])" " \\1 " "Punctuation that is always solo")
	;; apostrophes
	(make-regex-replace-filter "'" " '" "Apostrophes")
	(make-regex-replace-filter "([A-Za-z0-9]) '(s)" "\\1'\\2" "Apostrophes, recover 1")
	(make-regex-replace-filter "(n) '(t)" " \\1'\\2" "Apostrophes, recover 2")
	;; punctuation in numbers
	(make-regex-replace-filter "([0-9])([:,/])([0-9])" "\\1@\\2\\3" "Number punctuation 1")
	(make-regex-replace-filter "([^@])([:,/])" "\\1 \\2 " "Number punctuation 2")
	(make-regex-replace-filter "@" "" "Get rid of @'s")
	;; word-final periods
	(make-regex-replace-filter "\\b([A-Za-z])\\. " "\\1 " "ie: G. Gordon Liddy")
	(make-regex-replace-filter "(\\.[A-Za-z]+)\\. " "\\1 " "ie: U.S. i.e. m.p.h.")
	(make-regex-replace-filter "\\. *([,;])" " \\1" "ie: Prop., but_")
	(make-regex-replace-filter "\\. +([a-z])" " \\1" "ie: cm. in diameter")
	;; Allow abbreviations with periods
	#'(lambda (text &key print)
	    (when print (print "Finding abbreviations..."))
	    (loop for abbrev in compiled-abbrevs do
	      (setf text (pregex:regex-replace-all abbrev text "\\1@ ")))
	    text)
	;; add line breaks
	(make-regex-replace-filter "([.?!]) +" " \\1
" "Add a line break.")
	(make-regex-replace-filter "@" "." "")
	;; normalize spaces
	(make-regex-replace-filter " +" " " "")
	;; multiword expressions
	(make-regex-replace-filter 
	 (concatenate 'string "\\b(" (merge-or-regex-strings multi-word-exp) ")\\b")
	 "<\\1>" "Wrap multi-word expressions: <m w e>")
	(make-regex-replace-filter " (?=[^<]+>)" "_" "<m_w_e>")
	(make-regex-replace-filter "<>" "" "m_w_e")))


;;(defun-exported tokenize-string (string &key debug)
;;  (filter-text string filters :debug debug))

(defun tokenize-file (source target &key full)
  (with-open-file (src source )
    (with-open-file (targ target :direction :output :if-exists :supersede)
      (if full 
	  (write-string 
	   (tokenize-string 
	    (read-file-to-string src))
	   targ)
	(do-contentful-lines (line src)
	  (write-line (tokenize-string line) targ))))))

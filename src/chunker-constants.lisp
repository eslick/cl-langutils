;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: utils -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          chunker-constants
;;;; Purpose:       Constant definitions that need to have compile time values 
;;;;                prior to macro use in chunker.lisp
;;;;
;;;; Programmer:    Ian S. Eslick
;;;; Date Started:  November 2004
;;;;

(in-package :langutils)

(defconstant noun-pattern
  '(OR 
    (AND 
     (? OR PDT)
     (OR DT PRP PRP$ WDT WP WP$)
     (* OR VBG VBD VBN JJ JJR JJS \,);; CC)
;;     (OR 
     (+ OR NN NNS NNP NNPS CD))
;;      (AND (+ OR NN NNS NNP NNPS CD)
;;	   (* OR VBG VBD VBN JJ JJR JJS #\, CC NN NNS NNP NNPS CD)
;;	   (+ OR NN NNS NNP NNPS CD))))
    (AND 
     (? OR PDT)
;;	(* OR JJ JJR JJS #\, CC NN NNS NNP NNPS CD)
;;	(last-1 OR NN NNS NNP NNPS CD))))
     (* OR JJ JJR JJS \,) ;; CC)
     (+ OR NN NNS NNP NNPS CD))
    EX PRP WP WDT))

(defconstant verb-pattern
  '(and 
    (* or RB RBR RBS WRB)
    (? or MD)
    (* or RB RBR RBS WRB)
    (+ or VB VBD VBG VBP VBZ)
    (* or VB VBD VBG VBN VBP VBZ RB RBR RBS WRB)
    (? or RP)))
;;  (? and (* or RB) (or VB VBN) (? or RP))))

(defconstant adv-pattern
;;  '(or 
;;    (and 
;;     (* or RB RBR RBS)
;;     (+ or JJ JJR JJS)
;;     (* or RB RBR RBS JJ JJR JJS)
;;;     (+ or JJ JJR JJS))
    '(and 
      (* or RB RBR RBS)
      (+ or JJ JJR JJS)))

(defconstant p-pattern
  '(and (+ or IN)))
	
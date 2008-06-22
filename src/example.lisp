;;
;; Silly langutils example file
;;
;; Use: evaluate this buffer in emacs or (load "test.lisp") from the repl

(in-package :langutils)

(defparameter test-sentence1 "This is a test of the running system.  End of sentences.  ")
(defparameter test-sentence2 "Ain't isn't a word.")
(defparameter test-sentence3 "The acid rain in Spain falls mightily upon the plain.  Or does it?")

;; Note that the last period in any string is not separated from the end word.  This is
;; a known bug.

(eval-when (eval load)
  (format t "You can tokenize sentences like: ~%\"~A\"~%  as~%\"~A\"~%~%"
	  test-sentence1
	  (tag test-sentence1))
  (format t "Tokenization does a few odd things: ~A -> ~A~%~%" 
	  test-sentence2 (mvretn 3 (tokenize-string test-sentence2)))
  (format t "You can get the root forms of a word: ~A,~A -> ~A,~A~%~%"
	  "testing" "tested" (get-lemma "testing") (get-lemma "tested"))
  (format t "Let's find some phrases within a sentence like this:~%\"~A\"~%->~%\"~A\"~%"
	  test-sentence3 (tag test-sentence3))
  (format t "Phrases (nps and vps): ~A~%"
	  (chunk test-sentence3))
  (format t "Get surface forms of a root (morph-surface-forms \"run\"):~%~A~%"
	  (morph-surface-forms "run"))
  (format t "Oops!  All words are represented as token-id's (mapcar #'token-for-id ...) :~%~A~%~%"
	  (mapcar #'token-for-id (morph-surface-forms "run")))
  (pprint "If one of the above statements doesn't do something obvious, there's a bug!"))
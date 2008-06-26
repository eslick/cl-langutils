(in-package :langutils)

(defvar *default-lexicon-file* nil
  "Path to the lexicon file")
(defvar *default-stems-file* nil
  "Path to the word stems file")
(defvar *default-stopwords-file* nil
  "Path to a stopwords file")
(defvar *default-concise-stopwords-file* nil
  "Path to a *very* small list of words. Mainly pronouns and determiners")
(defvar *default-lexical-rule-file* nil
  "Path to the brill lexical rule file")
(defvar *default-contextual-rule-file* nil
  "Path to the brill contextual rule file")
(defvar *default-token-map-file* nil
  "Path to the token map file")
(defvar *auto-init* nil
  "Whether to call initialize-langutils when the .fasl is loaded")
(defvar *report-status* nil
  "Where to print langutils messages; default to none")



(defparameter *config-paths*
  '((:lexicon *default-lexicon-file*)
    (:stems *default-stems-file*)
    (:stopwords *default-stopwords-file*)
    (:concise-stopwords *default-concise-stopwords-file*)
    (:lexical-rules *default-lexical-rule-file*)
    (:contextual-rules *default-contextual-rule-file*)
    (:token-map *default-token-map-file*)))

(defmacro write-log (name msg &rest args)
  (declare (ignore name))
  `(format *report-status* ,msg ,@args))

(defun relative-pathname (path)
  (when path
    (asdf:system-relative-pathname :langutils path)))

(defun read-config ()
  (with-open-file (file (relative-pathname "config.sexp"))
    (mapc #'handle-config-entry (read file))))

(defun handle-config-entry (entry)
  (destructuring-bind (option pathtype &optional path) entry
    (awhen (assoc option *config-paths*)
      (setf (symbol-value (second it))
	    (if (eq pathtype :relative)
		(relative-pathname path)
		path)))
    (case option
      (:auto-init (setf *auto-init* pathtype))
      (:report-status (setf *report-status* pathtype)))))

(eval-when (:load-toplevel)
  ;; Read config
  (read-config))
    

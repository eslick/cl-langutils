;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;; Language utilities asd files



(defpackage #:langutils
  (:use #:cl #:meta #:stdutils) ;; #:s-serialization)
  (:export   ;; initialization
           init-langutils
	   clean-langutils
	   reset-langutils
	   ;; vector documents
	   vector-document
	   make-vector-document
	   document-text
	   document-tags
	   document-annotations
	   vector-document-words
	   get-token-id
	   get-tag
	   string-tag
	   length-of
	   string-tag-tokenized
	   print-vector-document
	   vector-document-string
	   write-vector-document
	   read-vector-document
	   read-vector-document-to-string
	   ;; phrases in documents
	   make-phrase
	   make-phrase-from-sentence
	   make-phrase-from-vdoc
	   phrase
	   phrase-start
	   phrase-end
	   phrase-type
	   phrase-document
	   phrase-length
	   phrase-equal
	   phrase-overlap
	   print-phrase
	   print-window
	   phrase->string
	   phrase->token-array
	   phrase-words
	   phrase-distance
	   phrase-lemmas
	   print-phrase-lemmas
	   find-phrase
	   find-phrase-intervals
	   change-word
	   remove-word
	   add-word
	   lemmatize-phrase
	   get-annotation
	   set-annotation
	   unset-annotation
	   ;; altered phrases
	   altered-phrase
	   make-alterable-phrase
           ;; tokens
	   id-for-token
	   ids-for-tokens
	   token-for-id
	   tokens-for-ids
	   save-tokens
	   suspicious-word?
	   suspicious-string?
	   string->token-array
	   tokenized-string->token-array
	   ;; lexicon
	   get-lexicon-default-pos
	   get-lexicon-entry
	   get-lexicon-case-forms
	   lexicon-entry
	   lexicon-entry-tag
	   lexicon-entry-tags
	   lexicon-entry-id
	   lexicon-entry-roots
	   lexicon-entry-surface-forms
	   add-lexicon-entry
	   add-lemma
	   ;; lemma
	   get-lemma
	   get-lemma-for-id
	   morph-surface-forms
	   morph-case-surface-forms
	   morph-surface-forms-text
	   ;; tokenizer
	   tokenize-stream
	   tokenize-string
	   tokenize-file
	   ;; text tagger
	   tag 
	   tag-tokenized 
	   ;; vector tagger
	   vector-tag 
	   vector-tag-tokenized 
	   initial-tag 
	   ;; chunker
	   chunk
	   chunk-tokenized
	   all-chunks
	   get-event-chunks
	   get-extended-event-chunks1
	   get-extended-event-chunks2
	   get-nx-chunks
	   get-vx-chunks
	   get-adverb-chunks
	   get-imperative-chunks
	   get-p-chunks
	   get-pp-chunks
	   head-verbs
	   head-verb
	   root-nouns
	   root-noun
	   ;; stopwords
	   stopword?
	   contains-is?
	   string-stopword?
	   string-contains-is?
	   concise-stopword?
	   string-concise-stopword?
	   ))

(in-package #:langutils)



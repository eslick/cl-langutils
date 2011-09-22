======================================================
                LANGUTILS LIBRARY
======================================================

This file contains a simple guide to the main functions and files
of the langutils library.  The code is reasonably documented with
doc strings and inline comments.  Write to the author if there
are any questions: eslick@media.mit.edu (Ian Eslick).  Also peruse
the LISP2005-langutils.pdf which is a more involved exposition of
the implementation and performance issues in the toolkit.

The library provides a heirarchy of major functions and auxiliary
functions related to the structured analysis and processing of 
open text.  The major functions working from raw text up are:

- String tokenization (string -> string)
- Part of speech tagging (string -> tokens -> vector-document)
- Phrase chunking (vector-document -> phrases)

We also provide auxiliary functions that operate on strings,
tokens or vector-documents.  The lisp functions implementing
the functionality can be found under the appropriately labled
section in the reference below.

Strings:
- Tokenize a string (separate punctuation from word tokens)
- POS tag a string or file returning a file, string or vector-document
- Identify suspicious strings that may become tokens

Tokens:
- String to token-id conversion routines
- Save/Load token maps
- Guess the POS tag for a token (lexicon-based, also includes the porter stemmer)
- Identify suspicious tokens
- Identify stopwords; words used primarily as syntactic combinators
- Lookup words in the lexicon
- Get possible parts of speech for known words
- Lemmatize a token (find the root lemma for a given surface form)
- Generate all surface forms of a root word

Vector-Documents:
- Generate phrases using the regex chunker

Miscellaneous:
- Concept representation
  Simple lemmatized noun or verb phrases can be treated as equal abstract notions;
  provides a CLOS class wrapper.



=================================================
           INTERFACE REFERENCE
=================================================

This documents the important functions of the langutils toolkit.
Documentation entries are of the form:


----------------------------------------------------------------------
function( args )
----------------------------------------------------------------------
Input:
arg1 - description
arg2 - description

Output:
description

Notes:
discussion of use cases, etc.

Functions are explicitely referenced by putting () around them; variables or
parameters have the form of *<name>*. 


:::::::::::::::::::::::::::::::::
:::: TOKENS and TOKENIZATION ::::
:::::::::::::::::::::::::::::::::

----------------------------------------------------------------------
tokenize-stream (stream &key (by-sentence nil) (fragment ""))
--------------------------------------------------------------------
Input:
stream - A standard lisp stream containing the characters to analyze,
         the stream can be of any length
by-sentence - Stop the tokenization process after each processed sentence
         meaning each validly parsed period, exclamation or question mark.
fragment - Provide a fragment from a prior call to tokenize stream at the
         beginning of the parse stream.

Output: (multiple-values)
1 - parsing success (t) or failure (nil)
2 - the current index into the stream, starts from 0 on every call
3 - a string containing the tokenized data parsed up to 'index'
4 - if parsing was a success, provides a fragment of any unparsed
    data (primarily in by-sentence mode)

Notes:
 This function is intended to be called all at once or in batches.
 For large strings or files it should be called in by-sentence mode
 in a loop that captures any fragments and passes them to the next call.
 The function operates by grabbing one character at a time from the stream
 and writing it into a temporary array.  When it reaches a punctuation
 character, it inserts a whitespace then backs up to the beginning of the current 
 token and checks whether the token should have included the punctuation
 and fixes up the temporary array.  Upon completion of the current parse (end 
 of stream or end of sentence) it 


----------------------------------------------------------------------
tokenize-string (string)
----------------------------------------------------------------------
Input:
string - a string of English natural language text

Output: (string)
Returns a string which is the result of calling (tokenize-stream) on
the stream version of the input string.


----------------------------------------------------------------------
tokenize-file (source target &key (if-exists :supersede))
----------------------------------------------------------------------
Input:
source - The source file name as a string or pathname
target - The target file name as a string or pathname


----------------------------------------------------------------------
id-for-token ( token )
----------------------------------------------------------------------
Input:
token - A string representing a primitive token

Output:
A fixnum providing a unique id for the provided string token.  

Notes:
Tokens are case sensitive so several 'The', 'the' and 'THE' all 
map to different tokens but should map to the same entry in the 
lexicon.  The root form of a lexicon word is the lower case 
representation.


----------------------------------------------------------------------
token-for-id ( id )
----------------------------------------------------------------------
Input:
id - A fixnum id

Output:
The original token string.


----------------------------------------------------------------------
tokens-for-id ( ids )
----------------------------------------------------------------------
Input:
ids - A list of fixnum ids

Output:
A list of string representations of the each id


----------------------------------------------------------------------
save-token-map ( filename )
----------------------------------------------------------------------
Input:
filename - A path or string to save token information to

Output:
t on success or nil otherwise

Notes:
This procedure will default to the filename in *default-token-map-file-int* 
which can be set via the asdf-config parameter 'token-map'


----------------------------------------------------------------------
load-token-map ( filename )
----------------------------------------------------------------------
Input:
filename - A path or string to save token information to

Output:
t on success or nil otherwise

Notes:
This procedure will default to the filename in *default-token-map-file-int* 
which can be set via the asdf-config parameter 'token-map'


----------------------------------------------------------------------
suspicious-word? ( word )
----------------------------------------------------------------------
Input:
A fixnum id for a word to test

Output:
A boolean representing whether this word has been labelled as fishy


----------------------------------------------------------------------
suspicious-string? ( string )
----------------------------------------------------------------------
Input:
A string

Output:
A boolean representing whether the word is fishy as determined by
parameters set in tokens.lisp (max numbers, total length and other 
characters in the token).  This is used inside id-for-token to 
keep the hash for suspicious-word? up to date.


::::::::::::::::::::::::::::::::::::::::::::::
:::: POS TAGGING AND OPERATIONS ON TOKENS ::::
::::::::::::::::::::::::::::::::::::::::::::::


----------------------------------------------------------------------
tag ( string )
----------------------------------------------------------------------
Input:
string - An input string to tag.  Input should be less than 100k 
         characters if possible.

Output:
A tagged string using the format <word>/<tag> where the tags are symbols
taken from the Penn Treebank 2 tagset.  Actual slash characters will 
show up as '///' meaning a slash word and slash token slash-separated!

Note:
This procedure calls the tokenizer to ensure that the input string is
properly tokenized in advance.

----------------------------------------------------------------------
tag-tokenized ( string )
----------------------------------------------------------------------
Input:
string - An input string to tag.  The string is assumed to be tokenized
  already and should be less than 100k bytes in size

Output:
A tagged string as in 'tag' above.

----------------------------------------------------------------------
vector-tag ( string )
----------------------------------------------------------------------
Input:
string - as in tag above

Output:
A CLOS object of type vector-document with the token array initialized
to fixnum representations of the word tokens and the tag array initialized
with symbols represented the selected tags.


----------------------------------------------------------------------
vector-tag-tokenized ( string &key end-tokens )
----------------------------------------------------------------------
Input:
string - as in tag-tokenized above
end-tokens - A list of string tokens to add to the end of the tokenization
   array.  Sometimes this is useful to ensure a closing period if you are
   doing tagging of structured NL data

Output:
A vector-document as in vector-tag

Note:
As in tag and tag-tokenized, this interface does not tokenize the input string.

----------------------------------------------------------------------
get-lexicon-entry ( word )
----------------------------------------------------------------------
Input:
word - Token id or token string

Output:
A lexicon-entry structure related to the lexical characteristics of the token

Notes:
The lexical-entry can be manipulated with a set of accessor
functions: lexicon-entry-tag, lexicon-entry-tags, lexical-entry-id,
lexical-entry-roots, lexical-entry-surface-forms, lexical-entry-case-forms,
get-lexicon-default-pos.  These functions are not all exported from the library
package, however.


----------------------------------------------------------------------
initial-tag ( token )
----------------------------------------------------------------------
Input:
token - A string token

Output:
A keyword symbol of the initially guessed tag (:PP :NN, etc)

Notes:
Provides an initial guess based purely on lexical features and lexicon
information of the provided string token.


----------------------------------------------------------------------
read-file-as-tagged-document ( file )
----------------------------------------------------------------------
Input:
A string filename or path object

Output:
A vector-document representing the tagged contents of file

Notes:
Loads the file into a string then calls vector-tag

----------------------------------------------------------------------
read-and-tag-file ( file )
----------------------------------------------------------------------
Input:
A path string or a path object

Output:
A string with tag annotations of the contents of file

Notes:
Uses tag on the string contents of file


----------------------------------------------------------------------
get-lemma ( word &key pos (noun t) porter )
----------------------------------------------------------------------
Input:
word - String of the word to find the lemma for
pos - The part of speech of the lemma to return (nil otherwise)
noun - Whether to stem nouns to the singular form
porter - Whether to use the porter algorithm if a word is unknown

Output:
A string representing the lemma of the word, if found


----------------------------------------------------------------------
get-lemma-for-id ( id &key pos (noun t) porter )
----------------------------------------------------------------------
Input:
id - The token id to find the lemma of
pos - As above
noun - ""
porter - ""

Output:
The lemma id

----------------------------------------------------------------------
lemmatize ((sequence list/array) &key strip-det pos (noun t) porter last-only )
----------------------------------------------------------------------
Input:
list/array - The input sequence of token ids as a list or an array
strip-det - Remove determiners from the sequence
pos - Part of speech of root of terms
noun - Whether to stem nouns
porter - Whether to use the porter stemmer
last-only - lemmatize the last token in the sequence only

Output:
Return the lemmatized list of tokens

Notes:
The main method for performing lemmatization.  Valid on lists and arrays of
fixnum values only.  Useful for getting the lemmatization of short phrases.


----------------------------------------------------------------------
morph-surface-forms ( root &optional pos-class )
----------------------------------------------------------------------
Input:
root - The root form to expand
pos-class - if provided (V - verb, N - noun, A - Adverb) the class of 
            surface forms to generate

Output:
A list of suface ids


----------------------------------------------------------------------
morph-surface-forms-text ( root &optional pos-class )
----------------------------------------------------------------------

String to string form of the above function


----------------------------------------------------------------------
stopword? ( id )
----------------------------------------------------------------------
Input:
id - Input token id

Output:
boolean 

----------------------------------------------------------------------
concise-stopword? ( id )
----------------------------------------------------------------------
Input:
id - Input token id

Output:
boolean

----------------------------------------------------------------------
contains-is? ( ids )
----------------------------------------------------------------------
Input:
ids - a list of fixnum token ids

Output:
boolean

Notes:
A sometimes useful utility.  Searches the list for the token for 'is'


----------------------------------------------------------------------
string-stopword?, string-concise-stopword?, string-contains-is? ( string )
----------------------------------------------------------------------
The three above functions but accepting string or list of string arguments


::::::::::::::::::
:::: CHUNKING ::::
::::::::::::::::::

----------------------------------------------------------------------
chunk ( text )
----------------------------------------------------------------------
Input:
Text - raw string text

Output:
A list of phrases referencing a document created from the text

Note:
Runs the tokenizer on the text prior to POS tagging

----------------------------------------------------------------------
chunk-tokenized ( text )
----------------------------------------------------------------------
Input:
text - raw string text

Output:
A list of phrases referencing a document created from the text

Note:
Does not run the tokenizer on text prior to POS tagging

----------------------------------------------------------------------
get-all-chunks ( doc )
----------------------------------------------------------------------
Input:
doc - a vector-document

Output:
A list of chunks of all the primitive types (verb, adverb, preps and nouns)

----------------------------------------------------------------------
get-nx-chunks ( doc )
get-vx-chunks ( doc )
get-ax-chunks ( doc )
get-pp-chunks ( doc )
get-event-chunks ( doc )
get-verb-arg-chunks ( doc )
----------------------------------------------------------------------
Input:
A vector-document

Output:
A list of phrases each of which references a segment of the
vector document array believed to be a phrase of type (phrase-type doc)

Notes:
- Events are concatenated verb-noun chunks
- verb-arg chunks look for verb-pp-noun chunk groups
These two functions could search over sequences of phrases, but
usually those are done alone and not on top of a more primitive
verb, noun, adverb decomposition.  Also note that common preposition 
idioms (by way of, in front of, etc) are not typically captured 
properly and would need to be special cased (ie would be VP-sNP-P-NP 
where sNP is a special type of NP instead of the usual VP-P-NP 
verb-arg formulation)


::::::::::::::::::
:::: CONCEPTS ::::
::::::::::::::::::

Concepts are a CLOS abstraction over token sequences that establishes
identity over lemmatized phrases.  This supports special applications
(ConceptNet, LifeNet) at the MIT Media Lab but might be more generally
useful.  

----------------------------------------------------------------------
concept
----------------------------------------------------------------------
A clos object with the following operations:
concept->words - Return a list of token strings
concept->string - Return a string representing the concept
concept->token-array - Return an array representing the concept
phrase->concept - Create a concept from a phrase
words->concept - Create a concept from a list of token ids
token-array->concept - ""
associate-concepts - Take a list of phrases, lists or token-arrays and find the concept
   the they represent.  Returns a list of pairs of the form (phrase concept)
conceptually-equal - equal under lemmatization and with phrases, arrays of tokens
concept-contains - subset relations


----------------------------------------------------------------------
lookup-canonical-concept-instance ( ta )
----------------------------------------------------------------------
Input:
A token array or list of tokens

Output:
A concept instance


=================================================
             EXAMPLE USES
=================================================

See the file example.lisp.  This shows basic use of the tagger, 
tokenizer, lemmatizer and chunker interfaces.  

More examples of use can be generated if enough mail is sent to 
the author to invoke a guilt-driven re-release of the library 
with improved documentation.


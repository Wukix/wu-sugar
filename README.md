wu-sugar is a small Common Lisp utility library to cut down on keystrokes. It might be similar in spirit to Alexandria, but is very minimalist.

The following functions are defined:
* **str**: like (concatenate 'string ...), but accepts non-string arguments
* **join**: concatenates strings, joining them by a separator character
* **split**: splits a string (e.g. a comma-separated string)
* **starts-with-p**: tests if a sequence begins with a particular subsequence
* **ends-with-p**: tests if a sequence ends with a particular subsequence
* **file-to-string**: reads a file's contents into a string
* **string-to-file**: writes a string's contents into a file
* **format-universal-time-iso**: a format function to print iso/rfc dates
* **universal-time-to-iso**: returns an iso/rfc formatted date string

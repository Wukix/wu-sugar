;;; -*- mode:lisp;coding:utf-8 -*-
;;;
;;; Copyright (C) 2013 Wukix, Inc. (http://wukix.com)
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;  * Redistributions of source code must retain the above copyright notice,
;;;    this list of conditions and the following disclaimer.
;;;
;;;  * Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

#+xcvb (module ())

(in-package :cl-user)

(defpackage :wu-sugar
  (:use :cl)
  (:export
   #:str
   #:join
   #:split
   #:starts-with-p
   #:ends-with-p
   #:file-to-string
   #:string-to-file
   #:format-universal-time-iso
   #:universal-time-to-iso
   ))

(in-package :wu-sugar)

;;; String/Sequence Functions

(defun str (&rest values)
  (with-output-to-string (s)
    (dolist (val values)
      (princ val s))))

(defun join (separator &rest strings)
  "Concatenates STRINGS, joining them by SEPARATOR."
  (when (characterp separator)
    (setf separator (string separator)))
  (if strings
      (reduce (lambda (a b) (& a separator b)) strings)
      ""))

(defun split (string &rest delimiter-chars)
  "Splits STRING by one or more delimiter characters, returning a list."
  (let ((current-pos 0)
	result)
    (loop
       (push (subseq string 
		     current-pos 
		     (setf current-pos (position-if (lambda (c) (member c delimiter-chars)) 
						    string 
						    :start current-pos)))
	     result)
       (unless current-pos (return))
       (incf current-pos))
    (nreverse result)))

(defun starts-with-p (seq subseq)
  (let ((subseq-len (length subseq))) 
    (if (<= subseq-len (length seq)) 
	(search subseq seq :end2 subseq-len)
	nil)))

(defun ends-with-p (seq subseq)
  (let ((seq-len (length seq))
	(subseq-len (length subseq))) 
    (if (<= (length subseq) (length seq)) 
	(search subseq seq :from-end t :start2 (- seq-len subseq-len))
	nil)))

;;; File Functions

(defun file-to-string (filespec)
  (with-open-file (f filespec)
    ;; In case the file contains multi-byte characters, we merely use file-length 
    ;; as an upper bound and then downsize to the actual character count
    (let* ((str (make-array (file-length f) :element-type 'character :adjustable t))
	   (character-length (read-sequence str f)))
      (adjust-array str character-length)
      str)))

(defun string-to-file (string filespec)
  (with-open-file (f filespec :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-sequence string f)))

;;; Light-weight date/time stuff 
;;; See LOCAL-TIME for more serious date handling
;;;
;;; Note: ISO 8601 date/time strings are lexicographically sortable provided
;;; that they have the same time zone part. Prefer UTC when appropriate, for
;;; portability and also because standard time and daylight time within the
;;; same zone are considered different offsets.

(defun format-universal-time-iso (stream universal-time colon atsign)
  "Format a universal time as an ISO 8601 / RFC 3339 date. Use with format, e.g. (format t \"~/wu-sugar:format-universal-time-iso/\" (get-universal-time)). If at-sign is supplied, time will be given in local time rather than UTC."
  (declare (ignore colon))
  (multiple-value-bind (second minute hour date month year day daylight-p zone-offset)
      (if atsign
          (decode-universal-time universal-time)
          (decode-universal-time universal-time 0))
    (declare (ignore day))
    (when daylight-p
      ;; This is not going to be reliable for all places in the world at all times, but w/o exact time zone data kept up to date, we assume daylight time differs from non-daylight time by one hour. See LOCAL-TIME for complete time zone handling.
      (decf zone-offset))
    (if atsign
	(multiple-value-bind (offset-hours offset-minutes) 
	    (truncate zone-offset)
	  (setf offset-minutes (round (* offset-minutes 60)))
	  (format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~A~2,'0D:~2,'0D" year month date hour minute second (if (plusp zone-offset) #\- #\+) offset-hours offset-minutes))
	(format stream "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month date hour minute second))))

(defun universal-time-to-iso (universal-time &optional local-time-zone-p)
  "Returns an ISO 8601 / RFC 3339 formatted date/time string either in UTC (the default), or the local time zone."
  (with-output-to-string (s)
    (format-universal-time-iso s universal-time nil local-time-zone-p)))

;;;; repl-dictionary.lisp 
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :repl-dictionary)

(defparameter *key-file* (asdf:system-relative-pathname :repl-dictionary ".repl-dictionary-keys")
  "Path to a JSON file containing an object with 'dictionary' and 'thesaurus' entries.")

(defun read-keys ()
  (with-input-from-file (ins *key-file*)
    (read-json ins)))

(defparameter *dictionary-key* nil)
(defparameter *thesaurus-key* nil)

(defun dictionary-key ()
  (cond (*dictionary-key* *dictionary-key*)
        (t
         (setf *dictionary-key* (getjso "dictionary" (read-keys)))
         *dictionary-key*)))

(defun thesaurus-key ()
  (cond (*thesaurus-key* *thesaurus-key*)
        (t
         (setf *thesaurus-key* (getjso "thesaurus" (read-keys)))
         *thesaurus-key*)))

(defparameter *definition-cache* nil)
(defparameter *alternatives-cache* nil)

(defun clear-cache ()
  (setf *definition-cache* nil)
  (setf *alternatives-cache* nil))

(defun dictionary-lookup (word)
  (read-json-from-string
   (dex:post
    (format nil
            "https://www.dictionaryapi.com/api/v3/references/collegiate/json/~a?key=~a"
            word
            (dictionary-key)))))

(defun thesaurus-lookup (word)
  (declare (ignorable word))
  (read-json-from-string
   (dex:post
    (format nil
            "https://www.dictionaryapi.com/api/v3/references/thesaurus/json/~a?key=~a"
            word
            (thesaurus-key)))))

(defun define (word)
  (cond ((assoc word *definition-cache* :test #'string=)
         (assoc-value *definition-cache* word :test #'string=))
        (t
         (let ((definition (dictionary-lookup word)))
           (push (cons word definition) *definition-cache*)
           definition))))

(defun get-synonyms (alternatives)
  (car alternatives))
;;  (car (getjso "syns" (car alternatives))))

(defun alternatives-to (word)
  (cond ((assoc word *alternatives-cache* :test #'string=)
         (get-synonyms (assoc-value *alternatives-cache* word :test #'string=)))
        (t
         (let ((alternatives (thesaurus-lookup word)))
           (push (cons word alternatives) *alternatives-cache*)
           (get-synonyms alternatives)))))

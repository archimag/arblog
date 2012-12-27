;;;; markdown.lisp

(defpackage #:arblog.markup.markdown
  (:use #:cl #:arblog.policy.markup)
  (:export #:arblog-markdown-markup))

(in-package #:arblog.markup.markdown)

(defclass arblog-markdown-markup () ())

(defmethod markup-render-content ((markup arblog-markdown-markup) content)
  (with-output-to-string (str)
    (cl-markdown:markdown 
     (reduce (lambda (x y) (concatenate 'string x y))
	     (with-input-from-string (in content)
	       (loop for line = (read-line in nil :eof)
		  while (not (eq line :eof))
		  collect line)))
     :format :html
     :stream str)))


;;;; rst.lisp

(defpackage #:arblog.markup.rst
  (:use #:cl #:iter #:arblog.policy.markup)
  (:import-from #:docutils.parser.rst #:&option #:&content #:&content-parser)
  (:export #:arblog-rst-markup))

(in-package #:arblog.markup.rst)


(defvar *interpreted-roles*
  (alexandria:copy-hash-table docutils.parser.rst::*interpreted-roles*))

(defvar *directives*
  (alexandria:copy-hash-table docutils.parser.rst::*directives*))

(defmacro with-arblog-markup (&body body)
  `(let ((docutils.parser.rst::*interpreted-roles* *interpreted-roles*)
         (docutils.parser.rst::*directives* *directives*))
     ,@body))

;;;; hypespec-ref

(defclass hyperspec-ref (docutils.nodes:raw)
  ((spec :initarg :spec :reader hyperspec-ref-spec)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node hyperspec-ref))  
  (docutils:part-append
   (docutils.writer.html::start-tag node
                                    "a"
                                    (list :href (clhs-lookup:spec-lookup (hyperspec-ref-spec node))
                                          :class "common-lisp-entity"))
   (hyperspec-ref-spec node)
   "</a>"))

(with-arblog-markup
  (docutils.parser.rst:def-role hs (spec)
    (make-instance 'hyperspec-ref
                   :spec spec)))

;;;; cliki-ref

(defclass cliki-ref (docutils.nodes:raw)
  ((spec :initarg :spec :reader cliki-ref-spec)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node cliki-ref))  
  (docutils:part-append
   (docutils.writer.html::start-tag node
                                    "a"
                                    (list :href (format nil "http://www.cliki.net/~A" (cliki-ref-spec node))))
   (cliki-ref-spec node)
   "</a>"))

(with-arblog-markup
  (docutils.parser.rst:def-role cliki (spec)
    (make-instance 'cliki-ref
                   :spec spec)))

;;;; code-block

(defclass code-block (docutils.nodes:raw)
  ((lang :initarg :lang :initform nil :reader code-block-lang)
   (code :initarg :code :initform nil :reader code-block-code)))

(defparameter *span-classes*
  '("symbol" "special" "keyword" "comment" "string" "character"))

(defun update-code-markup (markup)
  (labels
      ((bad-span-p (node)
         (and (string-equal (xtree:local-name node) "span")
              (not (member (xtree:attribute-value node "class") *span-classes*
                           :test #'string-equal))))
       ;;---------------------------------------
       (comment-p (node)
         (and (string-equal (xtree:local-name node) "span")
              (string-equal (xtree:attribute-value node "class") "comment")))
       ;;---------------------------------------
       (br-p (node)
         (string-equal (xtree:local-name node) "br"))
       ;;---------------------------------------
       (flatten-spans (node)
         (iter (for el in (xtree:all-childs node))
               (flatten-spans el))
         ;;---------------------------------------
         (when (comment-p node)
           (setf (xtree:text-content node)
                 (xtree:text-content node))
           (xtree:insert-child-after (xtree:make-element "br") node))
         ;;---------------------------------------
         (when (bad-span-p node)
           (iter (for el in (xtree:all-childs node))
                 (xtree:insert-child-before (xtree:detach el) node))
           (xtree:remove-child node))))
    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    (html:with-parse-html (doc (format nil "<div>~A</div>" markup))
      (let ((div (xtree:first-child (xtree:first-child (xtree:root doc)))))
        (flatten-spans div)
        (xtree:with-object (fragment (xtree:make-document-fragment doc))
          ;;---------------------------------------
          (let* ((pre (xtree:make-child-element fragment "div"))
                 (ol (xtree:make-child-element pre "ol")))
            (setf (xtree:attribute-value pre "class")
                  "prettyprint linenums")
            (setf (xtree:attribute-value ol "class")
                  "linenums")
            ;;---------------------------------------
            (iter (for line in (split-sequence:split-sequence-if #'br-p (xtree:all-childs div)))
                  (for i from 0)
                  (let ((li (xtree:make-child-element ol "li")))
                    (setf (xtree:attribute-value li "class")
                          (format nil "L~s" i))
                    ;;---------------------------------------
                    (iter (for el in line)
                          (xtree:append-child li (xtree:detach el))))))
          ;;---------------------------------------
          (html:serialize-html fragment :to-string))))))
        

(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node code-block))
  (let* ((code (code-block-code node))
         (lang (code-block-lang node))
         (coloring-type (colorize:find-coloring-type (find-symbol (string-upcase lang) :keyword))))
    (cond
      ;;---------------------------------------
      (coloring-type
       (docutils:part-append
        (update-code-markup (colorize::html-colorization coloring-type code))))
      ;;---------------------------------------
      (t
        (docutils:part-append
         "<div class=\"prettyprint linenums\">"
         "<ol class=\"linenums\">")
        ;;---------------------------------------
        (iter (for line in (split-sequence:split-sequence #\Newline code))
              (for i from 0)
              (docutils:part-append
               (format nil "<li class=\"L~A\">~A</li>" i line)))
        ;;---------------------------------------
        (docutils:part-append "</ol>" "</div>")))))

(with-arblog-markup 
  (docutils.parser.rst:def-directive code-block (parent lang &content content)
    (let ((node (docutils:make-node 'docutils.nodes:paragraph)))
      (docutils:add-child node
                          (make-instance 'code-block
                                         :lang lang
                                         :code (docutils::join-strings content #\Newline)))
      (docutils:add-child parent node))))

;;; arblog-rst-markup

(defclass arblog-rst-markup () ())

(defmethod markup-render-content ((markup arblog-rst-markup) content)
  (with-arblog-markup 
    (let ((doc (docutils:read-rst content))
          (writer (make-instance 'docutils.writer.html:html-writer)))
      (docutils:visit-node writer doc)
      (with-output-to-string (out)
        (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                              docutils.writer.html:docinfo
                              docutils.writer.html:body))
              (docutils:write-part writer part out))
        (format out "</div>")))))

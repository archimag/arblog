;;;; static.lisp

(in-package #:arblog.static)

(defun parse-native-namestring (thing)
  #+sbcl (sb-ext:parse-native-namestring thing)
  #-sbcl (parse-namestring thing))

(restas:define-route theme-static-file ("/:theme/*path")
  (let* ((theme-path (gethash theme arblog::*theme-static-dir-map*))
         (relative-path (parse-native-namestring (format nil "~{~A~^/~}" path)))
         (file (merge-pathnames relative-path theme-path)))
    (when (find :up (pathname-directory relative-path))
      (restas:abort-route-handler hunchentoot:+http-bad-request+))
    (unless (fad:file-exists-p file)
      (restas:abort-route-handler hunchentoot:+http-not-found+))
    file))


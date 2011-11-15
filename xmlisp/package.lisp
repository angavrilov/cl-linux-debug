(defpackage :XML
  (:use :common-lisp)
  (:export "XML-SERIALIZER" "PARSE-FILE-NAME" "*XMLISP-PACKAGES*"))

(defvar xml::*XMLisp-Packages* (list (find-package :xml)))

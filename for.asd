#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem for
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An extensible iteration macro library."
  :homepage "https://Shinmera.github.io/for/"
  :bug-tracker "https://github.com/Shinmera/for/issues"
  :source-control (:git "https://github.com/Shinmera/for.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "binding")
               (:file "clause")
               (:file "for")
               (:file "iterator")
               (:file "standard")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :lambda-fiddle
               :form-fiddle))

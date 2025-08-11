(asdf:defsystem for
  :version "1.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An extensible iteration macro library."
  :homepage "https://shinmera.com/docs/for/"
  :bug-tracker "https://shinmera.com/project/for/issues"
  :source-control (:git "https://shinmera.com/project/for.git")
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

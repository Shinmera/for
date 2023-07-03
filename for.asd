(asdf:defsystem for
  :version "1.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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

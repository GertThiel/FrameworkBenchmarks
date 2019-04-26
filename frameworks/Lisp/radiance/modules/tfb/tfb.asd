(in-package #:cl-user)
(asdf:defsystem #:tfb
  :version "1.0.0"

  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :components ((:file "tfb"))
  :depends-on ((:interface :database)
               :cl-markup
               :jonathan
               :postmodern
               :r-clip))

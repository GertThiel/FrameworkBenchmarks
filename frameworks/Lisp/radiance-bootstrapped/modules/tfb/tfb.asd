(in-package #:cl-user)
(asdf:defsystem #:tfb
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :components ((:file "tfb"))
  :depends-on (:jonathan
               :postmodern
               :r-clip))

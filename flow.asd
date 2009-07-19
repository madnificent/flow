(defpackage :flow.sysdef
  (:use :common-lisp :asdf))

(in-package :flow.sysdef)

(defsystem :flow
  :name "Flow"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "BSD"
  :description "Flow allows you to define a flow in your software.  A flow is a fixed order of stages which a certain request should go through.  (eg: Controller -> View in Web-MVC)"
  :depends-on (:fiveam)
  :components ((:file "flow")
	       (:file "example" :depends-on ("flow"))
	       (:file "flow-tests" :depends-on ("flow" "example"))))
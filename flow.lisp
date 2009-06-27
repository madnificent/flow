(defpackage :flow
  (:use :common-lisp)
  (:export :flow
	   :forward
	   :forward*
	   :recurse
	   :defflow))

(defclass flow ()
  ((order :initarg :order
	  :reader order
	  :documentation "Contains the order in which the flow should be passed")
   (flow-functions :initform (make-hash-table :test #'equal)
		   :reader functions
		   :documentation "Contains the functions this flow can execute"))
  (:documentation "Defines flows by their respective names and methods"))
(defclass flow-state ()
  ((state :initarg :state
	  :reader state
	  :documentation "Contains the current state of the flow")
   (flow :initarg flow
	 :reader flow
	 :documentation "Contians the flow this state belongs to")))
(defclass next-flow-state (flow-state)
  ((args :initarg :args
	 :accessor args))
  (:documentation "Object representing the next state in the flow.  This contains the variables that will be used to call the method with"))

(defgeneric flow-method (flow state)
  (:documentation "Fetches the function to be executed by the system"))
(defgeneric (setf flow-method) (function flow state)
  (:documentation "Sets the function to be executed for the given flow and state"))

(defun forward (&rest args)
  "Sets the forwarding to happen with the given arguments")

(defun forward* (new-function &optional args)
  "Forwards the flow to <new-function> in the next stage of the flow (at the end of the function calling it.")

(defun recurse (new-name &optional args)
  "Recurses within the current stage, to a function with a possibly different name.  If wanted, different arguments are given.")

(defmacro defflow (flow stage name args &body body)
  "Defines a function for <flow>'s <stage> with name <name>, that receives <args> arguments and executes <body>.
The functions <forward,forward* and recurse can be called from within the body")
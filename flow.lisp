(defpackage :flow
  (:use :common-lisp)
  (:export :flow
	   :forward
	   :forward*
	   :recurse
	   :defflow
	   :do-flow))

(defpackage :flow-user (:use :common-lisp :flow))

(in-package :flow)

(defclass flow ()
  ((order :initarg :order
	  :reader order
	  :documentation "Contains the order in which the flow should be passed")
   (flow-functions :initform (make-hash-table :test #'equal)
		   :reader functions
		   :documentation "Contains the functions this flow can execute"))
  (:documentation "Defines flows by their respective names and methods"))
(defclass flow-state ()
  ((name :initarg :name
	 :accessor name
	 :documentation "Contains the name of the function")
   (stage :initarg :stage
	  :accessor stage
	  :documentation "Contains the current stage of the flow")
   (flow :initarg :flow
	 :reader flow
	 :documentation "Contians the flow this stage belongs to")))
(defclass next-flow-state (flow-state)
  ((args :initarg :args
	 :accessor args))
  (:documentation "Object representing the next state in the flow.  This contains the variables that will be used to call the method with"))

(defgeneric flow-method (flow stage name)
  (:documentation "Fetches the function to be executed by the system"))
(defgeneric (setf flow-method) (function flow stage name)
  (:documentation "Sets the function to be executed for the given flow and stage"))

(defmethod flow-method ((flow flow) stage name)
  (gethash (list stage name) (functions flow)))
(defmethod (setf flow-method) (function (flow flow) stage name)
  (setf (gethash (list stage name) (functions flow)) function))

(defun forward (&rest args)
  "Sets the forwarding to happen with the given arguments"
  (declare (special *next-flow-state*))
  (setf (args *next-flow-state*) args))

(defun forward* (name &key (args nil args-p))
  "Forwards the flow to <new-function> in the next stage of the flow (at the end of the function calling it."
  (declare (special *next-flow-state*))
  (if args-p
      (setf (args *next-flow-state*) args))
  (setf (name *next-flow-state*) name))

(defun recurse (new-name &key (args nil args-p))
  "Recurses within the current stage, to a function with a possibly different name.  If wanted, different arguments are given."
  (declare (special *flow-state* *next-flow-state* *original-args*))
  (if args-p
      (setf (args *next-flow-state*) args)
      (setf (args *next-flow-state*) *original-args*))
  (setf (name *next-flow-state*) new-name)
  (setf (stage *next-flow-state*) (stage *flow-state*)))

(defmacro defflow (flow stage name args &body body)
  "Defines a function for <flow>'s <stage> with name <name>, that receives <args> arguments and executes <body>.
The functions <forward,forward* and recurse can be called from within the body"
  (let ((g-flow (gensym))
	(g-stage (gensym))
	(g-name (gensym)))
    `(let ((,g-flow ,flow)
	   (,g-stage ,stage)
	   (,g-name ,name))
       (setf (flow-method ,g-flow ,g-stage ,g-name)
	     (lambda ,args
	       (let* ((*flow-state* (make-instance 'flow-state :stage ,g-stage :flow ,g-flow :name ,g-name))
		      (*next-flow-state* (extend-flow-state *flow-state*))
		      (*original-args* (list ,@args)))
		 (declare (special *flow-state* *next-flow-state* *original-args*))
		 (let ((result (multiple-value-list (progn ,@body))))
		   (apply 'run *next-flow-state* result))))))))

(defgeneric extend-flow-state (flow-state)
  (:documentation "Extends the given flow state to the next flow state"))
(defmethod extend-flow-state ((state flow-state))
  (when (get-next-stage (flow state) (stage state))
    (make-instance 'next-flow-state :name (name state) :stage (stage state) :flow (flow state))))

(defgeneric get-next-stage (flow stage)
  (:documentation "Returns the next stage for the given flow and stage"))
(defmethod get-next-stage ((flow flow) stage)
  (loop for stage-in-list on (order flow) by #'cdr
     do (when (eql stage (car stage-in-list))
	  (return-from get-next-stage (cadr stage-in-list)))))

(defgeneric get-next-method (state)
  (:documentation "Returns the next method for the given state"))
(defmethod get-next-method (state)
  (let ((next-stage (stage state)))
    (loop while (setf next-stage (get-next-stage (flow state) next-stage))
	 as flow-method = (flow-method (flow state) next-stage (name state))
	 when flow-method do (return-from get-next-method flow-method))))

(defgeneric run (state &rest args)
  (:documentation "Runs the given flow state object"))
(defmethod run ((state flow-state) &rest args)
  (let ((flow-method (flow-method (flow state) (stage state) (name state))))
    (if flow-method
	(apply flow-method args) 
	(apply #'run (extend-flow-state state) args))))
(defmethod run ((state next-flow-state) &rest args)
  (let ((args (if (slot-boundp state 'args) (args state) args)))
    (if (get-next-method state)
	(apply (get-next-method state) args)
	(apply #'values args))))
(defmethod run (nothing &rest args)
  (declare (ignore nothing))
  (apply #'values args))

(defmethod do-flow (flow name &rest args)
  (apply #'run
	 (make-instance 'flow-state :name name :stage (first (order flow)) :flow flow)
	 args))
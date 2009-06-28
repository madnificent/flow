(defpackage :mvcd
  (:use :flow
	:common-lisp)
  (:export :defview
	   :defcontroller
	   :defdirector))

(in-package :mvcd)

(defparameter *mvcp* (make-instance 'flow :order '(controller director view)))

(defmacro defview (name args &body body)
  "Defines a new view"
  `(defflow *mvcp* 'view ',name ,(or args (list 'arg))
     ,@body))

(defmacro defcontroller (name args &body body)
  "Defines a new conttroller"
  `(defflow *mvcp* 'controller ',name ,(or args (list 'arg))
     ,@body))

(defmacro defdirector (name args &body body)
  "Defines a new director.  This is the strange part of this software.  This part will decide what is the next view to be called."
  `(defflow *mvcp* 'director ',name ,(or args (list 'arg))
     ,@body))


;; (defpackage :mvcd.example
;;   (:use :mvcd
;; 	:common-lisp
;;      :other-stuff))

;; (defcontroller add-user ()
;;   (with-vars (name email password)
;;     (save (make-instance 'user :name name :email email :password password))))

;; (defdirector add-user (saved-p errors user)
;;   (if saved-p
;;       (forward* 'show-user user)
;;       (forward* 'add-user user)))


;; (defview add-user (saved-p errors user)
;;   (standard-page :title "Create user"
;; 		 :content (list (h1 "Create a new user")
;; 				(p "Please enter the following data in order to create the new user")
;; 				(input-form user))))

;; (defview show-user (user)
;;   (standard-page :title "User page"
;; 		 :contetn (list (h1 "User page")
;; 				(htmlify user))))


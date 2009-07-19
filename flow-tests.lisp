(defpackage :flow-tests
  (:use :common-lisp
	:flow-example
	:flow
	:fiveam)
  (:export :run-all-tests))

(in-package :flow-tests)

(defun run-all-tests ()
    (run 'all))

(def-suite all :description "All tests about flow")

(in-suite all)

(defcontroller all-steps (value)
  (+ 2 value))
(defdirector all-steps (value)
  (* 2 value))
(defview all-steps (value)
  (expt value 3))

(test all-steps
  (is (= (do-flow *mvcp* 'all-steps 1)
	 216)
      "((1 + 2) * 2)^3 = 216")
  (is (= (do-flow *mvcp* 'all-steps 0)
	 64)
      "(2 * 2)^3 = 64"))

(defcontroller one-step-a (value)
  (* 2 value))
(defcontroller one-step-b (value)
  (+ 2 value))
(defcontroller one-step-c (value)
  (expt 2 value))

(test one-step
  (is (= (do-flow *mvcp* 'one-step-a 3)
	 6)
      "2 * 3 = 6")
  (is (= (do-flow *mvcp* 'one-step-b 3)
	 5)
      "2 + 3 = 5")
  (is (= (do-flow *mvcp* 'one-step-c 3)
	 8)
      "2 ^ 3 = 8"))

(defcontroller two-step-a (value)
  (* 2 value))
(defdirector two-step-a (value)
  (+ 2 value))
(defcontroller two-step-b (value)
  (* 2 value))
(defview two-step-b (value)
  (+ 2 value))
(defdirector two-step-c (value)
  (* 2 value))
(defview two-step-c (value)
  (+ 2 value))

(test two-step
  (is (= (do-flow *mvcp* 'two-step-a 2)
	 6)
      "two-step-a -> 6")
  (is (= (do-flow *mvcp* 'two-step-b 2)
	 6)
      "two-step-a -> 6")
  (is (= (do-flow *mvcp* 'two-step-c 2)
	 6)
      "two-step-a -> 6"))

(defcontroller forward-a (value)
  (forward (+ 2 value))
  (* 100 value))

(defview forward-a (value)
  (+ 2 value))

(defcontroller forward-b (value)
  (forward 100)
  (+ 1000 value))

(test forward
  (is (= (do-flow *mvcp* 'forward-a 2)
	 6)
      "2 + 2 + 2 = 6")
  (is (= (do-flow *mvcp* 'forward-b 100000)
	 100)
      "should return 100 in each case"))

(defcontroller forward*-a-a (value)
  (forward* 'forward*-a-b :args (list (+ 1000 value)))
  value)
(defview forward*-a-a (value)
  ;; this one shouldn't be called
  value)
(defview forward*-a-b (value)
  (+ 100 value))

(defcontroller forward*-b-a (value)
  (forward* 'forward*-b-b)
  (* 2 value))
(defview forward*-b-a (value)
  ;; this one shouldn't be called
  (+ 1000 value))
(defview forward*-b-b (value)
  (+ 100 value))

(test forward*
  (is (= (do-flow *mvcp* 'forward*-a-a 0)
	 1100)
      "Without args: 1000 + 100 = 1100")
  (is (= (do-flow *mvcp* 'forward*-a-a 11)
	 1111)
      "1000 + 100 + 11 = 1111")
  (is (= (do-flow *mvcp* 'forward*-b-a 0)
	 100)
      "Without args: 0 * 2 + 100 = 100")
  (is (= (do-flow *mvcp* 'forward*-b-a 10)
	 120)
      "2 * 20 + 100 = 120"))

(defcontroller recurse-a-a (value)
  (recurse 'recurse-a-b)
  (+ 1 value))
(defcontroller recurse-a-b (value)
  (+ 10 value))

(test recurse
  (is (= (do-flow *mvcp* 'recurse-a-a 0)
	 10)
      "0 + 10 = 10")
  (is (= (do-flow *mvcp* 'recurse-a-a 10)
	 20)
      "10 + 10 = 20"))
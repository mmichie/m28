;; Arithmetic Tests
(run-test "Addition" (lambda () 
  (and (assert (= (+ 1 2) 3))
       (assert (= (+ -1 1) 0))
       (assert (= (+ 1 2 3 4 5) 15)))))

(run-test "Subtraction" (lambda ()
  (and (assert (= (- 5 3) 2))
       (assert (= (- 10 3 2) 5))
       (assert (= (- 5) -5)))))

(run-test "Multiplication" (lambda ()
  (and (assert (= (* 2 3) 6))
       (assert (= (* 2 3 4) 24))
       (assert (= (* -2 3) -6)))))

(run-test "Division" (lambda ()
  (and (assert (= (/ 6 2) 3))
       (assert (= (/ 24 2 3) 4))
       (assert (= (/ 5 2) 2.5)))))

(run-test "Modulo" (lambda ()
  (and (assert (= (% 10 3) 1))
       (assert (= (mod 10 3) 1)))))

(run-test "Max and Min" (lambda ()
  (and (assert (= (max 1 2 3) 3))
       (assert (= (min 1 2 3) 1)))))

(run-test "Round and Truncate" (lambda ()
  (and (assert (= (round 3.7) 4))
       (assert (= (truncate 3.7) 3)))))

(run-test "Remainder" (lambda ()
  (assert (= (rem 10 3) 1))))

(run-test "Number Type Checks" (lambda ()
  (and (assert (evenp 2))
       (assert (oddp 3))
       (assert (zerop 0))
       (assert (plusp 1))
       (assert (minusp -1)))))

(run-test "Numeric Comparisons" (lambda ()
  (and (assert (< 1 2 3))
       (assert (> 3 2 1))
       (assert (<= 1 1 2))
       (assert (>= 2 2 1))
       (assert (= 2 2 2)))))

(run-test "Trigonometric Functions" (lambda ()
  (and (assert (= (sin 0) 0))
       (assert (= (cos 0) 1))
       (assert (= (tan 0) 0)))))

(run-test "Exponential and Logarithmic Functions" (lambda ()
  (and (assert (= (exp 0) 1))
       (assert (= (log 1) 0))
       (assert (= (sqrt 4) 2))
       (assert (= (pow 2 3) 8)))))

(run-test "Absolute Value and Rounding" (lambda ()
  (and (assert (= (abs -5) 5))
       (assert (= (floor 3.7) 3))
       (assert (= (ceiling 3.2) 4)))))

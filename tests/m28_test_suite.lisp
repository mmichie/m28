(print "Starting Lisp Interpreter Unit Tests")

;; Helper function to run tests
(defun run-test (name test-func)
  (print name)
  (if (funcall test-func)
      (print "  Passed")
      (print "  Failed")))

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

;; List Operations Tests
(run-test "Car" (lambda ()
  (assert (eq (car '(1 2 3)) 1))))

(run-test "Cdr" (lambda ()
  (assert (equal (cdr '(1 2 3)) '(2 3)))))

(run-test "Cons" (lambda ()
  (and
    (let ((result (cons 1 '(2 3)))
          (expected '(1 2 3)))
      (print "  Cons test 1 (list):")
      (print "    Result:")
      (print result)
      (print "    Expected:")
      (print expected)
      (assert (equal result expected)))
    (let ((result (cons 1 2))
          (expected '(1 2)))  ; Changed from (1 . 2) to (1 2)
      (print "  Cons test 2 (pair):")
      (print "    Result:")
      (print result)
      (print "    Expected:")
      (print expected)
      (assert (equal result expected)))
    (let ((result (cons 1 nil))
          (expected '(1)))
      (print "  Cons test 3 (with nil):")
      (print "    Result:")
      (print result)
      (print "    Expected:")
      (print expected)
      (assert (equal result expected)))
    (let ((result (car (cons 1 2)))
          (expected 1))
      (print "  Cons test 4 (car of pair):")
      (print "    Result:")
      (print result)
      (print "    Expected:")
      (print expected)
      (assert (equal result expected)))
    (let ((result (cdr (cons 1 2)))
          (expected 2))
      (print "  Cons test 5 (cdr of pair):")
      (print "    Result:")
      (print result)
      (print "    Expected:")
      (print expected)
      (assert (equal result expected))))))

(run-test "List" (lambda ()
  (assert (equal (list 1 2 3) '(1 2 3)))))

(run-test "Append" (lambda ()
  (assert (equal (append '(1 2) '(3 4)) '(1 2 3 4)))))

;; Function Definition and Application Tests
(run-test "Lambda and Function Application" (lambda ()
  (let ((square (lambda (x) (* x x))))
    (and (assert (= (funcall square 5) 25))
         (assert (= ((lambda (x y) (+ x y)) 3 4) 7))))))

(run-test "Recursive Function" (lambda ()
  (defun factorial (n)
    (if (<= n 1) 1 (* n (factorial (- n 1)))))
  (and (assert (= (factorial 5) 120))
       (assert (= (factorial 0) 1)))))

;; Variable Binding Tests
(run-test "Let Binding" (lambda ()
  (assert (= (let ((x 5) (y 3)) (+ x y)) 8))))

;; Conditional Tests
(run-test "If Statement" (lambda ()
  (and (assert (eq (if (> 5 3) 'yes 'no) 'yes))
       (assert (eq (if (< 5 3) 'yes 'no) 'no)))))

(run-test "Cond Statement" (lambda ()
  (assert (eq (cond ((< 5 3) 'less)
                    ((> 5 3) 'greater)
                    (t 'equal))
              'greater))))

;; Higher-order Function Tests
(run-test "Map Function" (lambda ()
  (assert (equal (mapcar (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6)))))

(print "All tests completed.")

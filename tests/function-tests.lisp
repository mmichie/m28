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

(run-test "Map Function" (lambda ()
  (assert (equal (mapcar (lambda (x) (* x 2)) '(1 2 3)) '(2 4 6)))))

(run-test "Other Misc Operations" (lambda ()
  (and (assert (equal (assoc 'b '((a 1) (b 2))) '(b 2)))
       (assert (equal (apply '+ '(1 2 3)) 6))
       (assert (equal (filter (lambda (x) (> x 2)) '(1 2 3 4)) '(3 4)))
       (assert (match '(1 ? 3) '(1 2 3)))
       (assert (exists? (lambda (x) (> x 2)) '(1 2 3 4))))))

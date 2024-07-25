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

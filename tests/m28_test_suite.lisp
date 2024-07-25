(princ "Starting Lisp Interpreter Unit Tests")
(terpri)

;; Helper function to run tests
(defun run-test (name test-func)
  (princ name)
  (princ ": ")
  (if (funcall test-func)
      (princ "Passed")
      (princ "Failed"))
  (terpri))

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

;; Arithmetic Operations
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

;; Comparison Operations
(run-test "Numeric Comparisons" (lambda ()
  (and (assert (< 1 2 3))
       (assert (> 3 2 1))
       (assert (<= 1 1 2))
       (assert (>= 2 2 1))
       (assert (= 2 2 2)))))

(run-test "Equality" (lambda ()
  (and (assert (equal? '(1 2) '(1 2)))
       (assert (eq 'symbol 'symbol)))))

;; Hash Table Operations
(run-test "Hash Table Operations" (lambda ()
  (let ((ht (make-hash-table)))
    (and (assert (null (gethash 'key ht)))
         (assert (eq (sethash 'key 'value ht) 'value))
         (assert (eq (gethash 'key ht) 'value))
         (assert (eq (remhash 'key ht) 't))
         (assert (null (gethash 'key ht)))))))

;; List Operations
(run-test "Additional List Operations" (lambda ()
  (and (assert (eq (caddr '(1 2 3)) 3))
       (assert (eq (cadr '(1 2 3)) 2))
       (assert (consp '(1 2)))
       (assert (eq (first '(1 2 3)) 1))
       (assert (eq (last '(1 2 3)) 3))
       (assert (= (length '(1 2 3)) 3))
       (assert (eq (nth 1 '(1 2 3)) 2))
       (assert (equal (nthcdr 1 '(1 2 3)) '(2 3)))
       (assert (null nil))
       (assert (null? nil))
       (assert (eq (second '(1 2 3)) 2))
       (assert (eq (third '(1 2 3)) 3)))))

;; Math Operations
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

;; Miscellaneous Operations
(run-test "Type Checks" (lambda ()
  (and (assert (number? 5))
       (assert (string? "hello"))
       (assert (symbol? 'symbol))
       (assert (not nil))
       (assert (pair? '(1 . 2)))
       (assert (integer? 5))
       (assert (numberp 5))
       (assert (symbolp 'symbol))
       (assert (atom 5)))))

(run-test "Other Misc Operations" (lambda ()
  (and (assert (equal (assoc 'b '((a 1) (b 2))) '(b 2)))
       (assert (equal (apply '+ '(1 2 3)) 6))
       (assert (equal (filter (lambda (x) (> x 2)) '(1 2 3 4)) '(3 4)))
       (assert (match '(1 ? 3) '(1 2 3)))
       (assert (exists? (lambda (x) (> x 2)) '(1 2 3 4))))))

;; Sequence Operations
(run-test "Sequence Operations" (lambda ()
  (and (assert (equal (subseq '(1 2 3 4 5) 1 3) '(2 3)))
       (assert (equal (reverse '(1 2 3)) '(3 2 1)))
       (assert (equal (remove 2 '(1 2 3 2 4)) '(1 3 4)))
       (assert (equal (remove-if (lambda (x) (> x 2)) '(1 2 3 4)) '(1 2)))
       (assert (equal (remove-if-not (lambda (x) (> x 2)) '(1 2 3 4)) '(3 4)))
       (assert (= (reduce '+ '(1 2 3 4)) 10))
       (assert (= (count 2 '(1 2 2 3 2)) 3))
       (assert (= (count-if (lambda (x) (> x 2)) '(1 2 3 4 5)) 3))
       (assert (equal (remove-duplicates '(1 2 2 3 3 3)) '(1 2 3)))
       (assert (equal (substitute 'z 'a '(a b a c)) '(z b z c)))
       (assert (equal (sort '(3 1 4 1 5 9) '<) '(1 1 3 4 5 9)))
       (assert (equal (stable-sort '(3 1 4 1 5 9) '<) '(1 1 3 4 5 9)))
       (assert (= (position 'c '(a b c d)) 2)))))

;; String Operations
(run-test "String Operations" (lambda ()
  (and (assert (string= (string-append "Hello" " " "World") "Hello World"))
       (assert (string= (number->string 123) "123"))
       (assert (= (string->number "123") 123))
       (assert (string= (concatenate 'string "Hello" " " "World") "Hello World"))
       (assert (string= (string-upcase "hello") "HELLO"))
       (assert (string= (print-value "test") "test"))
       (assert (= (to-number "123") 123)))))

;; Type Predicates
;(run-test "Type Predicates" (lambda ()
;  (and (assert (listp '(1 2 3)))
;       (assert (arrayp #(1 2 3)))
;       (assert (vectorp #(1 2 3)))
;       (assert (characterp #\a)))))

;; List Operations Tests
(run-test "Car" (lambda ()
  (assert (eq (car '(1 2 3)) 1))))

(run-test "Cdr" (lambda ()
  (assert (equal (cdr '(1 2 3)) '(2 3)))))

(run-test "Cons" (lambda ()
  (and (assert (equal (cons 1 '(2 3)) '(1 2 3)))
       (assert (equal (cons 1 2) '(1 . 2)))
       (assert (equal (cons 1 nil) '(1)))
       (assert (= (car (cons 1 2)) 1))
       (assert (= (cdr (cons 1 2)) 2)))))

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

(princ "All tests completed.")
(terpri)

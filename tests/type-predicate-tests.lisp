;; Type Checks and Predicates
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

(run-test "Equality" (lambda ()
  (and (assert (equal? '(1 2) '(1 2)))
       (assert (eq 'symbol 'symbol)))))

;; Uncomment and modify as needed when these predicates are implemented
;(run-test "Type Predicates" (lambda ()
;  (and (assert (listp '(1 2 3)))
;       (assert (arrayp #(1 2 3)))
;       (assert (vectorp #(1 2 3)))
;       (assert (characterp #\a)))))

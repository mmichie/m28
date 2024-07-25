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

;; Load test files
(load "arithmetic-tests.lisp")
(load "list-sequence-tests.lisp")
(load "hash-table-tests.lisp")
(load "string-tests.lisp")
(load "type-predicate-tests.lisp")
(load "function-tests.lisp")
(load "language-construct-tests.lisp")

(princ "All tests completed.")
(terpri)

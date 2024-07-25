;; String Operations
(run-test "String Operations" (lambda ()
  (and (assert (string= (string-append "Hello" " " "World") "Hello World"))
       (assert (string= (number->string 123) "123"))
       (assert (= (string->number "123") 123))
       (assert (string= (concatenate 'string "Hello" " " "World") "Hello World"))
       (assert (string= (string-upcase "hello") "HELLO"))
       (assert (string= (print-value "test") "test"))
       (assert (= (to-number "123") 123)))))

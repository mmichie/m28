(defun Y (f)
  (funcall 
   (lambda (x) (funcall x x))
   (lambda (x)
     (funcall f (lambda (y) (funcall (funcall x x) y))))))

(defvar factorial
  (Y (lambda (f)
       (lambda (n)
         (if (= n 0)
             1
             (* n (funcall f (- n 1))))))))

(print "Factorial of 5 using Y combinator:")
(print (funcall factorial 5))

(defvar fibonacci
  (Y (lambda (f)
       (lambda (n)
         (if (< n 2)
             n
             (+ (funcall f (- n 1)) (funcall f (- n 2))))))))

(print "10th Fibonacci number using Y combinator:")
(print (funcall fibonacci 10))

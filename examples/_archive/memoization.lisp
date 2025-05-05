(defun make-memoized (f)
  (let ((cache '()))
    (lambda (x)
      (let ((cached-result (assoc x cache)))
        (if cached-result
            (cdr cached-result)
            (let ((result (funcall f x)))
              (setq cache (cons (cons x result) cache))
              result))))))

(defun slow-fib (n)
  (if (< n 2)
      n
      (+ (slow-fib (- n 1)) (slow-fib (- n 2)))))

(defvar memoized-fib (make-memoized slow-fib))

(print "Computing slow-fib(30):")
(print (slow-fib 30))

(print "Computing memoized-fib(30):")
(print (funcall memoized-fib 30))

(print "Computing memoized-fib(30) again (should be faster):")
(print (funcall memoized-fib 30))

(defmacro when (condition . body)
  `(if ,condition
       (progn ,@body)
       nil))

(defmacro unless (condition . body)
  `(if (not ,condition)
       (progn ,@body)
       nil))

(defvar x 10)

(print "Using 'when' macro:")
(when (> x 5)
  (print "x is greater than 5")
  (print "The value of x is:")
  (print x))

(print "Using 'unless' macro:")
(unless (< x 5)
  (print "x is not less than 5")
  (print "The value of x is:")
  (print x))

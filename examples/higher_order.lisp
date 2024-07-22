(defun apply-twice (f x)
  (funcall f (funcall f x)))

(defun add-one (x) 
  (+ x 1))

(print "Applying add-one twice to 5:")
(print (apply-twice #'add-one 5))

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun square (x) 
  (* x x))

(defvar square-and-add-one
  (compose #'add-one #'square))

(print "Square and add one to 5:")
(print (funcall square-and-add-one 5))

(defun make-object (class-name)
  (lambda (message &rest args)
    (case message
      ((get-class) class-name)
      (t (error "Unknown message")))))

(defun make-point (x y)
  (let ((self (make-object 'point)))
    (lambda (message &rest args)
      (case message
        ((get-x) x)
        ((get-y) y)
        ((set-x!) (setq x (car args)))
        ((set-y!) (setq y (car args)))
        (t (apply self (cons message args)))))))

(defvar p (make-point 3 4))
(print "Point class:") (print (funcall p 'get-class))
(print "X coordinate:") (print (funcall p 'get-x))
(print "Y coordinate:") (print (funcall p 'get-y))
(funcall p 'set-x! 5)
(print "New X coordinate:") (print (funcall p 'get-x))

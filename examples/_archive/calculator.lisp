(defun calculator (op x y)
  (cond
    ((equal op "+") (+ x y))
    ((equal op "-") (- x y))
    ((equal op "*") (* x y))
    ((equal op "/") (/ x y))
    (t (print "Unknown operation"))))

(print "Calculator:")
(print (concatenate 'string "5 + 3 = " (number->string (calculator "+" 5 3))))
(print (concatenate 'string "10 - 4 = " (number->string (calculator "-" 10 4))))
(print (concatenate 'string "6 * 7 = " (number->string (calculator "*" 6 7))))
(print (concatenate 'string "15 / 3 = " (number->string (calculator "/" 15 3))))

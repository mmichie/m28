(defun match (pattern expression)
  (cond
    ((null? pattern) (null? expression))
    ((equal? (car pattern) '_)
     (if (null? expression) nil (match (cdr pattern) (cdr expression))))
    ((equal? (car pattern) '?)
     (and (not (null? expression))
          (match (cdr pattern) (cdr expression))))
    ((and (consp (car pattern)) 
          (equal? (car (car pattern)) '?)
          (consp (cdr (car pattern))))
     (and (not (null? expression))
          (if (equal? (car (cdr (car pattern))) 'number?)
              (numberp (car expression))
              (funcall (car (cdr (car pattern))) (car expression)))
          (match (cdr pattern) (cdr expression))))
    ((and (not (null? expression))
          (equal? (car pattern) (car expression)))
     (match (cdr pattern) (cdr expression)))
    (t nil)))

(defun number? (x) (integerp x))

(print "Pattern: (1 ? 3), Expression: (1 2 3)")
(print (match '(1 ? 3) '(1 2 3)))

(print "Pattern: (1 (? number?) 3), Expression: (1 2 3)")
(print (match '(1 (? number?) 3) '(1 2 3)))

(print "Pattern: (1 _ 3), Expression: (1 2 3)")
(print (match '(1 _ 3) '(1 2 3)))

(print "Pattern: (1 _ 3), Expression: (1 2 4)")
(print (match '(1 _ 3) '(1 2 4)))

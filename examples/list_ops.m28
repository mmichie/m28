(defvar my-list '(1 2 3 4 5))
(print "Original list:")
(print my-list)

(defun sum (lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(print "Sum of list elements:")
(print (sum my-list))

(defun map (func lst)
  (if (null? lst)
      '()
      (cons (func (car lst)) (map func (cdr lst)))))

(defun square (x) 
  (* x x))

(print "Squared list:")
(print (map square my-list))

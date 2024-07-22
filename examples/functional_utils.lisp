; Implement map function
(defun map (func lst)
  (if (null? lst)
      '()
      (cons (funcall func (car lst))
            (map func (cdr lst)))))

; Implement filter function
(defun filter (pred lst)
  (cond
    ((null? lst) '())
    ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
    (t (filter pred (cdr lst)))))

; Implement reduce function
(defun reduce (func init lst)
  (if (null? lst)
      init
      (reduce func 
              (funcall func init (car lst))
              (cdr lst))))

; Test map
(print "Mapping (lambda (x) (* x 2)) over (1 2 3 4 5):")
(print (map (lambda (x) (* x 2)) '(1 2 3 4 5)))

; Test filter
(print "Filtering even numbers from (1 2 3 4 5 6 7 8 9 10):")
(print (filter (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5 6 7 8 9 10)))

; Test reduce
(print "Sum of (1 2 3 4 5) using reduce:")
(print (reduce #'+ 0 '(1 2 3 4 5)))

; Combine all three
(print "Sum of squares of even numbers in (1 2 3 4 5 6 7 8 9 10):")
(print (reduce #'+
               0
               (map (lambda (x) (* x x))
                    (filter (lambda (x) (= (mod x 2) 0))
                            '(1 2 3 4 5 6 7 8 9 10)))))

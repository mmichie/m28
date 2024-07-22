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

(defun filter (pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))

(defun is-even (x)
  (= (mod x 2) 0))

(print "Filtered even numbers:")
(print (filter is-even my-list))

(defun reduce (func lst init)
  (if (null? lst)
      init
      (reduce func (cdr lst) (func (car lst) init))))

(defun add (x y)
  (+ x y))

(print "Sum of list elements using reduce:")
(print (reduce add my-list 0))

(defun reverse (lst)
  (defun reverse-iter (lst acc)
    (if (null? lst)
        acc
        (reverse-iter (cdr lst) (cons (car lst) acc))))
  (reverse-iter lst '()))

(print "Reversed list:")
(print (reverse my-list))

(defun find (pred lst)
  (if (null? lst)
      nil
      (if (pred (car lst))
          (car lst)
          (find pred (cdr lst)))))

(print "First even number in list:")
(print (find is-even my-list))


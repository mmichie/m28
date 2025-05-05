; Define helper functions for list access
(defun cadr (lst) (car (cdr lst)))
(defun caddr (lst) (car (cdr (cdr lst))))

; Define helper functions for list operations
(defun map (f lst)
  (if (null? lst)
      '()
      (cons (funcall f (car lst)) (map f (cdr lst)))))

(defun filter (pred lst)
  (cond ((null? lst) '())
        ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (t (filter pred (cdr lst)))))

(defun remove-if (pred lst)
  (filter (lambda (x) (not (funcall pred x))) lst))

(defun find-if (pred lst)
  (cond ((null? lst) nil)
        ((funcall pred (car lst)) (car lst))
        (t (find-if pred (cdr lst)))))

(defun position-if (pred lst)
  (let ((helper (lambda (l n)
                  (cond ((null? l) nil)
                        ((funcall pred (car l)) n)
                        (t (funcall helper (cdr l) (+ n 1)))))))
    (funcall helper lst 0)))

(defun any (pred lst)
  (cond ((null? lst) nil)
        ((funcall pred (car lst)) t)
        (t (any pred (cdr lst)))))

(defun every (pred lst)
  (cond ((null? lst) t)
        ((not (funcall pred (car lst))) nil)
        (t (every pred (cdr lst)))))

; Define a list of student records: (name age grade)
(defvar students '(
  (Alice 18 95)
  (Bob 17 82)
  (Charlie 19 78)
  (David 18 90)
  (Eve 17 88)
))

; Function to get the grade from a student record
(defun get-grade (student) (caddr student))

; Function to get the age from a student record
(defun get-age (student) (cadr student))

; Function to get the name from a student record
(defun get-name (student) (car student))

; 1. Use map to extract all grades
(defvar grades (map get-grade students))
(print "All grades:") (print grades)

; 2. Use filter to find students with grades above 85
(defvar high-performers (filter (lambda (s) (> (get-grade s) 85)) students))
(print "High performers:") (print high-performers)

; 3. Use remove to exclude students under 18
(defvar adult-students (remove-if (lambda (s) (< (get-age s) 18)) students))
(print "Adult students:") (print adult-students)

; 4. Use find to get the first student with a grade below 80
(defvar struggling-student (find-if (lambda (s) (< (get-grade s) 80)) students))
(print "First struggling student:") (print struggling-student)

; 5. Use position to find the index of the first student named "David"
(defvar david-position (position-if (lambda (s) (equal (get-name s) 'David)) students))
(print "Position of David:") (print david-position)

; 6. Use any to check if there's any student with a perfect score (100)
(defvar perfect-score-exists (any (lambda (s) (= (get-grade s) 100)) students))
(print "Perfect score exists:") (print perfect-score-exists)

; 7. Use every to check if all students are 17 or older
(defvar all-seventeen-or-older (every (lambda (s) (>= (get-age s) 17)) students))
(print "All students 17 or older:") (print all-seventeen-or-older)

; 8. Combine operations: Find names of adult students with grades 90 or above
(defvar top-adult-students 
  (map get-name
       (filter (lambda (s) 
                 (and (>= (get-age s) 18)
                      (>= (get-grade s) 90)))
        students)))
(print "Top adult students:") (print top-adult-students)

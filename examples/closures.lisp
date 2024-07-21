(defun make-counter ()
  (let ((count 0))
    (lambda ()
      (setq count (+ count 1))
      count)))

(defvar counter1 (make-counter))
(defvar counter2 (make-counter))

(print "Counter 1:")
(print (counter1))
(print (counter1))
(print (counter1))

(print "Counter 2:")
(print (counter2))
(print (counter2))

(print "Counter 1 again:")
(print (counter1))

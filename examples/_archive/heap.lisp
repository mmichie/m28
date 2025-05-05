(defun make-heap () '())

(defun parent (index) (/ (- index 1) 2))
(defun left-child (index) (+ 1 (* 2 index)))
(defun right-child (index) (+ 2 (* 2 index)))

(defun swap (heap i j)
  (let ((temp (nth i heap)))
    (setf (nth i heap) (nth j heap))
    (setf (nth j heap) temp))
  heap)

(defun heapify-up (heap index)
  (if (<= index 0)
      heap
      (let ((parent-index (parent index)))
        (if (< (nth index heap) (nth parent-index heap))
            (let ((new-heap (swap heap index parent-index)))
              (heapify-up new-heap parent-index))
            heap))))

(defun heapify-down (heap index)
  (let* ((left (left-child index))
         (right (right-child index))
         (heap-size (length heap))
         (smallest (if (and (< left heap-size)
                            (< (nth left heap) (nth index heap)))
                       left
                       index)))
    (setq smallest (if (and (< right heap-size)
                            (< (nth right heap) (nth smallest heap)))
                       right
                       smallest))
    (if (not (= smallest index))
        (let ((new-heap (swap heap index smallest)))
          (heapify-down new-heap smallest))
        heap)))

(defun heap-insert (heap value)
  (heapify-up (append heap (list value)) (- (length heap) 1)))

(defun heap-remove-min (heap)
  (if (null heap)
      (list nil heap)
      (let* ((last-index (- (length heap) 1))
             (min-value (car heap))
             (new-heap (swap heap 0 last-index)))
        (list min-value
              (heapify-down (butlast new-heap) 0)))))

(defun heap-sort (lst)
  (let ((heap (make-heap)))
    (dolist (elem lst)
      (setq heap (heap-insert heap elem)))
    (let ((sorted '()))
      (while (not (null heap))
        (let ((result (heap-remove-min heap)))
          (setq sorted (append sorted (list (car result))))
          (setq heap (car (cdr result)))))
      sorted)))

; Testing
(defvar my-heap (make-heap))
(print "Empty heap:")
(print my-heap)

(setq my-heap (heap-insert my-heap 10))
(setq my-heap (heap-insert my-heap 4))
(setq my-heap (heap-insert my-heap 15))
(setq my-heap (heap-insert my-heap 7))
(setq my-heap (heap-insert my-heap 3))
(setq my-heap (heap-insert my-heap 8))

(print "Heap after inserts:")
(print my-heap)

(let ((result (heap-remove-min my-heap)))
  (print "Minimum value removed:")
  (print (car result))
  (print "Heap after removing min:")
  (print (car (cdr result))))

(print "Sorted list from heap:")
(print (heap-sort '(10 4 15 7 3 8)))

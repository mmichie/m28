;; Load test utilities
(load "test-utils.lisp")

;; Example Program Tests

;; Bubble Sort Test
(run-test "Bubble Sort" (lambda ()
  (load "../examples/bubblesort.lisp")
  (let ((unsorted-list '(5 2 9 1 7 6 3))
        (expected-sorted-list '(1 2 3 5 6 7 9)))
    (assert (equal (bubble-sort (copy-list unsorted-list)) expected-sorted-list)))))

;; Heap Operations Test
(run-test "Heap Operations" (lambda ()
  (load "../examples/heap.lisp")
  (let ((heap (make-heap)))
    (setq heap (heap-insert heap 10))
    (setq heap (heap-insert heap 4))
    (setq heap (heap-insert heap 15))
    (setq heap (heap-insert heap 7))
    (setq heap (heap-insert heap 3))
    (setq heap (heap-insert heap 8))
    
    (and
     ;; Test heap structure (allowing for valid variations)
     (assert (or (equal heap '(3 4 8 10 7 15))
                 (equal heap '(3 4 15 10 7 8))))
     
     ;; Test heap-remove-min
     (let ((result (heap-remove-min heap)))
       (and (assert (= (car result) 3))
            (assert (or (equal (cadr result) '(4 7 8 10 15))
                        (equal (cadr result) '(4 7 15 10 8))))))
     
     ;; Test heap-sort
     (assert (equal (heap-sort '(10 4 15 7 3 8)) '(3 4 7 8 10 15)))))))

;; Macro System Test
(run-test "Macro System" (lambda ()
  (load "../examples/macro_system.lisp")
  (let ((x 10)
        (when-result nil)
        (unless-result nil))
    
    ;; Test 'when' macro
    (when (> x 5)
      (setq when-result t))
    (assert when-result)
    
    ;; Test 'unless' macro
    (unless (< x 5)
      (setq unless-result t))
    (assert unless-result)
    
    ;; Test 'when' macro with false condition
    (setq when-result nil)
    (when (< x 5)
      (setq when-result t))
    (assert (not when-result))
    
    ;; Test 'unless' macro with false condition
    (setq unless-result nil)
    (unless (> x 5)
      (setq unless-result t))
    (assert (not unless-result)))))

(princ "All example program tests completed.")
(terpri)

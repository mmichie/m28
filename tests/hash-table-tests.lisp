;; Hash Table Operations
(run-test "Hash Table Operations" (lambda ()
  (let ((ht (make-hash-table)))
    (and (assert (null (gethash 'key ht)))
         (assert (eq (sethash 'key 'value ht) 'value))
         (assert (eq (gethash 'key ht) 'value))
         (assert (eq (remhash 'key ht) 't))
         (assert (null (gethash 'key ht)))))))

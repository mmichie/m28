; Define a tree structure: (value left-subtree right-subtree)
(defun make-tree (value left right) 
  (list value left right))

(defun tree-value (tree) (car tree))
(defun tree-left (tree) (cadr tree))
(defun tree-right (tree) (caddr tree))

; Check if a value is in the tree
(defun tree-contains? (tree value)
  (if (null? tree)
      nil
      (or (= (tree-value tree) value)
          (tree-contains? (tree-left tree) value)
          (tree-contains? (tree-right tree) value))))

; Sum all values in the tree
(defun tree-sum (tree)
  (if (null? tree)
      0
      (+ (tree-value tree)
         (tree-sum (tree-left tree))
         (tree-sum (tree-right tree)))))

; Create a sample tree
(defvar sample-tree 
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 4 '() '()))
             (make-tree 7
                        (make-tree 6 '() '())
                        (make-tree 8 '() '()))))

(print "Sample tree:")
(print sample-tree)

(print "Tree contains 4?")
(print (tree-contains? sample-tree 4))

(print "Tree contains 9?")
(print (tree-contains? sample-tree 9))

(print "Sum of all values in the tree:")
(print (tree-sum sample-tree))

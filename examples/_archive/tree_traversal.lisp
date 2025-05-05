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

; Pre-order traversal
(defun tree-preorder (tree)
  (if (null? tree)
      '()
      (append (list (tree-value tree))
              (tree-preorder (tree-left tree))
              (tree-preorder (tree-right tree)))))

; In-order traversal
(defun tree-inorder (tree)
  (if (null? tree)
      '()
      (append (tree-inorder (tree-left tree))
              (list (tree-value tree))
              (tree-inorder (tree-right tree)))))

; Post-order traversal
(defun tree-postorder (tree)
  (if (null? tree)
      '()
      (append (tree-postorder (tree-left tree))
              (tree-postorder (tree-right tree))
              (list (tree-value tree)))))

; Calculate the depth of the tree
(defun tree-depth (tree)
  (if (null? tree)
      0
      (+ 1 (max (tree-depth (tree-left tree))
                (tree-depth (tree-right tree))))))

; Find the minimum value in the tree
(defun tree-min (tree)
  (if (null? tree)
      nil
      (min (tree-value tree)
           (or (tree-min (tree-left tree)) (tree-value tree))
           (or (tree-min (tree-right tree)) (tree-value tree)))))

; Find the maximum value in the tree
(defun tree-max (tree)
  (if (null? tree)
      nil
      (max (tree-value tree)
           (or (tree-max (tree-left tree)) (tree-value tree))
           (or (tree-max (tree-right tree)) (tree-value tree)))))

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

(print "Pre-order traversal:")
(print (tree-preorder sample-tree))

(print "In-order traversal:")
(print (tree-inorder sample-tree))

(print "Post-order traversal:")
(print (tree-postorder sample-tree))

(print "Depth of the tree:")
(print (tree-depth sample-tree))

(print "Minimum value in the tree:")
(print (tree-min sample-tree))

(print "Maximum value in the tree:")
(print (tree-max sample-tree))


; Custom char-at function to replace the missing char function
(defun char-at (str index)
  (subseq str index (+ index 1)))

; Define a trie node structure: (value children is-end-of-word)
(defun make-trie-node (value) 
  (list value (make-hash-table :test 'equal) nil))

(defun trie-node-value (node) (nth 0 node))
(defun trie-node-children (node) (nth 1 node))
(defun trie-node-is-end-of-word? (node) (nth 2 node))
(defun set-trie-node-is-end-of-word (node value) (setf (nth 2 node) value))

; Create a new trie
(defun make-trie () 
  (make-trie-node nil))

; Insert a word into the trie
(defun trie-insert (trie word)
  (let ((node trie))
    (do ((i 0 (1+ i))
         (len (length word)))
        ((>= i len))
      (let* ((char (char-at word i))
             (children (trie-node-children node))
             (child (gethash char children)))
        (if (not child)
            (setf child (make-trie-node char)
                  (gethash char children) child))
        (setf node child)))
    (set-trie-node-is-end-of-word node t)))

; Search for a word in the trie
(defun trie-search (trie word)
  (let ((node trie))
    (do ((i 0 (1+ i))
         (len (length word)))
        ((or (>= i len) (not node)) (and node (trie-node-is-end-of-word? node)))
      (let ((char (char-at word i)))
        (setf node (gethash char (trie-node-children node)))))))

; Check if a prefix exists in the trie
(defun trie-starts-with (trie prefix)
  (let ((node trie))
    (do ((i 0 (1+ i))
         (len (length prefix)))
        ((or (>= i len) (not node)) (not (not node)))
      (let ((char (char-at prefix i)))
        (setf node (gethash char (trie-node-children node)))))))

; Helper function to collect all words in the trie from a given node
(defun trie-collect (node prefix)
  (let ((words '()))
    (when (trie-node-is-end-of-word? node)
      (setf words (cons prefix words)))
    (let ((children (trie-node-children node)))
      (maphash (lambda (key child)
                 (setf words (append words (trie-collect child (concatenate 'string prefix key)))))
               children))
    words))

; Find all words with a given prefix
(defun trie-words-with-prefix (trie prefix)
  (let ((node trie))
    (do ((i 0 (1+ i))
         (len (length prefix)))
        ((or (>= i len) (not node)) (and node (trie-collect node prefix)))
      (let ((char (char-at prefix i)))
        (setf node (gethash char (trie-node-children node)))))))

; Helper function for testing
(defun print-trie-words (words)
  (dolist (word words)
    (print word)))

; Testing the trie implementation
(defvar my-trie (make-trie))
(trie-insert my-trie "hello")
(trie-insert my-trie "hell")
(trie-insert my-trie "heaven")
(trie-insert my-trie "heavy")

(print "Words in the trie with prefix 'he':")
(print-trie-words (trie-words-with-prefix my-trie "he"))

(print "Words in the trie with prefix 'hell':")
(print-trie-words (trie-words-with-prefix my-trie "hell"))

(print "Words in the trie with prefix 'hea':")
(print-trie-words (trie-words-with-prefix my-trie "hea"))

(print "Search for word 'hello':")
(print (trie-search my-trie "hello"))

(print "Search for word 'heavens':")
(print (trie-search my-trie "heavens"))

(print "Does any word start with 'he'?")
(print (trie-starts-with my-trie "he"))

(print "Does any word start with 'hell'?")
(print (trie-starts-with my-trie "hell"))

(print "Does any word start with 'heav'?")
(print (trie-starts-with my-trie "heav"))

(print "Does any word start with 'hero'?")
(print (trie-starts-with my-trie "hero"))

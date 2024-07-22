(defun greet (name &optional (greeting "Hello") &key (shout nil))
  (let ((message (concatenate 'string greeting ", " name "!")))
    (if shout
        (string-upcase message)
        message)))

(print (greet "Alice"))                    ; => "Hello, Alice!"
(print (greet "Bob" "Hi"))                 ; => "Hi, Bob!"
(print (greet "Charlie" "Hey" :shout t))   ; => "HEY, CHARLIE!"

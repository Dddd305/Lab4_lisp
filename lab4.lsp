;;; Частина 1: Функціональне сортування з :key та :test
(defun bubble-pass-hof (lst key test)
  (if (or (null lst) (null (cdr lst)))
      (values lst nil)
      (let* ((x (car lst))
             (y (cadr lst))
             (kx (funcall key x))
             (ky (funcall key y)))
        (if (funcall test kx ky)
            
            (multiple-value-bind (rest swapped)
                (bubble-pass-hof (cons x (cddr lst)) key test)
              (declare (ignore swapped))
              (values (cons y rest) t))

            (multiple-value-bind (rest swapped)
                (bubble-pass-hof (cdr lst) key test)
              (values (cons x rest) swapped))))))

(defun functional-sort-hof (lst &key (key #'identity) (test #'>))
  (multiple-value-bind (new-list swapped) 
      (bubble-pass-hof lst key test)
    (if swapped
        (functional-sort-hof new-list :key key :test test)
        new-list)))

;;; Частина 2: Замикання remove-each-rnth-reducer
(defun remove-each-rnth-reducer (n &key key)
  (let ((counter 0) 
        (key-fn (or key (constantly t)))) 
    (lambda (elem acc)
      (if (funcall key-fn elem) 
          (progn
            (incf counter) 
            (if (= counter n)
                (progn 
                  (setf counter 0) 
                  acc)
                (cons elem acc)))
          (cons elem acc)))))

;;; Тести та утиліти
(defun check-function (name result expected)
  (format t "~:[FAILED~;passed~] ~a~%" (equal result expected) name))

(defun test-lab4 ()
  (format t "~%--- Part 1: Sort with HOF ---~%")
  
  (check-function "test-sort-numbers" 
                  (functional-sort-hof '(3 1 4 1 5 9 2)) 
                  '(1 1 2 3 4 5 9))
  
  (check-function "test-key-length" 
                  (functional-sort-hof '((1 2 3) (1) (1 2)) :key #'length) 
                  '((1) (1 2) (1 2 3)))
  
  (check-function "test-test-less" 
                  (functional-sort-hof '(1 2 3) :test #'<) 
                  '(3 2 1))

  (format t "~%--- Part 2: remove-each-rnth-reducer ---~%")
  
  (check-function "test-reducer-simple"
                  (reduce (remove-each-rnth-reducer 2)
                          '(1 2 3 4 5)
                          :from-end t
                          :initial-value nil)
                  '(1 3 5))

  (check-function "test-reducer-key"
                  (reduce (remove-each-rnth-reducer 2 :key #'evenp)
                          '(1 2 2 2 3 4 4 4 5)
                          :from-end t
                          :initial-value nil)
                  '(1 2 3 4 4 5)))

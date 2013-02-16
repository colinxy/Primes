;;;; Prime number search by Sieve of Eratosthenes.
;;;;


(defun primes (n)
  (let ((sieve (make-array (1+ n) 
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))

  (loop for i from 2 upto (isqrt n)
    when (zerop (aref sieve i)) do
      (loop for j from (* i i) upto n by i do
        (setf (aref sieve j) 1)))

  (loop for i from 2 upto n 
    when (zerop (aref sieve i)) collect i)))


;;;; End. 

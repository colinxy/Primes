;;;; Compute primes using recursive trial by division.
;;;;


(defun primes (n)
  (labels ((find-primes (primes candidates)
    (if (null candidates)
        primes
        (let ((i (first candidates)))
          (find-primes (cons i primes)
                       (delete-if #'(lambda (x) (zerop (rem x i)))
                                  (rest candidates)))))))

  (nreverse
    (find-primes nil (loop for i from 2 upto n collect i)))))


;;;; End.

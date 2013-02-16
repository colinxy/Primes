;;;; Prime number search using a list-based functional
;;;; Eratosthenes sieve.


(defun make-sieve (k n)
  "Create a sieve for prime k consisting of the
   square of k, then each multiple of k up to n."
  (loop for i from (* k k) upto n by k collect i))


(defun in-sieve-p (i sieve)
  "Test whether i is in the sieve. (Tests whether i is equal 
   to the first element of sieve.)
   Returns the result of the test and either a copy of the
   sieve with i removed or the original sieve."
  (let ((in-sieve (equal i (first sieve))))
    (values in-sieve
            (if in-sieve (rest sieve) sieve))))


(defun prime-p (i sieve-list)
  "Test whether i is in any of the sieves in sieve-list.
   Returns the result and updated sieves."
  (labels ((test-sieve (is-prime out-sieves in-sieves)
             (if (null in-sieves)
                 (values is-prime out-sieves)
                 (multiple-value-bind (not-prime new-sieve)
                                      (in-sieve-p i (first in-sieves))
                   (test-sieve (if not-prime nil is-prime)
                               (cons new-sieve out-sieves)
                               (rest in-sieves))))))

     (test-sieve t nil sieve-list)))


(defun primes (n)
  (let ((sqrt-n (isqrt n)))

    (labels ((test-prime (primes sieves candidates)
               (if (null candidates)
                   primes
                   (let ((i (first candidates)))
                     (multiple-value-bind (is-prime new-sieve-list)
                                          (prime-p i sieves)
                       (test-prime (if is-prime (cons i primes) primes)
                                   (if (and is-prime (< i sqrt-n))
                                       (cons (make-sieve i n) new-sieve-list)
                                       new-sieve-list)
                                   (rest candidates)))))))

      (nreverse
        (test-prime nil nil (loop for i from 2 upto n collect i))))))


;;;; End.
               

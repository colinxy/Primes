;;;; Find prime numbers.
;;;; Original versions.

(defun imperative-prime (n)
    "Sieve of Eratosthenes to find primes up to n.
     Imperative implementation."

    (let ((sieve (make-array (1+ n) :element-type '(unsigned-byte 8)
                                    :initial-element 0)))

      (loop for i from 2 upto n when (zerop (aref sieve i)) do
        (loop for j from (* i i) upto n by i do (setf (aref sieve j) 1)))

      (loop for i from 2 upto n when (zerop (aref sieve i)) collect i)))


(defun functional-prime-1 (n)
  "Find primes up to n.
   Functional implementation using trial division."

   (labels ((x-primes (primes candidates)
     (if (null candidates)
         primes
         (let ((pr (car candidates)))
           (x-primes (cons pr primes)
                     (remove-if #'(lambda (x) (zerop (rem x pr))) 
                                  (cdr candidates)))))))

      (nreverse 
        (x-primes nil (loop for i from 2 upto n collect i)))))


(defun build-multiples-list (k n) 
  "Generate a list consisting of the number k, its square, then
   each multiple of k, up to n. The generated list is backwards."

  (labels ((x-multiple (acc nxt)
    (if (> nxt n) 
        acc
        (x-multiple (cons nxt acc) (+ k nxt)))))

    (x-multiple (list k) (* k k))))


(defun get-subsieve-lengths (vals)
  (labels ((x-subsieve (outlst inlst)
       (if (null (cdr inlst))
         outlst
         (x-subsieve (cons (- (cadr inlst) (car inlst)) 
                        outlst) 
                  (cdr inlst)))))

        (nreverse (x-subsieve nil (cons 0 vals)))))

(defun gimme-n-of (what n)
  "Return a list of n whats"
  (loop repeat n collect what))


(defun build-sieve (k n)
  "Construct the 'cross-off' list (Eratosthenes sieve) for prime k."

  (flet ((build-sub (len)
    "Build a list of len-1 zeros and a 1 on the end"
    (nreverse (cons 1 (gimme-n-of 0 (1- len))))))

      (let* ((multiples     (build-multiples-list k n))
             (last-multiple (car multiples))
             (sieve         (mapcan #'build-sub (get-subsieve-lengths (nreverse multiples)))))

      (if (= n last-multiple)
         sieve
         (nconc sieve (gimme-n-of 0 (- n last-multiple)))))))

(defun functional-prime-2 (n)
  "Find primes up to n.
   Functional implementation using Sieve of Eratosthenes."

  (labels ((find-next-prime (primes sieve)
             (let* ((pos (position 0 sieve)))
                (if (null pos)
                    primes
                    (let ((next-prime (1+ pos)))
                      (find-next-prime (cons next-prime primes)
                                       (mapcar #'logior (build-sieve next-prime n) 
                                                        sieve)))))))

     (nreverse 
       (find-next-prime nil (cons 1 (gimme-n-of 0 (1- n)))))))


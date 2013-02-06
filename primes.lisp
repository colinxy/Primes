;;;; Find prime numbers.

;;; ----------------------------------------------------------------------------
;;; Imperative Sieve of Eratosthenes implementation.
;;;

(defun imperative-prime (n)
    "Sieve of Eratosthenes to find primes up to n.
     Imperative implementation."

    (let ((sieve (make-array (1+ n) :element-type '(unsigned-byte 8)
                                    :initial-element 0)))

      (loop for i from 2 upto (isqrt n) when (zerop (aref sieve i)) do
        (loop for j from (* i i) upto n by i do (setf (aref sieve j) 1)))

      (loop for i from 2 upto n when (zerop (aref sieve i)) collect i)))


;;; ----------------------------------------------------------------------------
;;; Naive trial by division implementation using recursion.
;;;

(defun functional-prime-1 (n)
  "Find primes up to n.
   Functional implementation using trial division."

   (labels ((get-primes (primes candidates)
              (if (null candidates)
                  primes
                  (let ((pr (first candidates)))
                    (get-primes (cons pr primes)
                                (delete-if #'(lambda (x) (zerop (rem x pr))) 
                                                         (rest candidates)))))))

      (nreverse 
        (get-primes nil (loop for i from 2 upto n collect i)))))


;;; ----------------------------------------------------------------------------
;;; Functional Sieve of Eratosthenes, version 1.
;;;

(defun build-multiples-list (k n)
  "Generate a list consisting of the prime k, its square, then
   each multiple of k up to n. The generated list is backwards."
  (labels ((gen-multiples (acc nxt)
    (if (> nxt n)
        acc
        (gen-multiples (cons nxt acc) (+ k nxt)))))

    (gen-multiples (list k) (* k k))))       


(defun get-subsieve-lengths (vals)
  (labels ((aux-subsieve (outlst inlst)
    (if (null (rest inlst))
              outlst
              (aux-subsieve (cons (- (second inlst) (first inlst))
                                  outlst)
                            (rest inlst)))))

   (nreverse (aux-subsieve nil (cons 0 vals)))))


(defun gimme-n-of (what n)
  "Return a list of n whats."
  (loop repeat n collect what))


(defun build-sieve (k n)
  "Construct the cross-off list for prime k."
  (flet ((build-sub (len)
    "Build a list of len-1 zeros and a 1 on the end."
    (nreverse (cons 1 (gimme-n-of 0 (1- len))))))

    (let* ((multiples     (build-multiples-list k n))
           (last-multiple (first multiples))
           (sieve         (mapcan #'build-sub (get-subsieve-lengths (nreverse multiples)))))
      (if (= n last-multiple)
          sieve
          (nconc sieve (gimme-n-of 0 (- n last-multiple)))))))


(defun functional-prime-2 (n)
  "Find primes up to n.
   Functional Eratosthenes Sieve."
  (labels ((find-next-prime (primes sieve)
              (let* ((pos (position 0 sieve)))
                (if (null pos)
                    primes
                   (let ((next-prime (1+ pos)))
                     (find-next-prime (cons next-prime primes)
                                      (mapcar #'logior (build-sieve next-prime n) sieve)))))))

   (nreverse
     (find-next-prime nil (cons 1 (gimme-n-of 0 (1- n)))))))


;;; ----------------------------------------------------------------------------
;;; Functional Sieve of Eratosthenes, version 2.
;;; 


(defun build-sieve-2 (k n) 
  "Generate a sieve for prime k consisting of the square of 
   k, then each multiple of k up to n."
  (loop for i from (* k k) upto n by k collect i))


(defun in-sieve-p (i sieve)
  "Test if the first element of the sieve is the same as the 
   integer i. Returns the truth value and an updated sieve."
  (let ((in-sieve (eq i (first sieve))))
    (values in-sieve 
            (if in-sieve (rest sieve) sieve))))


(defun prime-p (i sieves)
  "Test whether i is in any of the sieves.
   Return the result and updated sieves."
  (labels ((test-sieve (is-prime out-sieves in-sieves)
             (if (null in-sieves)
                 (values is-prime out-sieves)
                 (multiple-value-bind (not-prime new-sieve) (in-sieve-p i (first in-sieves))
                    (test-sieve (if not-prime nil is-prime)
                                (cons new-sieve out-sieves)
                                (rest in-sieves))))))

    (test-sieve t nil sieves)))


(defun functional-prime-3 (n)
  (let ((sqrt-n (isqrt n)))

    (labels ((test-prime (primes sieves candidates)
               (if (null candidates)
                   primes
                   (let ((i (first candidates)))
                     (multiple-value-bind (is-prime new-sieves) (prime-p i sieves)
                       (test-prime (if is-prime (cons i primes) primes)
                                   (if (and is-prime (< i sqrt-n))
                                       (cons (build-sieve-2 i n) new-sieves) new-sieves)
                                   (rest candidates)))))))


  (nreverse 
   (test-prime nil nil (loop for i from 2 upto n collect i))))))

     

;;; ----------------------------------------------------------------------------
;;; Functional binary tree.


(defun add-sieve-to-sieveset (sieve sieveset)
  (if (null sieveset)
      (list (first sieve) (rest sieve))
      (cons (first sieveset) (cons (rest sieve) (rest sieveset)))))


(defun cmp-value-sieveset (i sieveset)
  (let ((diff (- i (first sieveset))))
    (cond ((zerop diff) 0)
          (minusp diff) -1)
          (t 1))))


(defun cmp-sieve-sieveset (sieve sieveset)
  (cmp-value-sieveset (first sieve) sieveset))






;;;; End.

;;;; Compute primes to n using a lazy functional
;;;; Sieve of Eratosthenes.


;;; Lazy list. 
;;; This is represented as a dotted pair, with the CAR
;;; containing the first element of the list and the CDR
;;; containing a thunk that generates the next element.
;;; When the list is exhausted, lazy-rest returns nil.

(declaim (inline make-lazy-list
                 lazy-first
                 lazy-rest))


(defun make-lazy-list (thunk)
  (let ((value (funcall thunk)))
    (if (null value)
        nil
        (cons value thunk))))


(defun lazy-first (lazy-list)
  (car lazy-list))


(defun lazy-rest (lazy-list)
  (make-lazy-list (cdr lazy-list)))


;;; Priority queue.
;;;

 
(defun pq-insert (element queue)
  "Insert element into the priority queue."
   (if (null queue)
       (list element)
       (let ((priority (first element))
	     (next-element (first queue)))
	 (if (>= (first next-element) priority)
	     (cons element queue)
	     (cons next-element (pq-insert element (rest queue)))))))



(defun pq-remove (priority queue)
  "Remove elements with a given priority from the queue
   and return them along with the updated queue."
  (labels ((delete-elements (deleted q)
             (if (null q)
                 (values deleted nil)
                 (let* ((next-element (first q))
                        (cmp (- priority (first next-element))))
                   (cond ((minusp cmp)
                          (values deleted q))
                         ((zerop cmp)
                          (delete-elements 
                             (cons next-element deleted)
                             (rest q)))
                         (t
                          (multiple-value-bind (del-lst in-lst)
                                               (delete-elements deleted (rest q))
                            (values del-lst (cons next-element in-lst)))))))))

  (delete-elements nil queue)))


;;; Lazy generators for sieve and prime candidates.
;;;

(defun make-sieve (k n)
  "Lazy k-sieve."
  (let ((next-value (* k k)))
    (make-lazy-list #'(lambda ()
             (if (null next-value) 
                 nil
			     (let ((current-value next-value)
			           (nv (+ k next-value)))
			       (setf next-value (if (> nv n) nil nv))
                   current-value))))))


(defun make-lazy-candidates (n)
  "Lazy prime number candidates list."
  (let ((next-value 2))
    (make-lazy-list #'(lambda ()
      (if (null next-value)
          nil
          (let ((current-value next-value)
                (nv (1+ next-value)))
               (setf next-value (if (> nv n) nil nv))
               current-value))))))



;;; Eratosthenes sieve.
;;;

(defun prime-p (i sieves)
  (multiple-value-bind (matches new-sieves) 
                       (pq-remove i sieves)
    (if (null matches)
        (values t sieves)
        (values nil (reduce #'(lambda (q s) (pq-insert s q))
                            (remove-if 'null
                                       (mapcar 'lazy-rest matches))
                            :initial-value new-sieves)))))

(defun primes (n) 
  (let ((sqrt-n (isqrt n)))

    (labels ((test-prime (primes sieves candidates)
                (if (null candidates)
                primes
                (let ((i (lazy-first candidates)))
                   (multiple-value-bind (is-prime new-sieves)
                                        (prime-p i sieves)
                      (test-prime (if is-prime (cons i primes) primes)
                                  (if (and is-prime (< i sqrt-n))
                                      (pq-insert (make-sieve i n) new-sieves)
                                      new-sieves)
                                  (lazy-rest candidates)))))))

      (nreverse
        (test-prime nil nil (make-lazy-candidates n))))))


;;;; End.

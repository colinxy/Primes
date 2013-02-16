;;;; Find primes to n by functional Sieve of Eratosthenes.
;;;; Uses a pure functional priority queue.

 
(defun pq-insert (element queue)
  "Insert element into the priority queue."
   (if (null queue)
       (list element)
       (let ((priority (first element))
	     (next-element (first queue)))
	 (if (>= (first next-element) priority)
	     (cons element queue)
	     (cons next-element (pq-insert element (rest queue)))))))


(defun pq-lookup (priority queue)
  "Return a list of elements with the given priority."
  (labels ((find-elements (q elements)
              (if (null q)
                  elements
                  (let* ((next-element (first q))
                         (cmp (- priority (first next-element))))
                    (if (minusp cmp) 
                        elements
                        (find-elements (rest q)
                                       (if (zerop cmp) 
                                           (cons next-element elements)
                                           elements)))))))

    (find-elements queue nil)))


(defun pq-discard (priority queue)
   "Discard elements with the given priority."
   (if (null queue)
       nil
       (let* ((next-element (first queue))
              (cmp (- priority (first next-element))))
         (cond ((minusp cmp) queue)
               ((zerop  cmp) (pq-discard priority (rest queue)))
               (t (cons next-element (pq-discard priority (rest queue))))))))


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



(defun make-sieve (k n)
   (loop for i from (* k k) upto n by k collect i))


(defun alt-prime-p (i sieves)
   "Test whether i is in any of the sieves in sieve-list.
    Returns the truth value of the test and an updated
    sieve list with the k-sieves matching i re-inserted
    after removal of their first elements."
   (let ((matching-sieves (pq-lookup i sieves)))
     (if (null matching-sieves)
         (values t sieves)
         (values nil (reduce #'(lambda (q s) (pq-insert s q))
                             (remove-if 'null
                                        (mapcar 'rest matching-sieves))
                              :initial-value (pq-discard i sieves))))))

(defun prime-p (i sieves)
  (multiple-value-bind (matches new-sieves) 
                       (pq-remove i sieves)
    (if (null matches)
        (values t sieves)
        (values nil (reduce #'(lambda (q s) (pq-insert s q))
                            (remove-if 'null
                                       (mapcar 'rest matches))
                            :initial-value new-sieves)))))

(defun primes (n) 
  (let ((sqrt-n (isqrt n)))

    (labels ((test-prime (primes sieves candidates)
                (if (null candidates)
                primes
                (let ((i (first candidates)))
                   (multiple-value-bind (is-prime new-sieves)
                                        (prime-p i sieves)
                      (test-prime (if is-prime (cons i primes) primes)
                                  (if (and is-prime (< i sqrt-n))
                                      (pq-insert (make-sieve i n) new-sieves)
                                      new-sieves)
                                  (rest candidates)))))))

      (nreverse
        (test-prime nil nil (loop for i from 2 upto n collect i))))))

;;;; End.
                                 

;;;; A simple purely functional priority queue implemented as a 
;;;; sorted list of elements.
;;;;

(defpackage :priority-queue 
  (usepackage :common-lisp)
  (export #:insert
          #:lookup-priority
          #:discard-priority
          #:remove-priority))

(in-package :priority-queue)


(defun insert (element fn-priority queue)
  "Insert element into the priority queue.
   The function fn-priority is gives the numerical (integer)
   priority of an element."
   (if (null queue)
       (list element)
       (let ((priority (funcall fn-priority element))
	     (next-element (first queue)))
	 (if (>= (funcall fn-priority next-element) priority)
	     (cons element queue)
	     (cons next-element (insert element fn-priority (rest queue)))))))


(defun lookup-priority (priority fn-priority queue)
  "Return a list of elements with the given priority."
  (labels ((find-elements (q elements)
              (if (null q)
                  elements
                  (let* ((next-element (first q))
                         (cmp (- priority (funcall fn-priority next-element))))
                    (if (minusp cmp) 
                        elements
                        (find-elements (rest q)
                                       (if (zerop cmp) 
                                           (cons next-element elements)
                                           elements)))))))

    (find-elements queue nil)))


(defun discard-priority (priority fn-priority queue)
   "Discard elements with the given priority."
   (if (null queue)
       nil
       (let* ((next-element (first queue))
              (cmp (- priority (funcall fn-priority next-element))))
         (cond ((minusp cmp) queue)
               ((zerop  cmp) (discard-priority priority fn-priority (rest queue)))
               (t (cons next-element (discard-priority priority fn-priority (rest queue))))))))


(defun remove-priority (priority fn-priority queue)
  "Remove elements with a given priority from the queue
   and return them along with the updated queue."
  (labels ((delete-elements (deleted q)
             (if (null q)
                 (values deleted nil)
                 (let* ((next-element (first q))
                        (cmp (- priority (funcall fn-priority next-element))))
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

;;;; End.

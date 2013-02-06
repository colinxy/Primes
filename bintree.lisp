;;;; Binary search tree.

(defpackage :bintree
  (:use :common-lisp)
  (:export #:tree-insert 
           #:tree-search
           #:tree-delete))

(in-package :bintree)


;;; A node is defined as a tuple (left element right).

(declaim (inline node-left node-right node-element new-node))

(defun node-left    (node) (first node))
(defun node-right   (node) (third node))
(defun node-element (node) (second node))

(defun new-node (left element right)
  (list left element right))


(defun test-node (element node cmp-element)
  (funcall cmp-element element (node-element node)))


(defun tree-insert (element 
                    tree 
                    &optional (add-element #'(lambda (v &rest cntr) v))
                              (cmp-element #'-))
  (if (null tree)
      (new-node nil element nil)
      (let ((cmp (test-node element tree cmp-element)))
        (cond ((zerop cmp)
               (new-node (node-left tree)
                         (funcall add-element element (node-element tree))
                         (node-right tree)))
              ((minusp cmp)
               (new-node (tree-insert element (node-left tree))
                         (node-element tree)
                         (node-right tree)))
              (t 
               (new-node (node-left tree)
                         (node-element tree)
                         (tree-insert element (node-right tree))))))))


(defun tree-search (element tree &optional (cmp-element #'-))
  (if (null tree)
      nil
      (let* ((node-value (node-element tree))
             (cmp        (funcall cmp-element element node-value)))
	(cond ((zerop cmp)  node-value)
	      ((minusp cmp) (tree-search element (node-left tree)  cmp-element))
	      (t            (tree-search element (node-right tree) cmp-element))))))


(defun node-children (node) 
  "Returns the number of children of a node."
  (reduce #'+ 
	  (mapcar #'(lambda (x) (if (null x) 0 1))
		  (list (node-left node) (node-right node)))))


(defun node-equal-p (element node cmp-element)
  "Test if node corresponds to element."
  (and (not (null node)) (zerop (test-node element node cmp-element))))


(defun parent-of-p (element this-node cmp-element)
  "Returns whether this-node is a parent of element node."
  (if (minusp (test-node element this-node cmp-element))
      (values -1 (node-equal-p element (node-left this-node)  cmp-element))
      (values  1 (node-equal-p element (node-right this-node) cmp-element))))
    



(defun replace-with-predecessor (tree)
  (multiple-value-bind (predecessor-element subtree) (x-get-predecessor (node-left tree))
    (new-node subtree predecessor-element (node-right tree))))
)


(defun delete-node (tree cmp-element)
  "Remove node from tree and return its replacement node."
  (let ((n-children (node-children tree)))
    (cond ((zerop n-children) nil)
          ((= 1 n-children) 
           (if (null (node-left tree)) 
               (node-right tree) 
               (node-left tree)))
          (t (replace-with-predecessor tree)))))


(defun tree-delete (element tree &optional (cmp-element #'-))
  (if (node-equal-p element tree cmp-element)
      (delete-node tree cmp-element)
      (if (and (null (node-left tree)) (null (node-right tree)))
          tree
          ;; Recursively descend, reconstructing ancestors 
          ;; of the node to delete.
          (multiple-value-bind (descend-direction is-parent) (parent-of-p element tree cmp-element)
            (if (minusp descend-direction)
                (new-node (if (null is-parent)
                              (tree-delete element (node-left tree) cmp-element)
                              (delete-node (node-left tree) cmp-element))
                          (node-element tree)
                          (node-right tree))
                (new-node (node-left tree) 
                          (node-element tree) 
                          (if (null is-parent)
                              (tree-delete element (node-right tree) cmp-element)
                              (delete-node (node-right tree) cmp-element))))))))


;;;; End.

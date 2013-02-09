;;;; Binary search tree.
;;;; Implemented as a purely functional (persistent) structure.
;;;; Insertion and deletion operations return a new tree and leave
;;;; the original unchanged.


(defpackage :bintree
  (:use :common-lisp)
  (:export #:tree-insert 
           #:tree-search
           #:tree-delete))

(in-package :bintree)


;;; A node is defined as a tuple (left element right).

(declaim (inline node-left node-right node-element new-node test-node))

(defun node-left    (node) (first node))
(defun node-right   (node) (third node))
(defun node-element (node) (second node))

(defun new-node (left element right)
  (list left element right))


(defun test-node (element node cmp-element)
  "Compare element with node.
   Return 0 if the element and node contents are equal
   a negative value if the element is less than the 
   or a positive value if the element is greater than
   the node value."
  (funcall cmp-element element (node-element node)))


(defun tree-insert (element 
                    tree 
                    &optional (add-element #'(lambda (v &rest cntr) v))
                              (cmp-element #'-))
  "Insert element into tree, returning a new tree."
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
  "Search tree for element.
   Returns nil if the element is not found, otherwise returns
   the tree node's value."
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
    

(defun x-get-predecessor (node)
  (labels ((right-descend (node)
             (let ((right-child (node-right node)))
               (if (null right-child)
                    node
                    (right-descend right-child)))))

    (right-descend (node-left node))))
      

(defun replace-with-predecessor (tree cmp-element)
  "Return a new node consisting of the immediate predecessor of 
   the element in tree, the left subtree with the predecessor
   removed, and the right subtree as it is."
  (let* ((predecessor-node (x-get-predecessor tree))
         (predecessor-element (node-element predecessor-node)))
    (new-node (tree-delete predecessor-element (node-left tree) cmp-element)
              predecessor-element
              (node-right tree))))


(defun delete-node (tree cmp-element)
  "Remove node from tree and return its replacement node."
  (let ((n-children (node-children tree)))
    ;; We treat three cases: the node to delete has no children,
    ;; one child or two children.
    (cond ;;The no-child case is the simplest - simply delete the node.
          ((zerop n-children) nil)
          ;; If one child, replace the node with the child.
          ((= 1 n-children) 
           (if (null (node-left tree)) 
               (node-right tree) 
               (node-left tree)))
          ;; With two children, replace the node with either its
          ;; immediate predecessor (left subtree) and leave the
          ;; right subtree unchanged, or with its immediate successor
          ;; (right subtree) and leave the left subtree unchanged.
          (t (replace-with-predecessor tree cmp-element)))))


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

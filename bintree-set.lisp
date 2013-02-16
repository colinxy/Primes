;;;; Binary search tree for collections. 
;;;; Implemented as a purely functional (persistent) structure.
;;;; Insertion and deletion operations return a new tree and leave
;;;; the original unchanged.
;;;; A collection has a key and a list of values.


(defpackage :bintree
  (:use :common-lisp)
  (:export #:tree-insert 
           #:tree-search
           #:tree-delete))

(in-package :bintree)



;;; ---------------------------------------------------------------------
;;; Node utility functions.
;;; A node is defined as a tuple (left element right).
;;; An element is defined as a tuple (key values).

(declaim (inline node-left node-right node-element 
                 node-key  node-values 
                 new-node  test-node))

(defun node-left    (node) (first node))
(defun node-right   (node) (third node))
(defun node-element (node) (second node))
(defun node-key     (node) (first  (node-element node)))
(defun node-values  (node) (second (node-element node)))


(defun new-node (left element right)
  (list left element right))


(defun test-node (key node)
  "Compare key with node.
   Returns:
      0 if the key and node key are equal,
      a negative value if key is less than the node key, 
      a positive value if key is greater than the node key."
  (- key (node-key node)))


;;; ---------------------------------------------------------------------


(defun tree-insert (key value tree)
  "Insert the key/value pair into tree, returning a new tree."
  (if (null tree)
      (new-node nil (list key (list value)) nil)
      (let ((cmp (test-node key tree)))
        (cond ((zerop cmp)
               (new-node (node-left tree)
                         (list key (cons value (node-values tree)))
                         (node-right tree)))
              ((minusp cmp)
               (new-node (tree-insert key value (node-left tree))
                         (node-element tree)
                         (node-right tree)))
              (t 
               (new-node (node-left tree)
                         (node-element tree)
                         (tree-insert key value (node-right tree))))))))


;;; ---------------------------------------------------------------------


(defun tree-search (key tree &optional not-found)
  "Search tree for key and return the associated value list.
   Returns nil (or not-found, if specified) if the key is not found."
  (if (null tree)
      not-found
      (let ((cmp (test-node key tree)))
        (cond ((zerop cmp)  (node-values tree))
              ((minusp cmp) (tree-search key (node-left tree) not-found))
              (t            (tree-search key (node-right tree) not-found))))))


;;; ---------------------------------------------------------------------
;;; Node deletion.
;;; We have to consider three cases:
;;; (a) The node to delete has no children. In this case, we simply 
;;; remove the node.
;;; (b) The node to delete has a single child. In this case we replace
;;; the node with its child.
;;; (c) The node to delete has two children. In this case we can either
;;;     (i) replace the node with its immediate predecessor, in which
;;;         case we delete the predecesor from the left subtree and
;;;         leave the right subtree unchanged, or
;;;     (ii) replace the node with its immediate successor, in which
;;;         case we leave the left subtree unchanged and delete the 
;;;         predecessor from the right subtree.
;;;     The immediate successor/predecessor will only have one child
;;;     at most, so deleting it can be handled as case (a) or (b) 
;;;     above.
;;; Because we are dealing with a purely functional tree, we have to
;;; create a new copy of all the ancestors of the node to delete as 
;;; well as the node itself. We need to find the *parent* of the node
;;; to be deleted.


(defun node-children (node) 
  "Returns the number of children of a node."
  (reduce #'+ 
          (mapcar #'(lambda (x) (if (null x) 0 1))
                  (list (node-left node) (node-right node)))))


(defun node-equal-p (key node)
  "Test if node corresponds to key."
  (and (not (null node)) (zerop (test-node key node))))


(defun parent-of-p (key this-node)
  "Returns whether this-node is a parent the node associated
   with key."
  (if (minusp (test-node key this-node))
      (values -1 (node-equal-p key (node-left this-node)))
      (values  1 (node-equal-p key (node-right this-node)))))
    

(defun get-successor (node)
  "Return leftmode node in right subtree, which corresponds
   to this node's immediate successor."
  (labels ((left-descend (n)
            (let ((left-child (node-left n)))
              (if (null left-child)
                  n
                  (left-descend left-child)))))

    (left-descend (node-right node))))


(defun replace-with-successor (tree)
  "Return a new node consisting of the immediate successor of
   the element in tree, the right subtree with the successor
   removed, and the left subtree as it is."
  (let ((successor-node (get-successor tree)))
    (new-node (node-left tree)
              (node-element successor-node)
              (tree-delete (node-key successor-node) (node-right tree)))))


(defun get-predecessor (node)
  "Return rightmost node in left subtree, which corresponds
   to this node's immediate predecessor."
  (labels ((right-descend (node)
             (let ((right-child (node-right node)))
               (if (null right-child)
                    node
                    (right-descend right-child)))))

    (right-descend (node-left node))))
      

(defun replace-with-predecessor (tree)
  "Return a new node consisting of the immediate predecessor of 
   the element in tree, the left subtree with the predecessor
   removed, and the right subtree as it is."
  (let ((predecessor-node (get-predecessor tree)))
    (new-node (tree-delete (node-key predecessor-node) (node-left tree))
              (node-element predecessor-node)
              (node-right tree))))


(defun delete-node (tree)
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
          (t (if (zerop (random 2))
                 (replace-with-predecessor tree)
                 (replace-with-successor tree))))))


(defun tree-delete (key tree)
  (if (node-equal-p key tree)
      (delete-node tree)
      (if (and (null (node-left tree)) (null (node-right tree)))
          tree ; Element not found - return the new tree.
          ;; Recursively descend, reconstructing ancestors 
          ;; of the node to delete.
          (multiple-value-bind (descend-direction is-parent) (parent-of-p key tree)
            (if (minusp descend-direction)
                (new-node (if (null is-parent)
                              (tree-delete key (node-left tree))
                              (delete-node (node-left tree)))
                          (node-element tree)
                          (node-right tree))
                (new-node (node-left tree) 
                          (node-element tree) 
                          (if (null is-parent)
                              (tree-delete key (node-right tree))
                              (delete-node (node-right tree)))))))))


;;;; End.

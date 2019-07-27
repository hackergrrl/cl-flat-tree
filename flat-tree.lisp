;;               7 
;;       3                11
;;   1       5       9          13
;; 0   2   4   6   8   10   12     14


(defpackage :flat-tree (:use cl))

(in-package :flat-tree)

(defun index (depth offset)
  "Returns an array index for the tree element at the given depth and offset."
  (+
   (1- (expt 2 depth))
   (* (expt 2 (1+ depth)) offset)))

(defun depth-n? (index depth)
  "Returns T if 'index' is of depth 'depth'."
  (= 0 (mod (- index (1- (expt 2 depth))) (expt 2 (1+ depth)))))

(defun depth (index)
  "Returns the depth of an index."
  (let ((depth 0))
    (loop
       until (depth-n? index depth)
       do (incf depth))
    depth))

(defun step-size (depth)
  "Returns the offset step size, given 'depth'."
  (expt 2 (1+ depth)))

(defun offset (index)
  "Returns the offset of an index."
  (let* ((d (depth index))
         (step (step-size d)))
    (decf index (1- (expt 2 d)))
    (/ index step)))

(defun sibling (index)
  "Returns the index of this element's sibling."
  (if (evenp (offset index))
      (+ index (step-size (depth index)))
      (- index (step-size (depth index)))))

(defun parent (index)
  "Returns the index of the parent element in tree."
  (/ (+ index (sibling index)) 2))

(defun children (index)
  "Returns a list (leftChild rightChild) with the indexes of this element's children. If this element does not have any children it returns NIL."
  (let* ((d (depth index))
         (step (/ (step-size (1- d)) 2)))
    (if (= d 0) nil
        (list (- index step) (+ index step)))))

(defun left-span (index)
  "Returns the left spanning in index in the tree index spans."
  (let ((step (1- (expt 2 (depth index)))))
    (- index step)))

(defun right-span (index)
  "Returns the right spanning in index in the tree index spans."
  (let ((step (1- (expt 2 (depth index)))))
    (+ index step)))

(defun spans (index)
  "Returns the range (inclusive) that the tree rooted at 'index' spans. For example (spans 3) would return (0 6)."
  (list (left-span index) (right-span index)))

(defun counts (index)
  "Returns how many nodes (including parent nodes) a tree contains."
  (1- (expt 2 (1+ (depth index)))))

(defun full-roots (index)
  (when (not (evenp index)) (error "You can only look roots for depth=0 nodes"))
  (setf index (/ index 2))
  (let ((result nil)
        (offset 0)
        (factor 1))
    (loop until (= index 0)
       do (progn
            (loop while (<= (* factor 2) index) do (setf factor (* factor 2)))
            (push (+ offset factor -1) result)
            (incf offset (* 2 factor))
            (decf index factor)
            (setf factor 1)))
    (nreverse result)))

(defstruct iterator
  (index 0)
  (step-size 2)
  (offset 0)
  (depth 0))

(defun iterator-next (iter)
  (incf (iterator-index iter) (iterator-step-size iter))
  (incf (iterator-offset iter)))

(defun iterator-prev (iter)
  (when (> (iterator-offset iter) 0)
    (decf (iterator-index iter) (iterator-step-size iter))
    (decf (iterator-offset iter))))
  
(defun iterator-seek (iter index)
  (setf (iterator-index iter) index)
  (setf (iterator-step-size iter) (step-size (depth index)))
  (setf (iterator-offset iter) (offset index))
  (setf (iterator-depth iter) (depth index)))

(defun iterator-parent (iter)
  )

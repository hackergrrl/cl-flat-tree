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

(defun offset (index)
  "Returns the offset of an index."
  (let* ((d (depth index))
         (step (expt 2 (1+ d))))
    (decf index (1- (expt 2 d)))
    (/ index step)))

(defun sibling (index)
  "Returns the index of this element's sibling."
  )

(defun parent (index)
  "Returns the index of the parent element in tree."
  )

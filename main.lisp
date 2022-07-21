(defmacro make-vector (&aux type)
  (if type
    `(make-array 0 :fill-pointer 0 :adjustable t :element-type ,type)
    `(make-array 0 :fill-pointer 0 :adjustable t)))

(defun vector-push-resize (var vec)
  (adjust-array vec (1+ (array-dimension vec 0)))
  (vector-push var vec))

(defun delete-nth (i seq)
  "Delete nth element from sequence."
  (let ((slide (subseq seq (1+ i)))
        (num (1- (fill-pointer seq))))
    (replace seq slide :start1 i)
    (adjust-array seq num
                  :fill-pointer num)))

(defun make-tree (&aux
                    (relations (make-vector 'integer))
                    (names (make-vector 'string)))
  (when (zerop (array-dimension relations 0))
    (vector-push-resize -1 relations)
    (vector-push-resize nil names))
  (cons relations names))

(defun new-node (tree parent)
  (vector-push-resize parent (car tree))
  (vector-push-resize nil (cdr tree))
  (1- (array-dimension (car tree) 0)))

(defmacro ref-parent (tree node)
  `(aref (car ,tree) ,node))

(defmacro ref-name (tree node)
  `(aref (cdr ,tree) ,node))

(defun path-to-root (tree node)
  (let ((path
          (make-vector 'integer)))
    (loop
      (when
          (= node -1)
        (vector-push-resize node path)
        (return path))
      (vector-push-resize node path)
      (setq node (ref-parent tree node)))))

(defun path-from-root (tree node)
  (reverse (path-to-root tree node)))

(defun in-range (tree n)
  (and (< n (1- (array-dimension (car tree) 0))) (>= n 0)))

(defun get-occurences (tree node)
  (let ((found
          (make-vector 'integer)))
    (dotimes (i (array-dimension (car tree) 0))
      (when (= (ref-parent tree i) node)
        (vector-push-resize i found)))
    found))

(defun vector-cat (vec1 vec2)
  (adjust-array vec1 (+ (array-dimension vec1 0) (array-dimension vec2 0)))
  (dotimes (n (array-dimension vec2 0))
    (vector-push (aref vec2 n) vec1)))

(defun get-all-children (tree node)
  (let ((found
          (get-occurences tree node))
        (nfound
          (make-vector 'integer))
        (i 0))
    (loop
      (when
          (= i (1- (array-dimension found 0)))
        (return found))
      (setf nfound (get-occurences tree (aref found i)))
      (vector-cat found nfound)
      (incf i))))

(defun remove-node (tree node)
  (labels ((r-last-node (tree node)
             (delete-nth node (car tree))
             (delete-nth node (cdr tree))
             (dotimes (i (array-dimension (car tree) 0))
               (when (> (aref (car tree) i) node) (decf (aref (car tree) i))))))
    (let ((found
            (get-all-children tree node)))
      (loop
        (when (zerop (array-dimension found 0)) (r-last-node tree node) (return t))
        (r-last-node tree (aref found (1- (array-dimension found 0))))
        (delete-nth (1- (array-dimension found 0)) found)
        (dotimes (i (array-dimension found 0))
          (when (> (aref found i) node) (decf (aref found i))))))))

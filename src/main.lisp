;;;; Copyright (c) 2026 Alexei Pustovoi
;;;; SPDX-License-Identifier: CeCILL-2.1


(defun ranged-random (from to)
  (+ from (random to)))

(defun generate-tree (depth width &optional (max? t))
  (cond
    ((= 0 depth)
     (ranged-random -100 100))
    (max?
     (cons 'MAX
           (loop repeat width
                 collect (generate-tree
                          (1- depth)
                          width
                          nil))))
    (t
     (cons 'MIN
           (loop repeat width
                 collect (generate-tree
                          (1- depth)
                          width
                          t))))))

(defun minimax (node)
  (cond
    ((not (listp node)) node)
    ((equal (car node) 'MAX)
     (loop for x in (cdr node)
	   maximizing (minimax x)))
    ((equal (car node) 'MIN)
     (loop for x in (cdr node)
	   minimizing (minimax x)))))

(defun alpha-beta (node)
  (labels ((alpha-beta1 (node alpha beta)
             (cond
               ((not (listp node))
                node)
               ((equal (car node) 'MAX)
                (let ((best -101))
                  (loop for x in (cdr node)
                        for value = (alpha-beta1 x alpha beta)
                        do
                          (setf best (max best value))
                          (setf alpha (max alpha best))
                          (when (>= alpha beta)
                            (return best)))
                  best))
               ((equal (car node) 'MIN)
                (let ((best 101))
                  (loop for x in (cdr node)
                        for value = (alpha-beta1 x alpha beta)
                        do
                          (setf best (min best value))
                          (setf beta (min beta best))
                          (when (>= alpha beta)
                            (return best)))
                  best)))))
    (alpha-beta1 node
                 -101
                 101)))

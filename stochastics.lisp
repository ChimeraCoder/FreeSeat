


;;Assume global constant *MATRIX*

;;Define the weight to be the size of the group
(defun calculate-weight (group)
  (length group))


;;Calculate the score of a group of people
(defun calculate-score (group)
  (let ( (head (car group))
         (tail (cdr group))
         )
    (if tail
      (+ 
        (calculate-score-with-everyone-else head tail) ;;The score of the first person with each other person
        (calculate-score group)                        ;;The scores of every other person EXCEPT the first, with each other
      )
      '0                                               ;;The last person has already been compared to everyone else
      )
    )
  )
    
;;Given a person A and a list of other people L, calculate the sum of the pairwise scores of A with each person in L
(defun calculate-score-with-everyone-else (person tail)
  ;;Sum the list of scores
  (apply '+
         ;;Calculate the pairwise score of the person with every person in the tail
         (apply 
           (lambda (other) (pairwise-score person other))
           tail)
         )
  )

;;Get the score (hard or soft) between two people. Assume symmetry in the true values, but at most one of the two values may be incorrectly recorded as zero
(defun pairwise-score (a b)
  (let ( (row-wise (score a b))
         (column-wise (score b a))
         )
    (if (eq row-wise '0)
      (row-wise)
      (column-wise)
      )
    )
  )

;; This function is defined separately in case we want to separate the matrix into two amtrices (hard and soft) later on
(defun score (a b)
  (aref *matrix* a b)
  )


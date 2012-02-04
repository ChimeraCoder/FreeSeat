(in-package :freeseat)

;;seeds the random state
;; don't do this yet!
(setf *random-state* (make-random-state t)) 


;;Assume global constant *MATRIX*

;;Define the weight to be the size of the group
(defun calculate-weight (group)
  (length group))


;;Calculate the score of a group of people
(defun calculate-score (group)
  (let ((head (car group))
        (tail (cdr group)))
    (if tail
        (+ (calculate-score-with-everyone-else head tail) ;;The score of the first person with each other person
           (calculate-score tail))                        ;;The scores of every other person EXCEPT the first, with each other
        '0)))                                               ;;The last person has already been compared to everyone else
    
;;Given a person A and a list of other people L, calculate the sum of
;;the pairwise scores of A with each person in L
(defun calculate-score-with-everyone-else (person tail)
  ;;Sum the list of scores
  (apply '+
         ;;Calculate the pairwise score of the person with every person in the tail
         (map 'list
              (lambda (other) (pairwise-score person other))
              tail)))

;;Get the score (hard or soft) between two people. Assume symmetry in
;;the true values, but at most one of the two values may be
;;incorrectly recorded as zero
(defun pairwise-score (a b)
  (let ((row-wise (score a b))
        (column-wise (score b a)))
    (if (eq row-wise '0)
        row-wise
        column-wise)))

;; This function is defined separately in case we want to separate the
;; matrix into two matrices (hard and soft) later on
(defun score (a b)
  (aref *matrix* a b))

;;Get random person from a group
(defun get-random-person (group)
  (nth (random (length group)) group))

;;Get random group from a list of groups
(defun get-random-group (groups)
  (let ((len (length groups)))
    (if (> len 0)
        (nth (random len) groups)
        '())))

;;Returns a copy of the list without the nth element
(defun remove-nth (list n)
    (remove-if (constantly t) list :start n :end (1+ n)))

;;Simulate the scores of two groups after switching two random members
;;Return NIL if no changes should be made; otherwise, returns a list
;;(of length 2) containing the two new groups. Some of these
;;intermediate definitions can be removed, but I've left them in for
;;now so it's easy to tweak our algorithm, since this is the core
;;heuristic
(defun possibly-switch-groups (group1 group2)
  (let* ((prescore1 (calculate-score group1))
         (prescore2 (calculate-score group2))
         (switched-person-1 (get-random-person group1))
         (switched-person-2 (get-random-person group2))
         (hypothetical-table-1 (cons switched-person-1 
                                     (remove switched-person-2 group2)))
         (hypothetical-table-2 (cons switched-person-2
                                     (remove switched-person-1 group1)))
         (postscore1 (calculate-score hypothetical-table-1))
         (postscore2 (calculate-score hypothetical-table-2))
         (pretotal (+ prescore1 prescore2))
         (posttotal (+ postscore1 postscore2)))

    (if (or (>= posttotal pretotal ) (> (/ posttotal pretotal) (random 1.0)))
      (list hypothetical-table-1 hypothetical-table-2)
      '())))

; Tom and Mary must sit together
; Tom and Josephina must not sit together


(let ((matrix (make-array '(num-people num-people))) :initial-element 0)
     (defun set-relationship (person1 person2 level)
       ;;Return matrix
       )

     (defun symmetrically-set (person1 person2 level)
       (setrelationship matrix person1 person2 level)
       ;;Set both slots in the relationship to level
       ;;Return matrix
       )

     (defun get-relationship (person1 person2)
       ;; Return the relationship weight from person1 to person2
       )

     (defun get-indexes-sorted ()
       ;; Return a list of user ids sorted by the sum of the absolute
       ;; value of the weights in their row, only if they have any
       ;; restriction
       )

     (defun seat-by-most-restricted (indexes-sorted groups)
       ;; Takes the list of indexes sorted by restriction and a list
       ;; of groups stored as lists and returns the groups populated
       ;; with people
       )

     
       



(setf (aref 1 2) 5)
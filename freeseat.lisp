(in-package :freeseat)

(let* ((filename "sampleinput.txt")
       (processed (initial-processing filename)))
  (defvar *tables* (nth 0 processed))
  (defvar *name-to-id* (nth 1 processed))
  (defvar *id-to-name* (nth 2 processed))
  (defvar *num-people* (nth 3 processed))
  (defvar *groups* (nth 4 processed))
  (defvar *matrix* (nth 5 processed)))


;;Randomizes the order of the list
;;Fix the naming here
(defun randomize-list (l)
  (let ((random-group (get-random-group l)))
    (cons random-group (randomize-list (remove random-group l)))))

;;Given a list l of randomized groups, combine any two groups if they can be combined
(defvar combine-groups (l)
  (let ((first-group) (car l)
        (second-group) (cadr l))
    (if (and (combination-possible first-group second-group) (<= (+ (calculate-weight first-group) (calculate-weight second-group)) *table-max-size*))
      (cons (concatenate 'list first-group second-group) (combine-groups (cddr l)))
      (cons first-group (cons second-group (combine-groups cddr l))))))

(defun mcmc (groups) 
  (let* ((initial-configuration (combine-groups (randomize-list groups )))
         (group1 (get-random-group initial-configuration))
         (group2 (get-random-group initial-configuration))
         (switch-result (possibly-switch-groups group1 group2))
    (if (or (equal group1 group2) (not switch-result)))   ;;This can be optimized/combined in a clever way using or
      (initial-configuration)
      (cons (car switch-result) 
            (cons (cadr switch-result) 
                  (remove group1 
                          (remove group2 initial-configuration)))))))


;TODO Figure out how to do parallel-let!
(defun many-parallel-mcmc (groups)
   (parallel-let ((result1 (iterate-mcmc groups))
                  (result2 (iterate-mcmc groups))
                  (result3 (iterate mcmc groups))
                  (result4 (iterate-mcmc groups)))
            '(result1 result2 result3 result4)))



        




(defun combination-possible 
  
  don't compile )






(in-package :freeseat)

;;Input is a GoogleDoc, downloaded as a .csv
(let* ((filename "FreeSeat Launch Party%21 - Sheet1.csv")
       (processed (initial-processing filename)))
  (defvar *table-max-size* 5)
  (defvar *name-to-id* (nth 0 processed))
  (defvar *id-to-name* (nth 1 processed))
  (defvar *num-people* (nth 2 processed))
  (defvar *groups* (nth 3 processed))
  (defvar *matrix* (nth 4 processed)))


;;Randomizes the order of the list
;;Fix the naming here
(defun randomize-list (l)
  (if l
      (let ((random-group (get-random-group l)))
        (cons random-group (randomize-list (remove random-group l))))
      '()))

(defun combination-possible (group1 group2)
  (defun can-sit-p-p (person1 person2)
    (not (= (aref *matrix*  person1 person2) *cannot*)))
  (defun can-sit-p-g (person group)
    (if group
        (and (can-sit-p-p person (car group))
             (can-sit-p-g person (cdr group)))
        t))
  (defun can-sit-g-g (group1 group2)
    (if group1
        (and (can-sit-p-g (car group1) group2)
             (can-sit-g-g (cdr group1) group2))
        t))
  (can-sit-g-g group1 group2))


;;Given a list l of randomized groups, combine any two groups if they
;;can be combined
(defun combine-groups (l)
  (if l 
      (let ((first-group (car l))
            (second-group (cadr l)))
        (if (and (combination-possible first-group second-group)
                 (<= (+ (calculate-weight first-group)
                        (calculate-weight second-group))
                     *table-max-size*))
            (cons (concatenate 'list first-group second-group)
                  (combine-groups (cddr l)))
            (cons first-group
                  (cons second-group (combine-groups (cddr l))))))
      '()))

(defun mcmc (groups)
  (let* ((initial-configuration (combine-groups (randomize-list groups)))
         (group1 (get-random-group initial-configuration))
         (group2 (get-random-group initial-configuration))
         (switch-result (possibly-switch-groups group1 group2)))
    (if (or (equal group1 group2)
            (not switch-result))   ;;This can be optimized/combined
        ;;in a clever way using OR
        initial-configuration
        (cons (car switch-result) 
              (cons (cadr switch-result) 
                    (remove group1 
                            (remove group2 initial-configuration)))))))



;;#TODO Figure out how to do parallel-let!
;;SBCL should be able to do something to its effect with its threading support
;;#TODO figure out how to run this through SBCL with native threading
(defun many-parallel-mcmc (groups threads iterations)
  (if (<= threads 0)
      '()
      (cons (iterate-mcmc groups iterations)
            (many-parallel-mcmc groups (1- threads) iterations))))

(defun iterate-mcmc (groups iterations)
  (if (<= iterations 0)
      groups
      (iterate-mcmc (mcmc groups) (1- iterations))))

(let ((n 0))
  (defun counter () (incf n)))

(dolist (group (iterate-mcmc *groups* 7000))
  (format t "Group ~a:~%" (counter))
  (dolist (member group)
    (format t "  ~a~%" (aref *id-to-name* member))))

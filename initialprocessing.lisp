(in-package :freeseat)

(defparameter *must* 100)
(defparameter *cannot* -100)

(defun read-file (filename)
"Reads through a file in proper format. Returns a list of the sexps in
the file. The first one should be a list of table sizes; the next (all
symbols) is the guest list; the remainder are seating requests."
  (with-open-file (stream filename)
    (labels ((read-sexp ()
               (let ((sexp (read stream nil)))
                 (if sexp (cons sexp (read-sexp))
                     '()))))
      (read-sexp))))

(defun make-guest-list-lookup (guest-list)
"Takes the guest list (in form '(PERSON1 PERSON2)) and returns a list
containing a hash table from person names to id, an array of names,
with each name at its proper id index, and the number of guests"
  (let ((guest-num -1))
    (defun next-guest () (incf guest-num))
    (defun num-guests () (1+ guest-num)))

  (let ((name-to-id (make-hash-table :test #'equalp))
        (id-to-name (make-array 5 :adjustable t :fill-pointer 0)))
    (defun process-next-guest (guest-list)
      (let ((guest (car guest-list)))
        (when guest
          (setf (gethash guest name-to-id) (next-guest))
          (vector-push-extend guest id-to-name)
          (process-next-guest (cdr guest-list)))))
    (process-next-guest guest-list)
    `(,name-to-id ,id-to-name ,(num-guests))))

(defun requirements-matrix-nontrans (requests name-to-id num-people)
"Takes the number of people, the hash of names to ids, and the list of
seating requests and returns a matrix of the requests,
nontransitively. Should be symmetric diagonally"
  (defun index (name)
    (gethash name name-to-id))
  ; Construct first matrix, non-transitive, but symmetric
  (let ((requirements
         (make-array `(,num-people ,num-people) :initial-element 0)))
    (defun process-requests (requests)
      (let ((request (car requests)))
        (if request
          (let ((score (first request))
                (id1 (index (second request)))
                (id2 (index (third request))))
            (setf (aref requirements id1 id2) score)
            (setf (aref requirements id2 id1) score)
            (process-requests (cdr requests)))
          requirements))))
  (process-requests requests))
        

;; Construct groups from first matrix 
(defun matrix-to-groups (matrix num-people)
"Give the initial symmetric, non-transitive matrix, returns a list of
lists of ids that represent groupings that must remain together."
  ;; Given an id, returns a list of all of the "musts" for that
  ;; person, non-transitively
  (defun musts-nontrans (id)
    (let ((musts '()))
      (dotimes (i num-people)
        (when (eql (aref matrix id i) *must*)
          (push i musts)))
      musts))

  (let ((seen (make-hash-table)))
    (defun seen-p (id)
      (gethash id seen))
    (defun mark-seen! (id)
      (setf (gethash id seen) t))
    (defun join (lists)
      (apply 'concatenate 'list lists))
    (defun musts-trans (id)
      (when (not (seen-p id))
        (mark-seen! id)
        (cons id (join (map 'list 'musts-trans (musts-nontrans id)))))))

  (let ((groups '()))
    (dotimes (i num-people)
      (let ((group (musts-trans i)))
        (when (not (null group))
          (push group groups))))
    (nreverse groups)))

(define-condition unsatisfiable-condition-error (error)
  ((text :initarg :text :reader text)))

;; Construct second matrix transitively (absolutes take precedence,
;; then average the soft requests (error checking goes here)

(defun groups-to-matrix2 (groups num-people matrix)
"Takes the list of groups of 'musts', the number of people on the
guest list, and the matrix that contains the list of requirements read
in symmetrically and returns a new matrix where every member of a
group has identical rows and columns"

  (defun process-score (score value status)
    ;; Take in score, value, status; return (newvalue . newstatus)
    (cond ((= score *must*)
           (if (eql status :cannot)
               (error 'unsatistiable-condition-error
                      :text "Read a cannot value when status was must")
               (cons *must* :must)))
          ((= score *cannot*)
           (if (eql status :must)
               (error 'unsatistiable-condition-error
                      :text "Read a must value when status was cannot")
               (cons *cannot* :cannot)))
          (t
           (if (eql status :soft)
               (cons (+ score value) :soft)
               (cons value status)))))

  ; bug: will return the total value instead of *cannot* for members
  ; of a group that contains a member with which the group cannot sit
  ; correct for me in compute-array/going from array to matrix
  (defun compute-group-value (group id)
    (let ((value 0) (status :soft))
      (dolist (group-member group)
        (let* ((score (aref matrix id group-member))
               (processed (process-score score value status)))
          (setf value (car processed))
          (setf status (cdr processed))))
      (if (eql status :soft) (/ value (length group))
          value)))

  (defun compute-group-array (group)
    (let ((group-array (make-array num-people)))
      (dotimes (i num-people)
        (setf (aref group-array i) (compute-group-value group i)))
      group-array))

  (let ((new-matrix (make-array `(,num-people ,num-people))))
    (defun set-row-sym (member array)
      (dotimes (i num-people)
        (let ((value (cond ((= (aref matrix i member) *must*) *must*)
                           ((= (aref matrix i member) *cannot*) *cannot*)
                           (t (aref array i)))))
          (setf (aref new-matrix member i) value)
          (setf (aref new-matrix i member) value))))
    
    (dolist (group groups)
      (let ((array (compute-group-array group)))
        (dolist (member group)
          (set-row-sym member array))))
    new-matrix))

(defun initial-processing (filename)
"Chains all of the functions together and returns a list of useful
output: the tables list, name to id hash, id to name array, number of
people, the groupings of people, and the transitively calculated
scoring matrix"
  (let ((input (read-file filename)))
    (setf tables (first input))
    (setf guest-list (second input))
    (setf requests (third input)))
  (let ((guests-processed (make-guest-list-lookup guest-list)))
    (setf name-to-id (first guests-processed))
    (setf id-to-name (second guests-processed))
    (setf num-people (third guests-processed)))
  (setf nontrans-matrix
        (requirements-matrix-nontrans requests name-to-id num-people))
  (setf groups (matrix-to-groups nontrans-matrix num-people))
  (setf trans-matrix (groups-to-matrix2 groups num-people nontrans-matrix))
  `(,tables ,name-to-id ,id-to-name ,num-people ,groups ,trans-matrix))

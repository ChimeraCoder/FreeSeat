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
      (cons tables (read-sexp))))))

(defun make-guest-list-lookup (guest-list)
"Takes the guest list (in form '(PERSON1 PERSON2)) and returns a list
containing a hash table from person names to id, an array of names,
with each name at its proper id index, and the number of guests"
  (let ((guest-num -1))
nn    (defun next-guest () (incf guest-num))
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
"DOC ME"
  ; make an empty, appropriately sized matrix

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

  ; This is only here to test
  (compute-group-value groups num-people))
          
;  (defun compute-group-array (group)
;    (let ((group-vector '()))
;      (
  ; for each group
    ; create the group vector
    ; iterate through every index
      ; if any must, *must*
      ; if any cannot, *cannot* CHECK ERRORS HERE
      ; else average
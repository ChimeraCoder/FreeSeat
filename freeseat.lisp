(in-package :freeseat)

(let* ((filename "sampleinput.txt")
       (processed (initial-processing filename)))
  (defvar *tables* (nth 0 processed))
  (defvar *name-to-id* (nth 1 processed))
  (defvar *id-to-name* (nth 2 processed))
  (defvar *num-people* (nth 3 processed))
  (defvar *groups* (nth 4 processed))
  (defvar *matrix* (nth 5 processed)))


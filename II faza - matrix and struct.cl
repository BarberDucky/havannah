;;variable and constant definitions ****************************************************************************************************************************************************

(defconstant *emptyField* '-)
(defconstant *invalidField* '0)
(defvar *n* )                                ;; n is the board dimension
(setq *n* '())
(defvar *matrixDim*)                         ;; matrix dimension
(setq *matrixDim* '())
(defvar *board*)                             ;; stores the board matrix
(setq *board* '())

(defconstant *firstPlayer* 'X)
(defconstant *secondPlayer* 'O)

;;human and computer
(defvar *human*)
(setq *human* '())
(defvar *computer*)
(setq *computer* '())

(defstruct cell
  value                                      ;; value of a cell: X,O,0,-
  parent                                     ;; parent element for union-rank
  groupSize                                  ;; group size for union-rank
  isEdge                                     ;; is the cell an edge
  isCorner                                   ;; is the cell a corner
  )

;;Matrix generating *********************************************************************************************************************************************************

(defun initMatrix()
  (progn
    (setq *board* (make-array (list *matrixDim* *matrixDim*)))
    (generateMatrix '0 *emptyField* *n* *invalidField* (- *n* 1))
    ))

(defun generateRow (rowIndex firstMember firstCount secondMember secondCount)
  (progn
    (dotimes (i firstCount)
      (let* ((field 
              (make-cell
               :value firstMember
               :parent '0
               :groupSize '1
               :isEdge '0
               :isCorner '0)))
        (setf (aref *board* rowIndex i) field)))
    (dotimes (j secondCount)
      (let* ((field2 
              (make-cell
               :value secondMember
               :parent '0
               :groupSize '1
               :isEdge '0
               :isCorner '0)))
        (setf (aref *board* rowIndex (+ firstCount j)) field2 ))))) 

(defun generateMatrix (rowIndex firstMember firstCount secondMember secondCount)
  (cond 
   ( (equalp rowIndex *matrixDim*) (return-from generateMatrix '()) )
   ( t(progn
        (generateRow rowIndex firstMember firstCount secondMember secondCount)
        (cond ((equalp rowIndex (- *n* 1))
                       (generateMatrix (+ 1 rowIndex) secondMember (+ secondCount 1) firstMember (- firstCount 1) ))
               (t(generateMatrix (+ 1 rowIndex) firstMember (+ 1 firstCount) secondMember (- secondCount 1))))))))


;;Dimension input and start of the game *****************************************************************************************************************************************


;;Dimension input and initial matrix state 
(defun setDimension ()
  (format t "~%Input the board dimension: ")
  (let ((input (read)))
    (cond 
     ( (or (not (numberp input)) (< input 1) (> input 10)) (format t "~%Invalid dimension input!") (setDimension) )
     ( t 
      (setq *n* input) 
      (setq *matrixDim* (- (* 2 *n*) 1))
      (initMatrix)
      ))))
(setDimension)

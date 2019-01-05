;; PREDEFINISANI PREDIKATI

(defparameter *T1-RULES* '())
(defparameter *T1-FACTS* '())

;;******************************** FJE ZA MASINU **************************************************
(defun !eq (a b)
  (equal a b))

(defun !ne (a b)
  (not (equal a b)))

(defun !isEdge (row col)
  (let* ((edge (isEdge row col)))
    (if (> edge '0) t '())))

(defun !isCorner (row col)
  (let* ((corner (isCorner row col)))
    (if (> corner '0) t '())))

(defun !playerCell (currentPlayer row col board)
  (let* ((matrixEl (aref board row col)))
    (if (equalp currentPlayer (cell-value matrixEl)) t '())))

(defun !opponentCell (currentPlayer row col board)
  (let* ((opponent '())
         (matrixEl (aref board row col))
         )
    (progn
      (if (equalp currentPlayer *human*)
          (setf opponent *computer*) (setf opponent *human*))
      (if (equalp opponent (cell-value matrixEl)) t '()))))

;;****************************************** POMOCNE FJE I HEURISTIKA **********************************************

(defun get-cell-state (i j player board)
  (cond ( (equalp player (cell-value (aref board i j))) (list (list 'move i j)) )
        ( t (list (list 'move-opponent i j)) )
        ))


(defun get-states (board index matrixDim states cp)
  (let* ((j (getColumnFromIndex index matrixDim)) 
         (i (getRowFromIndex index j matrixDim)))
    (cond (  (> i (- matrixDim 1)) states )
          ( (equalp (aref board i j) *invalidField*) (get-states board (+ 1 index) matrixDim states cp) )
          ( (equalp (cell-value (aref board i j)) *emptyField*) (get-states board (+ 1 index) matrixDim states cp) )
          ( t (get-states board (+ 1 index) matrixDim (append states (get-cell-state i j cp board)) cp))
          )
    )
  )

(defun generate-facts (board matrixDim lastMove)
  (let* ((matrixEl (aref board (move-row lastMove) (move-col lastMove)))
         (player (cell-value matrixEl))
         (element '()))
      (setf *T1-FACTS* (get-states board '0 matrixDim '() player))
    (setf element (list (list 'last-move (move-row lastMove) (move-col lastMove) player)))
  ;;  (setf *T1-FACTS* (append *T1-FACTS* element))
         ))

(defun set-rules ()
  (setf *T1-RULES*
    '(
    (IF (AND (move ?x ?y) (!isCorner ?x ?y)) THEN (corner-current 1))
    (IF (AND (move ?x ?y) (!isEdge ?x ?y)) THEN (edge-current 1))
    (IF (AND (move-opponent ?x ?y) (!isCorner ?x ?y)) THEN (corner-opponent 1))
    (IF (AND (move-opponent ?x ?y) (!isEdge ?x ?y)) THEN (edge-opponent 1))
    ))
  )

(defun heuristika-inference-engine (board matrixDim lastMove)
  (generate-facts board matrixDim lastMove)
  (set-rules)
  (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
  (let ((cornerNum (count-results '(corner-current ?x)))
        (edgeNum (count-results '(edge-current ?x)))
        (cornerNumOpp (count-results '(corner-opponent ?x)))
        (edgeNumOpp (count-results '(edge-opponent ?x))))
        (- (+ (* 3 cornerNum) (* 2 edgeNum)) (+ (* 3 cornerNumOpp ) (* 2 edgeNumOpp)))))
    ;;(return-from heuristika-inference-engine cornerNum)))

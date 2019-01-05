;;Matrix generating *********************************************************************************************************************************************************

(defun generateValidRow (rowIndex colIndex member count)
  (dotimes (i count)
    (let ((columnIndex (+ i colIndex)))
      (cond ((equalp member *invalidField*) 
             (setf (aref *board* rowIndex columnIndex) *invalidField*))
            (t 
             (let* ((field 
                     (make-cell
                      :value member
                      :parent (parentIndex rowIndex columnIndex *matrixDim*)
                      :groupSize '(1)
                      :isEdge (list (isEdge rowIndex columnIndex))
                      :isCorner (list (isCorner rowIndex columnIndex))
                      :ringDepth '0
                      :locality '(0))
                     ))
               (setf (aref *board* rowIndex columnIndex) field)))))))

(defun generateRow (rowIndex firstMember firstCount secondMember secondCount)
  (progn
    (generateValidRow rowIndex '0 firstMember firstCount)
    (generateValidRow rowIndex firstCount secondMember secondCount)))

(defun generateMatrix (rowIndex firstMember firstCount secondMember secondCount)
  (cond 
   ( (equalp rowIndex *matrixDim*) (return-from generateMatrix '()) )
   ( t(progn
        (generateRow rowIndex firstMember firstCount secondMember secondCount)
        (cond ((equalp rowIndex (- *n* 1))
                       (generateMatrix (+ 1 rowIndex) secondMember (+ secondCount 1) firstMember (- firstCount 1) ))
              (t(generateMatrix (+ 1 rowIndex) firstMember (+ 1 firstCount) secondMember (- secondCount 1))))))))

(defun initMatrix()
  (progn
    (setq *board* (make-array (list *matrixDim* *matrixDim*)))
    (generateMatrix '0 *emptyField* *n* *invalidField* (- *n* 1))
    ))

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
      (setq *maxNumMoves* (-  (expt *matrixDim* '2) (* *n* (- *n* '1))))
      (initMatrix)
      ))))

;; Play move **************************************************************************************************************************************************************

(defun setElement (el i j matrix)
  (let ((field (aref matrix i j)))
    (progn
      (setf (cell-value field) el)
      (setf (aref matrix i j) field))))
    
(defun validateMove(i j boardState)
  (cond
   ((not(and (numberp i) (numberp j))) '() )
   ((or (< i '0) (< j '0)) '())
   ((or (> i (- *matrixDim* '1)) (> j (- *matrixDim* '1))) '())
   (t(let ((field (aref boardState i j)))
       (cond ((equalp field *invalidField*) '())
             (t(equalp (cell-value field) *emptyField*)))))))

(defun playMove (player)
  (format t "~%Enter a character for the row and number for the column: ")
  (let* ((rowChar (read-char)) (i (- (char-code rowChar) 65)) (j (read)))
    (cond ((not(validateMove i j *board*)) (format t "~%Invalid input!") (playMove player))
          (t 
           (setElement player i j *board*)
           (setLocality *board* i j player)
           (setq *numMoves* (+ *numMoves* 1))
           (if (equalp *numMoves* *maxNumMoves*) (setq *gameState* '2)
             (uniteNeighbours i j *board* *matrixDim* player))
           ))
    ))

(defun playMoveComputer ()
  (let ((bestMove (negamaxRetMove '3 *board* '() '-2000 '2000 1 *numMoves*)))
    (format t "~a" bestMove)
    (setElement *computer* (move-row bestMove) (move-col bestMove) *board*)
    (setLocality *board* (move-row bestMove) (move-col bestMove) *computer*)
    (setq *numMoves* (+ *numMoves* 1))
      (if (equalp *numMoves* *maxNumMoves*) (setq *gameState* '2)
             (uniteNeighbours (move-row bestMove) (move-col bestMove) *board* *matrixDim* *computer*))
           ))

;; Choose player ******************************************************************************************************************

(defun choosePlayer()
  (format t "~%Enter h/c for the first player human/computer:")
  (let* ((player (read)))
    (cond ((not (or (equalp player 'h) (equalp player 'c))) (format t "~%Invalid input!") (choosePlayer))
          ((equalp player 'h) (setq *human* *firstPlayer*) (setq *computer* *secondPlayer*))
          (t(setq *human* *secondPlayer*) (setq *computer* *firstPlayer*))
          )))

(defun switchCurrentPlayer()
  (if (equalp *currentPlayer* *firstPlayer*) 
      (setq *currentPlayer* *secondPlayer*)
    (setq *currentPlayer* *firstPlayer*)))

;; Game ***************************************************************************************************************************

(defun isEndGame()
  (cond ((= *gameState* '2)
         (printBoard *board*)
         (format t "It's a tie! ~%")
         (return-from isEndGame t))
        ((and (= *gameState* '1))
         (printBoard *board*)
         (format t "Game over! Player ~a won! ~%" *currentPlayer*)
         (return-from isEndGame t))
        (t (return-from isEndGame '()))))


(defun playGameHumans()
  (progn
    (printBoard *board*)
    (playMove *currentPlayer*)
    (if (isEndGame) (return-from playGameHumans))
    (switchCurrentPlayer)
    (playGameHumans)))

(defun playGameComputer ()
  (progn
    (printBoard *board*)
    (cond ( (equal *currentPlayer* *computer*) (playMoveComputer) )
          ( (equal *currentPlayer* *human*) (playMove *human*) ))
    (if (isEndGame) (return-from playGameComputer))
    (switchCurrentPlayer)
    (playGameComputer)))
  


(defun havannah ()
  (progn 
    (setq *gameState* '0)
    (setq *numMoves* '0)
    (setq *currentPlayer* *firstPlayer*)
    (setDimension)
    (format t "~%Enter h/c to play with a human/computer:")
    (let* ((player (read)))
    (cond ((not (or (equalp player 'h) (equalp player 'c))) (format t "~%Invalid input!") (choosePlayer))
          ((equalp player 'c) (choosePlayer) (playGameComputer))
          (t (setq *human* *firstPlayer*) (setq *computer* *secondPlayer*) (playGameHumans))
          ))
    
    
    (format t "~%Do you want to restart? ('Y' for YES, anything else for NO)~%")
    (if (equalp (read) 'Y) (havannah)
      (format t "Exited!"))
      )
    )
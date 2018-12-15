
(defun playerToCurrent (player)
    (cond
        ((equalp player '1) *computer*)
        (t *human*)
    )
  )
(playerToCurrent '-1)

(defun checkWin (board lastMove numMoves maxPlayer)
    (cond 
        ( (null lastMove) '() )
        ( (equalp numMoves (- (* *ringSize* 2) 1)) (checkRing (move-row lastMove) (move-col lastMove) (playerToCurrent maxPlayer) *ringSize* board))
        (t 
            (let* ((rootEl (root (parentIndex (move-row lastMove) (move-col lastMove) *matrixDim*) *matrixDim* board)))
              (if (or (> (cell-isCorner rootEl) '1) (>= (aref *bitCount* (cell-isEdge rootEl)) '3)) t '() )
              ;;(printBoard board)
              ;;(format t "~a~%" lastMove)
            )
        )
    )
)


(defun evaluate (board lastMove numMoves)
    (return-from evaluate '0)
)


(defun alpha-beta (depth board lastMove alpha beta maxPlayer numMoves )
    (cond ( (equalp numMoves *maxNumMoves*) (make-move :row 0 :col 0 :score 0) )
          ( (checkWin board lastMove numMoves maxPlayer) (make-move 
                                                :row 0
                                                :col 0
                                                :score (* (- 0 maxPlayer) 1000)) )
          ( (zerop depth) (make-move
                           :row 0
                           :col 0
                           :score (evaluate board lastMove numMoves) ) )
          ( (equal maxPlayer '1) ;;max player je *computer*
            (let ((bestMove (make-move :row 0 :col 0 :score alpha))) 
                (dotimes (i *matrixDim*) 
                    (dotimes (j *matrixDim*)
                        (let ((newState (getNewState *computer* i j board))
                              (newMove '())) 
                           (cond ( (not (null newState))  
                                (progn 
                                    (setf newMove (make-move :row i :col j :score '()))
                                    (setf (move-score newMove) (move-score (alpha-beta (- depth 1) newState newMove alpha beta (- 0 maxPlayer) (+ numMoves 1))))
                                    (if (> (move-score newMove) (move-score bestMove)) (setf bestMove (copy-structure newMove)))
                                    (if (< alpha (move-score bestMove)) (setf alpha (move-score bestMove)))
                                ) )
                                ( (>= alpha beta) (return-from alpha-beta bestMove) ) 
                           ) 
                        )
                    )
                )
                (return-from alpha-beta bestMove)
            ) 
        )
        ( t 
            (let ((bestMove (make-move :row 0 :col 0 :score beta))) 
                (dotimes (i *matrixDim*) 
                    (dotimes (j *matrixDim*)
                        (let ((newState (getNewState *human* i j board))
                              (newMove '())) 
                           (cond ( (not (null newState))  
                                (progn 
                                    (setf newMove (make-move :row i :col j :score '()))
                                    (setf (move-score newMove) (move-score (alpha-beta (- depth 1) newState newMove alpha beta (- 0 maxPlayer) (+ numMoves 1))))
                                    (if (< (move-score newMove) (move-score bestMove)) (setf bestMove (copy-structure newMove)))
                                    (if (> beta (move-score bestMove)) (setf beta (move-score bestMove)))
                                ) )
                                ( (>= alpha beta) (return-from alpha-beta bestMove) ) 
                           ) 
                        )
                    )
                )
                (return-from alpha-beta bestMove)
            )   
        )
    ))
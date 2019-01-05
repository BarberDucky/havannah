
(defun playerToCurrent (player)
    (cond
        ((equalp player '1) *computer*)
        (t *human*)
    )
  )

(defun checkWin (board lastMove numMoves maxPlayer)
    (cond 
     ( (null lastMove) '() )
     ( (and (>= numMoves '11) (equalp (checkRing (move-row lastMove) (move-col lastMove) (playerToCurrent (- 0 maxPlayer)) *ringSize* board) t) t))
        (t 
            (let* ((rootEl (root (parentIndex (move-row lastMove) (move-col lastMove) *matrixDim*) *matrixDim* board)))
              (if (or (> (car (cell-isCorner rootEl)) '1) (>= (aref *bitCount* (car (cell-isEdge rootEl))) '3)) t '() )
              )
        )
    )
)

(defun checkWinRetMove (board lastMove numMoves maxPlayer)
    (cond 
     ( (null lastMove) '() )
     ( (and (>= numMoves '11) (equalp (checkRing (move-row lastMove) (move-col lastMove) (playerToCurrent (- 0 maxPlayer)) *ringSize* board) t) t))
     (t 
      (let* ((rootEl (rootComputer (parentIndex (move-row lastMove) (move-col lastMove) *matrixDim*) *matrixDim* board)))
        (if (or (> (car (cell-isCorner rootEl)) '1) (>= (aref *bitCount* (car (cell-isEdge rootEl))) '3)) t '() )
        )
        )
    )
)

;;****************************** MINMAX ****************************************
(defun alpha-beta (depth board lastMove alpha beta maxPlayer numMoves )
    (cond ( (equalp numMoves *maxNumMoves*) (make-move :row 0 :col 0 :score 0) )
          ( (checkWin board lastMove numMoves maxPlayer) (make-move 
                                                :row 0
                                                :col 0
                                                :score (* (- 0 maxPlayer) 1000)) )
          ( (zerop depth) (make-move
                           :row 0
                           :col 0
                           (- 0 (evaluate board lastMove numMoves (playerToCurrent (- 0 maxPlayer))))))
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

;;***************************** NEGAMAX ****************************************
(defun negamax (depth board lastMove alpha beta maxPlayer numMoves)
    (cond ( (equalp numMoves *maxNumMoves*) (make-move :row 0 :col 0 :score 0) )
          ( (checkWin board lastMove numMoves maxPlayer) (make-move 
                                                :row 0
                                                :col 0
                                                :score (+ '-1000 numMoves)))
          ( (zerop depth) (make-move 
                           :row 0
                           :col 0
                           :score (- 0 (evaluate board lastMove numMoves (playerToCurrent (- 0 maxPlayer))))))
          ( t 
            (let ((bestMove (make-move :row 0 :col 0 :score alpha)))
                (dotimes (i *matrixDim*)
                    (dotimes (j *matrixDim*)
                        (let ((newState (getNewState (playerToCurrent maxPlayer) i j board))
                              (newMove '()))
                            (cond ( (not (null newState))  
                                (progn 
                                    (setf newMove (make-move :row i :col j :score '()))
                                    (setf (move-score newMove) (- 0 (move-score (negamax (- depth 1) newState newMove (- 0 beta) (- 0 alpha) (- 0 maxPlayer) (+ numMoves 1)))))
                                    (if (>= (move-score newMove) beta) (return-from negamax newMove))
                                  (if (> (move-score newMove) (move-score bestMove)) 
                                      (progn
                                        (setf alpha (move-score newMove)) 
                                        (setf bestMove (copy-structure newMove))))
                                ) ) 
                           )
                        )
                    )
                )
                (return-from negamax bestMove)
            ) 
          )
    )
  )
;;***************************** NEGAMAX RET MOVE ****************************************
(defun negamaxRetMove (depth board lastMove alpha beta maxPlayer numMoves)
    (cond ( (equalp numMoves *maxNumMoves*) (make-move :row 0 :col 0 :score 0) )
          ( (checkWinRetMove board lastMove numMoves maxPlayer) 
           (make-move 
            :row 0
            :col 0
            :score (+ '-1000 numMoves)))
          ( (zerop depth) (make-move 
                           :row 0
                           :col 0
                           ;;:score (evaluate board lastMove numMoves (playerToCurrent (- 0 maxPlayer)))))
                           :score (heuristika-inference-engine board *matrixDim* lastMove numMoves)))
          ( t 
            (let* ((bestMove (make-move :row 0 :col 0 :score alpha)))
                (dotimes (i *matrixDim*)
                    (dotimes (j *matrixDim*)
                        (let* ((valid (validateMove i j board))
                              (newMove '())
                              (player (playerToCurrent maxPlayer)))
                            (cond ( (not (null valid))  
                                   (progn
                                     (setElement player i j board)
                                     (uniteNeighboursComputer i j board *matrixDim* player)
                                     (setf newMove (make-move :row i :col j :score '()))
                                     (setLocalityComputer board i j player)
                                     (setf (move-score newMove) (- 0 (move-score (negamaxRetMove (- depth 1) board newMove (- 0 beta) (- 0 alpha) (- 0 maxPlayer) (+ numMoves 1)))))
                                     (remove-move i j board *matrixDim*)
                                     (unsetLocalityComputer board i j)
                                     (if (>= (move-score newMove) beta) (return-from negamaxRetMove newMove))
                                       (if (> (move-score newMove) (move-score bestMove)) 
                                         (progn
                                           (setf alpha (move-score newMove)) 
                                           (setf bestMove (copy-structure newMove))))
                                ) ) 
                           )
                        )
                    )
                  )
              (return-from negamaxRetMove bestMove)
            ) 
          )
    )
  )

(defun evaluate (board lastMove numMoves)
  (edgeConnectivity board lastMove)
)

(defun edgeConnectivity (board lastMove)
  (let* ((rootEl (root (parentIndex (move-row lastMove) (move-col lastMove) *matrixDim*) *matrixDim* board))
         (score '0))
    (progn 
      (setq score (* *edgeConnectivity* (+ (aref *bitCount* (cell-isEdge rootEl)) (cell-isCorner rootEl))))
     ;; (if (> score '20) (progn 
                            ;;   (printBoard board)
                       ; ;;  (format t "~a~%" lastMove)
                       ;;   (format t "~a~%" score)
                       ;;   ))
      (return-from edgeConnectivity score))))

(untrace edgeConnectivity)
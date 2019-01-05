(defun evaluate (board lastMove numMoves player)
  (let* ((rootEl (rootComputer (parentIndex (move-row lastMove) (move-col lastMove) *matrixDim*) *matrixDim* board))
         (score '0))
    (progn
      (setf score (* *edgeConnectivity* (edgeConnectivity rootEl)))
      (setf score (+ score (groupSize rootEl)))
      (setf score (+ score (localityScore board lastMove player)))
      (setf score (- numMoves score))
      (return-from evaluate score)
      )))

(defun edgeConnectivity (rootEl)
  (let* ((score '0))
    (progn 
      (setq score (+ (aref *bitCount* (car (cell-isEdge rootEl))) (car (cell-isCorner rootEl))))
      (return-from edgeConnectivity score))))

(defun groupSize (rootEl)
  (return-from groupSize (car (cell-groupSize rootEl))))

(defun localityScore (board lastMove player)
  (let* ((matrixEl (aref board (move-row lastMove) (move-col lastMove)))
         (shifte '0))
    (if (equalp player *secondPlayer*) (setf shifte '-2))
    (return-from localityScore (ash (car (cell-locality matrixEl)) shifte))))
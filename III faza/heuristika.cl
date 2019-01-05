(defun evaluate (board lastMove numMoves player)
  (let* ((rootEl (rootComputer (parentIndex (move-row lastMove) (move-col lastMove) *matrixDim*) *matrixDim* board))
         (score '0))
    (progn
      (setf score (* *edgeConnectivity* (edgeConnectivity rootEl)))
      (setf score (+ score (groupSize rootEl)))
      (setf score (+ score (localityScore board lastMove player)))
      (if (or (>= score 26) (<= score '-26))
          (progn
            (format t "Locality score ~a~%" (localityScore board lastMove player))
            (format t "Last move ~a~%" lastMove)
            (format t "Group size ~a~%" (groupSize rootEl))
            (format t "Connectivity ~a~%" (edgeConnectivity rootEl))
            (format t "Score ~a~%" score)

            (printBoard board)))
      (return-from evaluate score)
      )))

(defun edgeConnectivity (rootEl)
  (let* ((score '0))
    (progn 
      (setq score (+ (aref *bitCount* (car (cell-isEdge rootEl))) (car (cell-isCorner rootEl))))
     ;; (if (> score '20) (progn 
                            ;;   (printBoard board)
                       ; ;;  (format t "~a~%" lastMove)
                       ;;   (format t "~a~%" score)
                       ;;   ))
      (return-from edgeConnectivity score))))

(defun groupSize (rootEl)
  (return-from groupSize (car (cell-groupSize rootEl))))

(defun localityScore (board lastMove player)
  (let* ((matrixEl (aref board (move-row lastMove) (move-col lastMove)))
         (shifte '0))
    (if (equalp player *secondPlayer*) (setf shifte '-2))
    (return-from localityScore (ash (car (cell-locality matrixEl)) shifte))))
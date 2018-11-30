;;Union-rank funcions****************************************************************************************************************************************************

;;FALI NAM PATH COMPRESSION
(defun root (index dim boardState)
  (let* ((columnIndex (getColumnFromIndex index dim)) 
         (rowIndex (getRowFromIndex index columnIndex dim))
         (element (aref boardState rowIndex columnIndex)))
    (if (equalp (cell-parent element) index) index
      (root (cell-parent element) dim boardState))))

(defun isInUnion(firstNodeIndex secondNodeIndex board dim)
  (equalp (root firstNodeIndex dim board) (root secondNodeIndex dim board)))

(defun union-rank (firstNodeIndex secondNodeIndex board dim)
  (let* ((rootFirstIndex (root firstNodeIndex dim board))
         (colFirstIndex (getColumnFromIndex rootFirstIndex dim)) (rowFirstIndex (getRowFromIndex rootFirstIndex colFirstIndex dim))
         (rootSecondIndex (root secondNodeIndex dim board))
         (colSecondIndex (getColumnFromIndex rootSecondIndex dim)) (rowSecondIndex (getRowFromIndex rootSecondIndex colSecondIndex dim))
         (elementFirst (aref board rowFirstIndex colFirstIndex))
         (elementSecond (aref board rowSecondIndex colSecondIndex))
         (newGroupSize (+ (cell-groupSize elementSecond) (cell-groupSize elementFirst)))
         (newCorner (+ (cell-isCorner elementSecond) (cell-isCorner elementFirst)))
         (newEdge (logior (cell-isEdge elementSecond) (cell-isEdge elementFirst)))
         )
    (cond ((equalp rootFirstIndex rootSecondIndex) (return-from union-rank))
          ((< (cell-groupSize elementFirst) (cell-groupSize elementSecond))
           (progn
             (setf (cell-parent elementFirst) rootSecondIndex)
             (setf (cell-groupSize elementSecond) newGroupSize)
             (setf (cell-isCorner elementSecond) newCorner)
             (setf (cell-isEdge elementSecond) newEdge)
             (return-from union-rank elementSecond)
             ))
          (t(progn
              (setf (cell-parent elementSecond) rootFirstIndex)
              (setf (cell-groupSize elementFirst) newGroupSize)
              (setf (cell-isCorner elementFirst) newCorner)
              (setf (cell-isEdge elementFirst) newEdge)
              (return-from union-rank elementFirst)
              )))))

(defun uniteNeighboursWithList (neighbourList rowIndex colIndex board dim)
  (cond
   ((null neighbourList) (return-from uniteNeighboursWithList))
   (t(let* ((rootElement (union-rank (parentIndex rowIndex colIndex dim) (car neighbourList) board dim)))
       (if (and (not (null rootElement) ) (or (> (cell-isCorner rootElement) '1) (>= (aref *bitCount* (cell-isEdge rootElement)) '3)))
          (progn
            (setq *gameState* '1)
            (return-from uniteNeighboursWithList)
            )))
      (uniteNeighboursWithList (cdr neighbourList) rowIndex colIndex board dim))))

(defun uniteNeighbours (row col board dim currentPlayer)
  (uniteNeighboursWithList (findNeighbours board row col currentPlayer) row col board dim))

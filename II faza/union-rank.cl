;;Union-rank funcions****************************************************************************************************************************************************

;;FALI NAM PATH COMPRESSION
(defun root (index dim boardState)
  (let* ((columnIndex (getColumnFromIndex index dim)) 
         (rowIndex (getRowFromIndex index columnIndex dim))
         (element (aref boardState rowIndex columnIndex))
         (newIndex '()))
    (if (equalp (cell-parent element) index) element
      (prog1
          (setf newIndex (root (cell-parent element) dim boardState))
        (setf (cell-parent element) (cell-parent newIndex))))))

(defun isInUnion(firstNodeIndex secondNodeIndex board dim)
  (equalp (root firstNodeIndex dim board) (root secondNodeIndex dim board)))

(defun union-rank (firstNodeIndex secondNodeIndex board dim)
  (let* (;;(rootFirstIndex (root firstNodeIndex dim board))
         ;;(colFirstIndex (getColumnFromIndex rootFirstIndex dim)) (rowFirstIndex (getRowFromIndex rootFirstIndex colFirstIndex dim))
         ;;(rootSecondIndex (root secondNodeIndex dim board))
         ;;(colSecondIndex (getColumnFromIndex rootSecondIndex dim)) (rowSecondIndex (getRowFromIndex rootSecondIndex colSecondIndex dim))
         (elementFirst (root firstNodeIndex dim board))
         (elementSecond (root secondNodeIndex dim board))
         (rootFirstIndex (cell-parent elementFirst))
         (rootSecondIndex (cell-parent elementSecond))
         (newGroupSize (+ (cell-groupSize elementSecond) (cell-groupSize elementFirst)))
         (newCorner (+ (cell-isCorner elementSecond) (cell-isCorner elementFirst)))
         (newEdge (logior (cell-isEdge elementSecond) (cell-isEdge elementFirst)))
         )
    (cond ((equalp rootFirstIndex rootSecondIndex) (return-from union-rank (cell-groupSize elementFirst)))
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

(defun uniteNeighboursWithList (neighbourList rowIndex colIndex board dim currentPlayer)
  (cond
   ((null neighbourList) (return-from uniteNeighboursWithList))
   (t(let* ((rootElement (union-rank (parentIndex rowIndex colIndex dim) (car neighbourList) board dim)))
       (cond ((not (numberp rootElement))
              (if (or (> (cell-isCorner rootElement) '1) (>= (aref *bitCount* (cell-isEdge rootElement)) '3))     
                  (progn
                    (setq *gameState* '1)
                    (return-from uniteNeighboursWithList))))
             (t (if (and (>= rootElement *ringSize*) (checkRing rowIndex colIndex currentPlayer *ringSize* board))
                    (progn
                      (setq *gameState* '1)
                      (return-from uniteNeighboursWithList))))))
      (uniteNeighboursWithList (cdr neighbourList) rowIndex colIndex board dim currentPlayer))))

(defun uniteNeighboursWithListComputer  (neighbourList rowIndex colIndex board dim currentPlayer)
  (cond
   ((null neighbourList) (return-from uniteNeighboursWithListComputer))
   (t(let* ((rootElement (union-rank (parentIndex rowIndex colIndex dim) (car neighbourList) board dim)))
            (uniteNeighboursWithListComputer (cdr neighbourList) rowIndex colIndex board dim currentPlayer)))))

(defun uniteNeighbours (row col board dim currentPlayer)
  (uniteNeighboursWithList (findNeighboursNew board row col currentPlayer) row col board dim currentPlayer))

(defun uniteNeighboursComputer (row col board dim currentPlayer)
  (uniteNeighboursWithListComputer (findNeighboursNew board row col currentPlayer) row col board dim currentPlayer))

;;***********************************Ring**************************************************************

(defun checkRing (row col currentPlayer ringSize board)
  (let* ((element (aref board row col)) (success '()) (neighIndex '()) (neigh '()))
    (progn
      (setf (cell-ringDepth element) '1)
      (dotimes (i '4)
        (progn
          (setf neighIndex (validateNeighbourIndex row col i currentPlayer board))
          (if (not (null neighIndex))
              (progn 
                (setf neigh (aref board (car neighIndex) (cadr neighIndex)))
                (setf (cell-ringDepth neigh) '2)
                (setf success (followRing (car neighIndex) (cadr neighIndex) i currentPlayer '3 ringSize board))
                (setf (cell-ringDepth neigh) '0)
                (if (not (null success))
                    (progn
                      (setf (cell-ringDepth element) '0)
                      (return-from checkRing success)))))))
      (setf (cell-ringDepth element) '0)
      (return-from checkRing success))))

(defun followRing (row col dir currentPlayer depth ringSize board)
  (let* ((success '()) (nbIndex '()) (nbNextIndex '()) (neigh '()))
    (progn
      (dolist (i '(5 6 7))
        (progn
          (setf nbIndex (mod (+ dir i) '6))
          (setf nbNextIndex (validateNeighbourIndex row col nbIndex currentPlayer board))
          (if (not (null nbNextIndex))
              (progn
                (setf neigh (aref board (car nbNextIndex) (cadr nbNextIndex)))
                (if (> (cell-ringDepth neigh) '0)
                    (return-from followRing (>= (- depth (cell-ringDepth neigh)) ringSize)))
                (setf (cell-ringDepth neigh) depth)
                (setf success (followRing (car nbNextIndex) (cadr nbNextIndex) nbIndex currentPlayer (+ depth 1) ringSize board))
                (setf (cell-ringDepth neigh) '0)
                (if (not (null success))
                    (return-from followRing t))))))
      (return-from followRing '()))))
        
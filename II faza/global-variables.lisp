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

(defvar *currentPlayer*)
(setq *currentPlayer* *firstPlayer*)

;;human and computer
(defvar *human*)
(setq *human* '())
(defvar *computer*)
(setq *computer* '())

(defvar *gameState*)
(setq *gameState* '0) ;;0 - in progress, 1 - over, 2 - nereseno

(defvar *ringSize* '6)

(defvar *bitCount*)
(setf *bitCount* (make-array '(64)
  :initial-contents '(0 1 1 2 1 2 2 3
	1 2 2 3 2 3 3 4
	1 2 2 3 2 3 3 4
	2 3 3 4 3 4 4 5
	1 2 2 3 2 3 3 4
	2 3 3 4 3 4 4 5
	2 3 3 4 3 4 4 5
                      3 4 4 5 4 5 5 6)))

(defvar *neighbours*)
(setf *neighbours* (make-array '(6)
                               :initial-contents
                               '((-1 -1) 
                                 (0 -1)
                                 (1 0)
                                 (1 1)
                                 (0 1)
                                 (-1 0))))

(defstruct cell
  value                                      ;; value of a cell: X,O,0,-
  parent                                     ;; parent element for union-rank
  groupSize                                  ;; group size for union-rank
  isEdge                                     ;; is the cell an edge
  isCorner                                   ;; is the cell a corner
  ringDepth                                  ;; depth for check ring
  )
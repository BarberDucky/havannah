;;variable and constant definitions ****************************************************************************************************************************************************

(defconstant *emptyField* '-)
(defconstant *invalidField* '0)
(defvar *n* )                                ;; n is the board dimension
(defvar *matrixDim*)                         ;; matrix dimension
(defvar *board*)                             ;; stores the board matrix

(defconstant *firstPlayer* 'X)
(defconstant *secondPlayer* 'O)

;;human and computer
(defvar *human*)
(defvar *computer*)

;;Matrix generating *********************************************************************************************************************************************************


;;Generates a list with same elements
(defun generateMembers (element count)
  (cond ( (zerop count) '() )
        ( t (cons element (generateMembers element (- count 1))) )
        ))

;;Generates one row of the matrix
(defun generateRow (firstMember firstCount secondMember secondCount)
  (append (generateMembers firstMember firstCount) (generateMembers secondMember secondCount)))

;;Generates the whole matrix
(defun generateMatrix (firstCount secondCount lowerPart )
  (cond 
   ( (and lowerPart (= firstCount *n*)) '())
   ( (= *matrixDim* (- firstCount 1)) (generateMatrix (+ secondCount 2) (- firstCount 2) t ))
   ( (null lowerPart)  (cons (generateRow  *emptyField* firstCount *invalidField* secondCount) 
                      (generateMatrix (+ 1 firstCount) (- secondCount 1 ) lowerPart)))
    (t(cons (generateRow *invalidField*  firstCount *emptyField*  secondCount) 
                      (generateMatrix (+ 1 firstCount) (- secondCount 1 ) lowerPart)))
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
      (setq *board* (generateMatrix *n* (- *n* 1) '()))
      ))))

;;Board display ***************************************************************************************************************************************************************


;;Print newline and first row with numbers
(defun printFirstRow (count)                                                                           
  (cond
   ((equalp count -1) (format t "~%") (printFirstRow 0))                                                                ;print newline for the entire matrix
   ((equalp count (+ '2 *matrixDim*)) '())                                                                              ;end                                                
   ((< count (+ *n* '1)) (format t " ") (printFirstRow (+ count '1)))                                                   ;print space - n+1 times                                  
   (t (format t "~a " (- count *n* 1)) (printFirstRow (+ count '1)))))                                                  ;print numbers - n times

;;Prints newline and all rows except the first one 
(defun printRow (rowList count rowIndex)
  (cond 
   ((= count '-1) (format t "~%~a " (code-char (+ 65 rowIndex))) (printRow rowList '0 rowIndex))                         ;first el = newline + rowChar
   ((= count *matrixDim*) (if (< rowIndex (- *n* 1)) (format t "~a" (+ rowIndex *n*)) '()))                              ;last el number is printed only for the upper half
   ((equalp (car rowList) '0) (format t " ") (printRow (cdr rowList) (+ count 1) rowIndex))                              ;zeros are printed as blanks
   (t (format t "~a " (car rowList)) (printRow (cdr rowList) (+ count 1) rowIndex ))))                                   ;elements are printed out as element + blank

;;Print entire board 
(defun printBoardRecursive (board row)
  (cond 
   ((null board) (format t "~%"))                                                                                        ;end
   ((= row -1) (printFirstRow -1) (printBoardRecursive board 0))                                                         ;newline + first row
   ((>= row *n*) (printRow (car board) '-1 row) (printBoardRecursive (cdr board) (+ row 1)))                             ;directly applied for bottom half        
   (t(printRow(append (member '0 (car board)) (car board)) '-1 row) (printBoardRecursive (cdr board) (+ row 1)))))       ;zeros are put in the front for the upper half

;;Caller function for printBoardRecursive
(defun printBoard ()
  (cond
   ((or (null *n*) (null *matrixDim*) (null *board*)) (format t "Please set dimensions before attempting to print."))
   (t (printBoardRecursive *board* -1))
   ))

;; Play move **************************************************************************************************************************************************************


(defun setElement (el i j matrix)
  (cond 
   ((null matrix) '())
        ((> i '0) (cons (car matrix) (setElement el (- i 1) j (cdr matrix))))
        ((equalp i '0) (cons (setElement el (- i 1) j (car matrix)) (cdr matrix)))
        ((equalp j '0) (cons el (setElement el i (- j 1) (cdr matrix))))
        (t(cons (car matrix) (setElement el (- i 1) (- j 1) (cdr matrix))))))

(defun playMove (player)
  (format t "~%Enter a character for the row and number for the column: ")
  (let* ((rowChar (read-char)) (i (- (char-code rowChar) 65)) (j (read)))
    (cond ((not (equalp (nth j (nth i *board*)) '-)) (format t "~%Invalid input!") (playMove player))
          (t (setq *board* (setElement player i j *board*))))
    ))

(defun choosePlayer()
  (format t "~%Enter h/c for the first player human/computer:")
  (let* ((player (read)))
    (cond ((not (or (equalp player 'h) (equalp player 'c))) (format t "~%Invalid input!") (choosePlayer))
          ((equalp player 'h) (setq *human* *firstPlayer*) (setq *computer* *secondPlayer*))
          (t(setq *human* *secondPlayer*) (setq *computer* *firstPlayer*))
          )))
          
;;Function calls  - Test functions here ***************************************************************************************************************************************

(setDimension)
(choosePlayer)

;;Because of use of global variables, it is necessary to set the dimension before calling these functions
(playMove *computer*)
(playMove *human*)
(printBoard)








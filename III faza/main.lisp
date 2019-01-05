;;Load Milica
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\global-variables.lisp")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\helpers.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\union-rank.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\game.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\print-board.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\operatori-stanja.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\alpha-beta.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\heuristika.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\masina-zakljucivanja.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\III faza\\inference-engine.cl")

;;Load Ana
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\global-variables.lisp")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\helpers.cl")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\union-rank.cl")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\game.cl")
;;(load "D:\\Vestacka inteligencija\\Havannah\\havannah\\II faza\\print-board.cl")

;;Load Damjan
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\global-variables.lisp")
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\helpers.cl")
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\union-rank.cl")
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\game.cl")
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\print-board.cl")
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\alpha-beta.cl")
(load "C:\\Users\\DamjanTrifunovic\\Desktop\\havannah\\II faza\\operatori-stanja.cl")


(havannah)
(printBoard *board*)

(setq *matrixDim* '3)
(initMatrix)
(format t "~a" *board*)
(setElement 'X '0 '0 *board*)
(setElement 'X '1 '0 *board*)
(setElement 'O '0 '1 *board*)
(uniteNeighbours '0 '0 *board* *matrixDim* 'X)

(setElement 'O '1 '3 *board*)
(uniteNeighboursComputer '1 '3 *board* *matrixDim* 'O)
(setElement 'O '0 '2 *board*)
(uniteNeighboursComputer '0 '2 *board* *matrixDim* 'O)

(negamaxRetMove '3 *board* '() '-2000 '2000 '1 *numMoves*)

(find-parents t '2 *matrixDim* *board*)

(remove-move '0 '2 *board* *matrixDim*)
(remove-move '1 '3 *board* *matrixDim*)

(defun test (depth player ind)
  (cond ((equalp depth '0) (return-from test))
        (t
  (dotimes (i *matrixDim*)
    (dotimes (j *matrixDim*)
      (let ((valid (validateMove i j *board*)))
        (cond ( (not (null valid))  
               (progn
                 (setElement player i j *board*)
                 (uniteNeighboursComputer i j *board* *matrixDim* player)
                 (let ((newPlayer '()))
                   (if (equalp player 'X) (setf newPlayer 'O) (setf newPlayer 'X))
                   ;;(if (equalp (mod ind 9) 0) (printBoard *board*))
                   ;;(printBoard *board*)
                   (test (- depth 1) newPlayer (+ ind 1))
                   (remove-move i j *board* *matrixDim*)))))))))))
(test '3 'O '0)
(untrace test)
(random 900)

(setElement 'O '0 '0 *board*)
(uniteNeighboursComputer '0 '0 *board* *matrixDim* 'O)
(setElement 'O '0 '1 *board*)
(uniteNeighboursComputer '0 '1 *board* *matrixDim* 'O)
(setElement 'O '1 '0 *board*)
(uniteNeighboursComputer '1 '0 *board* *matrixDim* 'O)

(setElement '0 '8 '7 *board*)

(defun findNeighboursTest (board row col currentPlayer)
  (let* ((neighbourList '()) (nextNb '()))
    (dotimes (i '18)
      (setf nextNb (validateNeighbourIndex row col i currentPlayer board))
      (format t "~a~%" nextNb)
      (if (not (null nextNb))
         (cond ((equalp (caddr nextNb) '3)
                 (setElement '3 (car nextNb) (cadr nextNb) *board*))
                ((equalp (caddr nextNb) '2)
                 (setElement '2 (car nextNb) (cadr nextNb) *board*))
                (t
                 (setElement '1 (car nextNb) (cadr nextNb) *board*))))
        (setf neighbourList (append neighbourList (list (parentIndex (car nextNb) (cadr nextNb) *matrixDim*)))))
      )
    (return-from findNeighboursTest neighbourList)))

(findNeighboursTest *board* '8 '7 '1)
(format t "~a" (findNeighboursSecond *board* '8 '7))
(caddr '(1 2 3))
(equalp (caddr '(1 2 3)) '3)

(setElement 'X '8 '7 *board*)
(setLocalityComputer *board* '8 '7 'X)

(setElement 'X '8 '8 *board*)
(setLocalityComputer *board* '8 '8 'X)

(unsetLocalityComputer *board* '8 '8)
(unsetLocalityComputer *board* '8 '7)

(format t "~a" *board*)

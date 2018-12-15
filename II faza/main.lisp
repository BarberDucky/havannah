;;Load Milica
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\global-variables.lisp")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\helpers.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\union-rank.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\game.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\print-board.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\operatori-stanja.cl")
(load "D:\\Fax\\VII semestar\\Vestacka inteligencija\\Projekat\\havannah\\havannah\\II faza\\alpha-beta.cl")
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


;;********************TEST***************************************
(defvar list)
(setq list (getPossibleStates *board* *currentPlayer* *matrixDim*))
(defun printAll(lista)
  (cond ((null lista) (return-from printAll))
        (t(printBoard (car lista)) (printAll(cdr lista)))))
(trace printAll)
(untrace printAll)
(printAll list)
;;****************************************************************** 

(format t "~a" (validateNeighbour '0 '0 '4 *emptyField* *board*))
(format t "~a" *board*)
(printBoard *board*)

(setDimension)
(choosePlayer)
(setElement 'X 0 0 *board*)
(format t "~%~a" (alpha-beta '2 *board* '() '-2000 '2000 1 *numMoves*))

(setElement 'X 1 1 *board*)
(union-rank 0 6 *board* *matrixDim*)
(setElement 'X 2 2 *board* )
(union-rank 6 12 *board* *matrixDim*)
(havannah)

(setDimension)
(choosePlayer)
(defun gameComputer (ind)
    (progn
    (printBoard *board*)
      (playMove *human*)
      (let ((computerMove (alpha-beta '3 *board* '() '-2000 '2000 1 *numMoves* )))
        (format t "~a" computerMove)
        (setElement *computer* (move-row computerMove) (move-col computerMove) *board*))
      (printBoard *board*)
      (if (equalp ind '3) (return-from gameComputer) (gameComputer (+ ind 1)))
    )
  )
(gameComputer '0)